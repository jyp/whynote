{-# LANGUAGE RecordWildCards, FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses #-}
module App where
import Control.Lens hiding (transform)
import Control.Monad
import Control.Monad.IO.Class
import Event
import GtkProcess
import NoteData
import Render
import WNPrelude
import qualified Data.Map.Strict as M
import Prelude ()
import qualified Process
import qualified Data.Vector as V
import Config (configuredPens)

mkStroke :: [Sample] -> GtkP Stroke
mkStroke cs = do
  opts <- use stPen
  return $ Stroke opts (box $ Curve $ V.fromList cs)

strokeLoop :: [Sample] -> GtkP [Sample]
strokeLoop c = do
  tr <- use stDilation
  old <- (apply tr <$>) <$> mkStroke (reverse c) -- FIXME: slow!
  setGui [Strk old]
  ev <- wait "next stroke point"
  let cs' = (eventSample ev:c)
  case eventSource ev of
    Eraser -> return c
    Stylus -> case ev of
      Event {..} | eventModifiers == 0 || eventType == Event.Release || sampleZ (eventSample) == 0  -> return cs'
      _ -> strokeLoop cs'
    _ -> strokeLoop c -- ignore events from another source

-- | Remove 0-pressure subsequences in a stroke.
cleanStroke :: [Sample] -> [Sample]
cleanStroke (a:b:c) | sampleZ a == 0, sampleZ b == 0 = cleanStroke (b:c)
cleanStroke c = c

stroke :: Sample -> GtkP ()
stroke c0 = do
  strk <- mkStroke =<< (cleanStroke . reverse) <$> strokeLoop [c0]
  stNoteData %= (strk:)
  setGui []
 
lassoProcessLoop :: Source -> [Coord] -> GtkP ClosedCurve
lassoProcessLoop source c = do
  tr <- use stDilation
  let res = Closed $ V.fromList c
  setGui [Lass (apply tr <$> res)]
  ev <- wait "next lasso point"
  case eventSource ev == source of
    False -> lassoProcessLoop source c -- ignore events from another source
    True -> case ev of
      Event {..} | eventType == Event.Release || eventModifiers == 0 || sampleZ (eventSample) == 0 -> return res
      _ -> lassoProcessLoop source (eventCoord ev:c)

deselect :: GtkP ()
deselect = do
  (Selection _ oldSel) <- use stSelection
  stNoteData %= (oldSel ++)
  setSelection emptySelection

lassoProcess :: Source -> GtkP ()
lassoProcess source = do
  deselect
  bounds <- lassoProcessLoop source []
  setGui []
  (inside,outside) <- lassoPartitionStrokes (box bounds) <$> use stNoteData
  setSelection (mkSelection inside)
  stNoteData .= outside

mkSelection :: [Stroke] -> Selection
mkSelection strks = Selection (extend 10 $ boundingBox strks) strks

addToSelection :: [Stroke] -> GtkP ()
addToSelection strks = do
  Selection _ s0 <- use stSelection
  setSelection (mkSelection (s0 ++ strks))

selectNear :: Coord -> GtkP ()
selectNear p = do
  f <- fuzzyFactor
  (selected,kept) <- partitionStrokesNear f p <$> use stNoteData
  addToSelection selected
  stNoteData .= kept

fuzzyFactor :: GtkP Double
fuzzyFactor = use (stDilation.trZoom.to (10 /))

setSelection :: Selection -> GtkP ()
setSelection sel = do
  stSelection .= sel
  invalidateAll -- because the menu needs to be redrawn (and it annoying to figure out where it is)

deleteSelection :: GtkP ()
deleteSelection = do
  Selection _ sel <- use stSelection
  setSelection emptySelection
  stRedo %= (sel++)

deselectNear :: Coord -> GtkP ()
deselectNear p = do
  f <- fuzzyFactor
  Selection _ strokes <- use stSelection
  let (deselected,kept) = partitionStrokesNear f p strokes
  setSelection (mkSelection kept)
  stNoteData %= (++ deselected)

eraseNear :: Coord -> GtkP ()
eraseNear p = do
  tr <- use stDilation
  (erased,kept) <- partitionStrokesNear 10 p <$> use stNoteData
  invalidate (apply tr <$> boundingBox erased)
  stRedo %= (erased++)
  stNoteData .= kept

neighbourhoodProcessLoop :: Source -> (Coord -> GtkP ()) -> GtkP ()
neighbourhoodProcessLoop source action = do
  ev <- wait "next neighbourhood point"
  case source `elem` [Stylus, Eraser] of
    False -> neighbourhoodProcessLoop source action -- ignore non-pen events
    True -> case eventSource ev == source of
      False -> Process.recycle ev -- quick switch between pen/eraser
      True -> case ev of
        Event {..} | eventType == Event.Release
                   || eventModifiers == 0
                   || sampleZ eventSample == 0 -> return ()
        _ -> do action (eventCoord ev)
                neighbourhoodProcessLoop source action

eraseProcess :: Source -> GtkP ()
eraseProcess source = do
  deselect
  neighbourhoodProcessLoop source eraseNear

selectProcess :: Source -> GtkP ()
selectProcess source = neighbourhoodProcessLoop source selectNear

deSelectProcess :: Source -> GtkP ()
deSelectProcess source = neighbourhoodProcessLoop source deselectNear

fingerBegin coord = Finger {fingerStart = coord, fingerCurrent = coord}
fingerAdd coord Nothing = Just (fingerBegin coord)
fingerAdd coord (Just Finger{..}) = Just Finger {fingerCurrent=coord,..}

avg x y = average [x,y]
average :: [Coord] -> Coord
average xs = ((1::Double)/fromIntegral (length xs)) *^ (foldr (+) zero xs)

norm2 (Coord x1 y1) = x1*x1 + y1*y1
dist a b = sqrt (norm2 (a-b))
peri xs = sum $ zipWith dist xs (rot xs)

touchProcessEntry :: Event -> GtkP ()
touchProcessEntry ev = do
  wakeup ev 70
  touchProcessDetect (Just (eventTime ev)) (M.singleton (eventButton ev) (fingerBegin (eventCoord ev)))

touchProcessDetect :: Maybe Word32 -> M.Map Int Finger -> GtkP ()
touchProcessDetect time0 touches
  | M.null touches = return ()
  | otherwise = do
  ev <- waitInTrans zero ("multi-touch startup " ++ show (M.keys touches))
  wakeup ev 70
  let rb = Process.recycle ev
  case time0 of
    Just t0 | (eventTime ev > t0 + 50) && (M.size touches `elem` [1,2]) -> do
       -- fingers stable, run next phase.
       -- FIXME: two input touches must not start too close from each other!
       sel <- use stSelection
       tr <- use stDilation
       let msel = if any ((`inArea` sel) . apply (negate tr) . fingerStart) touches
                  then Just sel
                  else Nothing
       setGui (map Fing (M.elems touches))
       rb
       touchProcess msel tr touches
    _ -> case eventSource ev of
       MultiTouch -> case () of
         _ | eventType ev == Cancel -> return ()
         _ | eventType ev == End -> touchProcessDetect Nothing (M.delete (eventButton ev) touches)
             -- borked gesture; wait for cancelling.
         _ | eventType ev `elem` [Begin,Update]
             -> do let touches' = M.alter (fingerAdd (eventCoord ev)) (eventButton ev) touches
                   touchProcessDetect (Just (eventTime ev)) touches'
       Stylus -> rb
       Eraser -> rb
       Timeout -> touchProcessDetect time0 touches
       _ -> do liftIO $ putStrLn $ "WARNING: unexpected event: " ++ show ev
               rb

-- We pass the original selection here, so that we do not repeatedly modify the
-- selection with small translations, which would eventually accumulate
-- numerical errors.
touchProcess :: Maybe Selection -> Dilation -> M.Map Int Finger -> GtkP ()
touchProcess selection origTrans touches
  | M.null touches = exit
  | otherwise = do
  ev <- waitInTrans zero "multi-touch"
  let rb = do Process.recycle ev
              stDilation .= origTrans -- fixme: check if this is necessary
              exit
  case eventSource ev of
    MultiTouch -> case () of
      _ | not (eventButton ev `M.member` touches) -> exit
      _ | eventType ev `elem` [Begin,Cancel,End] -> exit
      _ | eventType ev `elem` [Update] -> do
                let touches' = M.alter (fingerAdd (eventCoord ev)) (eventButton ev) touches
                    pss = (apply (negate origTrans) <$>) <$> M.elems touches'
                    points = (<$> pss) <$> [fingerStart,fingerCurrent]
                    [a0,a1] = map average points
                    [s0,s1] = map peri points
                    factor = case length pss of 1 -> 1
                                                2 -> s1 / (s0+1) -- FIXME: +1 is not nice here
                    dil = Dilation factor (a1 - factor *^ a0)

                setGui (Fing <$> M.elems touches')
                case selection of
                  Just sel -> transSel sel dil
                  Nothing -> do stDilation .= origTrans + dil
                                invalidateAll
                cont touches'
      _ -> cont touches
    Stylus -> rb
    Eraser -> rb
    Timeout -> cont touches
    _ -> do liftIO $ putStrLn $ "event from" ++ show (eventSource ev)
            cont touches
 where
   exit = setGui []
   cont = touchProcess selection origTrans

transSel :: Selection -> Dilation -> GtkP ()
transSel origSel dil = setSelection (apply dil <$> origSel)

moveSelWithPen :: Selection -> Coord -> GtkP ()
moveSelWithPen origSel origCoord = do
  ev <- wait "move sel pen"
  case eventSource ev of
    Stylus -> case ev of
      Event {eventType  = Event.Release} -> return ()
      Event {eventModifiers = 0} -> return () -- motion without any pressed key
      _ -> do
        transSel origSel (Dilation 1 (eventCoord ev - origCoord))
        moveSelWithPen origSel origCoord
    _ -> moveSelWithPen origSel origCoord -- ignore events from another source

waitForRelease source = do
  ev <- wait "wait pen release"
  if eventSource ev /= source then waitForRelease source else do
    case ev of
      Event {eventType  = Event.Release} -> return ()
      Event {eventModifiers = 0} -> return () -- motion without any pressed key
      _ -> waitForRelease source


shiftUndos n (us,d:ds) | n > 0 = shiftUndos (n-1) (d:us,ds)
shiftUndos n (u:us,ds) | n < 0 = shiftUndos (n+1) (us,u:ds)
shiftUndos n x = x

undoProcess c0 st = do
  ev <- waitInTrans zero "undo"
  let (newRedo,kept) = shiftUndos dy st
      dy = dist c0 (eventCoord ev) / 10
  stNoteData .= kept
  stRedo .= newRedo
  invalidateAll
  case ev of
      Event {eventType = Press} -> waitForRelease (eventSource ev)
      _ -> undoProcess c0 st

menu :: Double -> [(String,Coord -> GtkP ())] -> Coord -> GtkP ()
menu a0 options c = menu' a0 c c options

menu' :: Double -> Coord -> Coord -> [(String, Coord -> GtkP ())] -> GtkP ()
menu' a0 p c options = do
  let hideMenu = setGui []
  if dist p c > menuOuterCircle
     then hideMenu
     else do
       let rMenu p' = Menu a0 p' c $ map fst options
       setGui [rMenu p]
       ev@Event {..} <- waitInTrans zero "menu"
       active <- testMenu $ rMenu (eventCoord ev)
       if eventType == Press || eventType == Begin
       then do
           hideMenu
           case active of
              Just i -> snd (options !! i) (eventCoord ev)
              Nothing -> Process.recycle Event{..}
       else menu' a0 (eventCoord ev) c options

penMenu :: [(String, Coord -> GtkP ())]
penMenu = [(name, \_ -> stPen .= pen) | (name,pen) <- configuredPens]

-- 1: shift
-- 256 mouse 1 or touch
-- 512 mouse 2 (mid)
-- 1024 mouse 3 (right)
whynote :: GtkP ()
whynote = do
 ctx <- view (to id)
 rmc <- evalUiElementCoord ctx rootMenuCenter
 mapM_ (Process.forkHandler . softButtonHandler) =<< mapM (mkUiElement ctx) softButtons
 Process.forkHandler (palmRejection (-1) (-1))
 loop $ do
  ev <- wait "top-level"
  st <- use (to id)
  sel <- use stSelection
  tr <- use stDilation
  let pressure = eventPressure ev
      havePressure = pressure > 0
      haveSel = not . isEmptySelection $ sel
      inSel = haveSel && (eventCoord ev `inArea` sel)
      evOrig = apply tr (eventCoord ev)
  -- liftIO $ print ev
  case ev of
    Event {..} | eventSource == Stylus || (eventSource == MultiTouch && eventType == Begin),
                 haveSel, dist evOrig (selMenuCenter st) < menuRootRadius ->
      menu (pi/4) [("Delete",\_ -> deleteSelection)] (selMenuCenter st)
    Event {..} | eventSource == Stylus || (eventSource == MultiTouch && eventType == Begin),
                 dist evOrig rmc < menuRootRadius -> do
      menu (pi/4) ([("Pen",menu 0 penMenu)
                   ,("Undo",\c -> do
                        dones <- use stNoteData
                        redos <- use stRedo
                        undoProcess c (redos,dones))
                   ,("Quit",menu 0 [("Confirm",\_ -> quit)])]) rmc
    Event {eventSource = Stylus,..} | (eventType == Press && eventButton == 1), _stButtons st == [] -> do
      if haveSel
        then if inSel
                then moveSelWithPen sel (eventCoord ev)
                else do
                  deselect
                  waitForRelease Stylus
        else stroke eventSample
    Event {..} | (eventSource == Eraser && (havePressure || eventType == Press))
              || (eventSource == Stylus && (Erase `elem` _stButtons st) && (eventButton == 1 && eventType == Press)) -> do
      if haveSel
        then do if inSel
                  then deleteSelection
                  else deselect
                waitForRelease eventSource
        else do
          eraseNear (eventCoord ev)
          eraseProcess eventSource
    Event {eventSource = Stylus,..} | (eventModifiers==512) && havePressure
      || ((Lasso `elem` _stButtons st) && (eventButton == 1 && eventType == Press)) -> do
      lassoProcess (eventSource ev)
    Event {eventSource = Stylus,..} | (eventModifiers==1024) && havePressure
      || ((Select `elem` _stButtons st) && (eventButton == 1 && eventType == Press)) -> do
      if inSel
        then deSelectProcess (eventSource ev)
        else selectProcess (eventSource ev)
    Event {eventSource = MultiTouch} -> do
      when (eventType ev == Begin) $ do
        touchProcessEntry (apply tr <$> ev)
    _ -> return ()

rootMenuCenter :: UiElementCoord
rootMenuCenter = UiElementCoord 0 0 (Coord 40 40)

selMenuCenter :: St -> Coord
selMenuCenter St{..} = apply _stDilation (intervalHi (selectionBox _stSelection))

renderAll ctx st@St{_stDilation = tr,..} = do
   let whenSel = when (not $ isEmptySelection $ _stSelection)
   renderNoteData ((apply tr <$>) <$> _stNoteData)
   whenSel $ renderSelection (apply tr <$> _stSelection)
   forM_ _stGui renderGui
   renderSoftButtons ctx st
   renderMenuRoot "menu" =<< evalUiElementCoord ctx rootMenuCenter
   whenSel $ renderMenuRoot "sel" (selMenuCenter st)

----------------------
-- Soft buttons

data Button' a = Button SoftButton String Double a deriving (Functor,Foldable,Traversable)
type Button = Button' Coord

softButtons :: [Button' UiElementCoord]
softButtons = [Button Erase  "erase"  50 (botRight (Coord (-40) ( -50)))
              ,Button Lasso  "lasso"  50 (botRight (Coord (-40) (-160)))
              ,Button Select "select" 50 (botRight (Coord (-40) (-270)))
              ] where botRight = UiElementCoord 1 1

renderSoftButtons ctx St {..} = do
  btns <- mapM (mkUiElement ctx) softButtons
  forM_ btns $ \(Button btn txt rad c) ->
    renderNode (btn `elem` _stButtons) txt rad c

mkUiElement :: (MonadIO f, Traversable t) => Ctx -> t UiElementCoord -> f (t Coord)
mkUiElement ctx = traverse (evalUiElementCoord ctx)

touchWaitForRelease :: SoftButton -> Int -> GtkP ()
touchWaitForRelease btn touchNumber = do
  ev <- waitOn "btn release" (\Event{..} -> eventSource == MultiTouch &&
                                            eventButton == touchNumber)
  case eventType ev of
    End -> do stButtons %= filter (/= btn)
              invalidateAll
    _ -> touchWaitForRelease btn touchNumber

softButtonHandler (Button softBtn txt rad c) = loop $ do
  ctx <- view (to id)
  ev@Event{..} <- waitOn ("btn: " ++ txt) (\Event{..} -> eventSource == MultiTouch &&
                                                         eventType == Begin)
  sz <- liftIO (getWinSize ctx)
  if (dist c (eventCoord ev) < rad)
    then do stButtons %= (softBtn:)
            invalidateAll
            Process.forkHandler (touchWaitForRelease softBtn eventButton)
    else do Process.reject Event{..} -- not for us after all


---------------------------------------
-- Logical Coordinate for ui elements

data UiElementCoord = UiElementCoord {uiElBot :: Double,
                                      uiElRight :: Double,
                                      uiElK :: Coord} deriving Show
instance AbelianAdditive UiElementCoord where
instance Additive UiElementCoord where
  zero = UiElementCoord 0 0 zero
  UiElementCoord b1 r1 k1 + UiElementCoord b2 r2 k2 = UiElementCoord (b1+b2) (r1+r2) (k1+k2)
instance Group UiElementCoord where
  negate = ((negate (1::Double)) *^)
instance Module Double UiElementCoord where
  f *^ (UiElementCoord b r k) = UiElementCoord (f*^b) (f*^r) (f*^k)

evalUiElementCoord :: MonadIO m => Ctx -> UiElementCoord -> m Coord
evalUiElementCoord ctx (UiElementCoord b r k) = do
    (rr,bb) <- liftIO (getWinSize ctx)
    let res = k + b *^ (Coord 0 (fromIntegral bb)) + r *^ (Coord (fromIntegral rr) 0)
    return res

mirrorUiElementCoord (UiElementCoord b r (Coord kx ky)) = UiElementCoord b (1-r) (Coord (negate kx) ky)
