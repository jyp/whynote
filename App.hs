{-# LANGUAGE RecordWildCards, FlexibleContexts, DeriveFunctor #-}
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

mkStroke :: [Coord] -> GtkP Stroke
mkStroke cs = do
  opts <- use stPen
  return $ Stroke opts (box $ Curve $ V.fromList cs)

strokeLoop :: [Coord] -> GtkP [Coord]
strokeLoop c = do
  old <- mkStroke c
  stRender .= drawStroke old
  invalidate $ boundingBox $ old
  ev <- wait "next stroke point"
  let cs' = (eventCoord ev:c)
  case eventSource ev of
    Eraser -> return c
    Stylus -> case ev of
      Event {..} | eventModifiers == 0 || eventType == Event.Release || coordZ (eventCoord) == 0  -> return cs'
      _ -> strokeLoop cs'
    _ -> strokeLoop c -- ignore events from another source

-- | Remove 0-pressure subsequences in a stroke.
cleanStroke :: [Coord] -> [Coord]
cleanStroke (a:b:c) | coordZ a == 0, coordZ b == 0 = cleanStroke (b:c)
cleanStroke c = c

stroke :: Coord -> GtkP ()
stroke c0 = do
  strk <- mkStroke =<< (cleanStroke . reverse) <$> strokeLoop [c0]
  stNoteData %= (strk:)
  stRender .= return ()
  return ()

lassoProcessLoop :: Source -> [Coord] -> GtkP ClosedCurve
lassoProcessLoop source c = do
  let res = Closed $ V.fromList c
  stRender .= drawLasso res
  invalidate $ boundingBox c
  ev <- wait "next lasso point"
  case eventSource ev == source of
    False -> lassoProcessLoop source c -- ignore events from another source
    True -> case ev of
      Event {eventType  = Event.Release} -> return res
      Event {eventModifiers = 0} -> return res -- motion without any pressed key
      _ -> lassoProcessLoop source (eventCoord ev:c)

deselect :: GtkP ()
deselect = do
  (Selection _ oldSel) <- use stSelection
  stNoteData %= (oldSel++)
  setSelection emptySelection

lassoProcess :: Source -> GtkP ()
lassoProcess source = do
  deselect
  bounds <- lassoProcessLoop source []
  stRender .= return ()
  (inside,outside) <- lassoPartitionStrokes (box bounds) <$> use stNoteData
  setSelection (mkSelection inside)
  stNoteData .= outside
  invalidate $ boundingBox bounds
  return ()

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
fuzzyFactor = use (stTranslation.trZoom.to (10 /))

setSelection :: Selection -> GtkP ()
setSelection sel = do
  stSelection .= sel
  invalidateAll -- because the menu needs to be redrawn (and it annoying to figure where it is)

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
  (erased,kept) <- partitionStrokesNear 10 p <$> use stNoteData
  invalidate $ boundingBox erased
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
                   || coordZ (eventCoord) == 0 -> return ()
        _ -> do action (eventCoord ev)
                neighbourhoodProcessLoop source action

eraseProcess :: Source -> GtkP ()
eraseProcess source = do
  stRender .= return ()
  deselect
  neighbourhoodProcessLoop source eraseNear
  return ()

selectProcess :: Source -> GtkP ()
selectProcess source = do
  stRender .= return ()
  neighbourhoodProcessLoop source selectNear
  return ()

deSelectProcess :: Source -> GtkP ()
deSelectProcess source = do
  stRender .= return ()
  neighbourhoodProcessLoop source deselectNear
  return ()

fingerBegin coord = Finger {fingerStart = coord, fingerCurrent = coord}
fingerAdd coord Nothing = Just (fingerBegin coord)
fingerAdd coord (Just Finger{..}) = Just Finger {fingerCurrent=coord,..}

avg x y = average [x,y]
average :: [Coord] -> Coord
average xs = ((1::Double)/fromIntegral (length xs)) *^ (foldr (+) zero xs)

norm2 (Coord x1 y1 _ _) = x1*x1 + y1*y1
dist a b = sqrt (norm2 (a-b))
peri xs = sum $ zipWith dist xs (rot xs)


touchProcessEntry :: Event -> GtkP ()
touchProcessEntry ev = do
  wakeup ev 70
  touchProcessDetect (Just (eventTime ev)) (M.singleton (eventButton ev) (fingerBegin (eventCoord ev)))

-- FIXME: use absolute positions for this process.
touchProcessDetect :: Maybe Word32 -> M.Map Int Finger -> GtkP ()
touchProcessDetect time0 touches
  | M.null touches = return ()
  | otherwise = do
  ev <- wait ("multi-touch startup " ++ show (M.keys touches))
  wakeup ev 70
  tr <- use stTranslation
  let rb = recycle tr ev
  case time0 of
    Just t0 | (eventTime ev > t0 + 50) && (M.size touches `elem` [1,2]) -> do
       -- fingers stable, run next phase.
       -- FIXME: two input touches must not start too close from each other!
       sel <- use stSelection
       let msel = if any (`inArea` sel) (map fingerStart (M.elems touches))
                  then Just sel
                  else Nothing
       let touches0 = fmap (fmap (apply tr)) (M.elems touches)
       stRender .= do -- show where fingers, to give feedback that the touch gesture is recognized.
         resetMatrix zero
         forM_ touches0 renderFinger
       invalidateIn zero $ boundingBox $ map fingerBox touches0
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
       Timeout -> do
         touchProcessDetect time0 touches
       _ -> do liftIO $ putStrLn $ "WARNING: unexpected event: " ++ show ev
               rb

-- We pass the original selection here, so that we do not repeatedly modify the
-- selection with small translations, which would eventually accumulate
-- numerical errors.
touchProcess :: Maybe Selection -> Translation -> M.Map Int Finger -> GtkP ()
touchProcess selection origTrans touches
  | M.null touches = exit
  | otherwise = do
  ev <- waitInTrans origTrans "multi-touch"
  let rb = do recycle origTrans ev
              stTranslation .= origTrans
              exit
  case eventSource ev of
    MultiTouch -> case () of
      _ | not (eventButton ev `M.member` touches) -> exit
      _ | eventType ev `elem` [Begin,Cancel,End] -> exit
      _ | eventType ev `elem` [Update] -> do
                let touches' = M.alter (fingerAdd (eventCoord ev)) (eventButton ev) touches
                    pss = M.elems touches'
                    ps0 = map fingerStart pss
                    ps1 = map fingerCurrent pss
                    s0 = peri ps0
                    s1 = peri ps1
                    factor = (s1 / (s0+1)) -- FIXME: +1 is not nice here
                    a0 = average ps0
                    a1 = average ps1

                stRender .= do
                  resetMatrix zero -- to have constant size rendering of fingers.
                  forM_ (M.elems  touches') (renderFinger . inIdMatrix)

                inv (M.elems touches' ++ M.elems touches)
                case (length pss,selection) of
                  (1,Just sel) -> transSel sel a0 a1 1
                  (2,Just sel) -> transSel sel a0 a1 factor
                  (1,Nothing) -> transSheet origTrans a0 a1 1
                  (2,Nothing) -> transSheet origTrans a0 a1 factor
                  _ -> exit
                cont touches'
      _ -> cont touches
    Stylus -> rb
    Eraser -> rb
    Timeout -> cont touches
    _ -> do liftIO $ putStrLn $ "event from" ++ show (eventSource ev)
            cont touches
 where
   exit = do
     stRender .= return ()
     inv (M.elems touches)
   inv ts = when (not (null ts)) $ do
     invalidateIn zero $ boundingBox $ map (fingerBox . inIdMatrix) ts
   inIdMatrix = fmap (apply origTrans)
   cont = touchProcess selection origTrans

-- | Translate and zoom the whole sheet by the given amount.
transSheet :: Translation -> Coord -> Coord -> Double -> GtkP ()
transSheet origTrans a0 a1 factor = do
  let Translation z0 dx0 dy0 = origTrans
  let z1 = max 0.1 (z0*factor)
      (a0x,a0y) = xy a0 (,)
      (a1x,a1y) = xy a1 (,)
  stTranslation .= Translation z1 (dx0 + z0 * a1x - z1 * a0x) (dy0 + z0 * a1y - z1 * a0y)
  invalidateAll

-- | Translate and zoom the selection by the given amount.
transSel :: Selection -> Coord -> Coord -> Double -> GtkP ()
transSel origSel a0 a1 factor = do
  let (Coord dx dy _ _) = a1 - factor *^ a0
  setSelection (fmap (apply (Translation factor dx dy)) origSel)

moveSelWithPen :: Selection -> Coord -> GtkP ()
moveSelWithPen origSel origCoord = do
  ev <- wait "move sel pen"
  case eventSource ev of
    Stylus -> case ev of
      Event {eventType  = Event.Release} -> return ()
      Event {eventModifiers = 0} -> return () -- motion without any pressed key
      _ -> do
        transSel origSel origCoord (eventCoord ev) 1
        moveSelWithPen origSel origCoord
    _ -> do moveSelWithPen origSel origCoord -- ignore events from another source

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
  let hideMenu = do stRender .= return (); invalidateAll
  if dist p c > menuOuterCircle
     then hideMenu
     else do
       let rMenu sho p' = renderMenu sho a0 p' c $ map fst options
       stRender .= do active <- rMenu True p ; return ()
       invalidateAll -- TODO optimize
       Event {..} <- waitInTrans zero "menu"
       active <- renderNow $ rMenu False eventCoord
       case eventType of
         Press -> do
           hideMenu
           case active of
              Just i -> snd (options !! i) eventCoord
              Nothing -> recycle zero Event{..}
         _ -> menu' a0 eventCoord c options

penMenu :: [(String, Coord -> GtkP ())]
penMenu = [ (name, \_ -> stPen .= pen) | (name,pen) <- configuredPens]

-- 1: shift
-- 256 mouse 1 or touch
-- 512 mouse 2 (mid)
-- 1024 mouse 3 (right)
whynote :: GtkP ()
whynote = do
 mapM_ (Process.forkHandler . softButtonHandler) softButtons
 Process.forkHandler (palmRejection (-1) (-1))
 loop $ do
  ev <- wait "top-level"
  st <- use (to id)
  sel <- use stSelection
  tr <- use stTranslation
  let pressure = coordZ (eventCoord ev)
      havePressure = pressure > 0
      haveSel = not . isEmptySelection $ sel
      inSel = haveSel && (eventCoord ev `inArea` sel)
      evOrig = apply tr (eventCoord ev)
  -- liftIO $ print ev
  case ev of
    Event {eventSource = Stylus,..} | haveSel, dist evOrig (selMenuCenter st) < menuRootRadius ->
      menu (pi/4) [("Delete",\_ -> deleteSelection)] (selMenuCenter st)
    Event {eventSource = Stylus,..} | dist evOrig rootMenuCenter < menuRootRadius -> do
      menu (pi/4) ([("Pen",menu 0 penMenu)
                   ,("Undo",\c -> do
                        dones <- use stNoteData
                        redos <- use stRedo
                        undoProcess c (redos,dones))
                   ,("Quit",menu 0 [("Confirm",\_ -> quit)])]) rootMenuCenter
    Event {..} | (eventSource == Eraser && (havePressure || eventType == Press))
              || (eventSource == Stylus && (Erase `elem` _stButtons st) && (eventButton == 1 && eventType == Press)) -> do
      if haveSel
        then do if inSel
                  then deleteSelection
                  else deselect
                waitForRelease eventSource
        else do
          eraseNear eventCoord
          eraseProcess eventSource
    Event {eventSource = Stylus,..} | (eventType == Press && eventButton == 1) -> do
      if haveSel
        then if inSel
                then moveSelWithPen sel eventCoord
                else do
                  deselect
                  waitForRelease Stylus
        else stroke eventCoord
    Event {eventSource = Stylus,eventModifiers=512,..} | havePressure  -> do
      lassoProcess (eventSource ev)
    Event {eventSource = Stylus,eventModifiers=1024,..} | havePressure  -> do
      if inSel
        then deSelectProcess (eventSource ev)
        else selectProcess (eventSource ev)
    Event {eventSource = MultiTouch} -> do
      when (eventType ev == Begin) $ do
        touchProcessEntry ev
    _ -> return ()


selMenuCenter :: St -> Coord
selMenuCenter St{..} = apply _stTranslation (intervalHi (selectionBox _stSelection))

rootMenuCenter :: Coord
rootMenuCenter = Coord 40 40 0 0

renderAll ctx st@St{..} = do
   let whenSel = when (not $ isEmptySelection $ _stSelection)
   resetMatrix  _stTranslation
   renderNoteData _stNoteData
   whenSel $ renderSelection _stSelection
   _stRender
   renderSoftButtons ctx st
   renderMenuRoot "menu" rootMenuCenter
   whenSel $ renderMenuRoot "sel" (selMenuCenter st)

----------------------
-- Soft buttons

data Button' a = Button SoftButton String Double a deriving Functor
type Button = Button' Coord

softButtons = [Button Erase "erase" 50 (Coord (-40) (-50) 0 0)]

shiftBySz :: (Int,Int) -> Coord -> Coord
shiftBySz (w,h) Coord {..} = Coord {coordX = coordX + fromIntegral w,
                                    coordY = coordY + fromIntegral h,..}

-- renderSoftButtons :: Ctx -> St -> Render ()
renderSoftButtons ctx St {..}= do
  sz <- liftIO (getWinSize ctx)
  forM_ (map (fmap (shiftBySz sz)) softButtons) $ \(Button btn txt rad c) ->
    renderNode (btn `elem` _stButtons) txt rad c

touchWaitForRelease btn touchNumber = do
  ev <- waitOn "btn release" (\Event{..} -> eventSource == MultiTouch &&
                                            eventButton == touchNumber)
  case eventType ev of
    End -> do
      stButtons %= filter (/= btn)
      invalidateAll
    _ -> touchWaitForRelease btn touchNumber
loop x = x >> loop x

softButtonHandler (Button softBtn txt rad c) = loop $ do
  ctx <- view (to id)
  Event{..} <- waitOn ("btn: " ++ txt) (\Event{..} -> eventSource == MultiTouch &&
                                                      eventType == Begin)
  sz <- liftIO (getWinSize ctx)
  if (dist (shiftBySz sz c) eventCoord < rad)
    then do stButtons %= (softBtn:)
            invalidateAll
            Process.forkHandler (touchWaitForRelease softBtn eventButton)
    else do Process.reject Event{..} -- not for us after all
