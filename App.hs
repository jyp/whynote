{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
module App where
import Control.Lens hiding (transform)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Event
import GtkProcess
import NoteData
import Render
import WNPrelude
import qualified Data.Map.Strict as M
import qualified Graphics.UI.Gtk as Gtk
import Prelude ()
import qualified Process
import Data.Bits
import qualified Data.Vector as V
import Config (configuredPens)

invalidateAll :: GtkP ()
invalidateAll = do
  Ctx {..} <- ask
  liftIO $ do
    w <- liftIO $ Gtk.drawWindowGetWidth _ctxDrawWindow
    h <- liftIO $ Gtk.drawWindowGetHeight _ctxDrawWindow
    Gtk.drawWindowInvalidateRect _ctxDrawWindow (Gtk.Rectangle 0 0 w h) False


invalidateIn :: Translation -> Box -> GtkP ()
invalidateIn tr b0 = do
  let Box (x0,y0) (x1,y1) = fmap coordToPt $ extend 5 $ fmap (apply tr) b0
  Ctx {..} <- ask
  let rect = (Gtk.Rectangle x0 y0 (x1-x0) (y1-y0))
  liftIO $ Gtk.drawWindowInvalidateRect _ctxDrawWindow rect False

invalidate :: Box -> GtkP ()
invalidate bx = do
  tr <- use stTranslation
  invalidateIn tr bx

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
  (Selection bbox oldSel) <- use stSelection
  stNoteData %= (oldSel++)
  stSelection .= emptySelection
  invalidate $ bbox

lassoProcess :: Source -> GtkP ()
lassoProcess source = do
  deselect
  bounds <- lassoProcessLoop source []
  stRender .= return ()
  (inside,outside) <- lassoPartitionStrokes (box bounds) <$> use stNoteData
  stSelection .= mkSelection inside
  stNoteData .= outside
  invalidate $ boundingBox bounds
  return ()

mkSelection :: [Stroke] -> Selection
mkSelection strks = Selection (extend 10 $ boundingBox strks) strks

addToSelection :: [Stroke] -> GtkP ()
addToSelection strks = do
  Selection _ s0 <- use stSelection
  let newSel = mkSelection (s0 ++ strks)
  stSelection .= newSel
  invalidate $ boundingBox newSel

selectNear :: Coord -> GtkP ()
selectNear p = do
  f <- fuzzyFactor
  (selected,kept) <- partitionStrokesNear f p <$> use stNoteData
  addToSelection selected
  stNoteData .= kept

fuzzyFactor :: GtkP Double
fuzzyFactor = (10 /) <$> use (stTranslation.trZoom)

deleteSelection :: GtkP ()
deleteSelection = do
  Selection bbox sel <- use stSelection
  stSelection .= emptySelection
  stRedo %= (sel++)
  invalidate $ boundingBox bbox

deselectNear :: Coord -> GtkP ()
deselectNear p = do
  f <- fuzzyFactor
  Selection bbox strokes <- use stSelection
  let (deselected,kept) = partitionStrokesNear f p strokes
  stSelection .= mkSelection kept
  stNoteData %= (++ deselected)
  invalidate bbox

eraseNear :: Coord -> GtkP ()
eraseNear p = do
  (erased,kept) <- partitionStrokesNear 10 p <$> use stNoteData
  invalidate $ boxUnion $ map boundingBox erased
  stRedo %= (erased++)
  stNoteData .= kept

neighbourhoodProcessLoop :: Source -> (Coord -> GtkP ()) -> GtkP ()
neighbourhoodProcessLoop source action = do
  ev <- wait "next neighbourhood point"
  case source `elem` [Stylus, Eraser] of
    False -> neighbourhoodProcessLoop source action -- ignore non-pen events
    True -> case eventSource ev == source of
      False -> Process.pushBack ev -- quick switch between pen/eraser
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
average xs = scale (1/fromIntegral (length xs)) (foldr (+) zero xs)

norm2 (Coord x1 y1 _ _) = x1*x1 + y1*y1
dist a b = sqrt (norm2 (a-b))
peri xs = sum $ zipWith dist xs (rot xs)

eventTime = coordT . eventCoord

touchProcessEntry :: Selection -> Translation -> Event -> GtkP ()
touchProcessEntry sel origTrans ev =
  touchProcess sel origTrans $ M.singleton (eventButton ev) (fingerBegin (eventCoord ev))

oldEnough :: Finger -> Bool
oldEnough Finger{..} = coordT fingerCurrent - coordT fingerStart > 100

debugTouch :: Bool
debugTouch = False

touchProcess :: Selection -> Translation -> M.Map Int Finger -> GtkP ()
touchProcess selection origTrans touches
  | M.null touches = return ()
  | otherwise = do
  let cont = touchProcess selection origTrans
  -- liftIO $ do
  --   putStrLn "touches"
  --   forM_ (M.assocs touches) print
  ev <- waitInTrans origTrans "multi-touch"
  let rb = rollback ev origTrans >> exit
      exit = when debugTouch $ do
        stRender .= return ()
        inv touches
      inv ts =
        when (not (null ts)) $ do
          invalidate $ foldr1 unionBox $ map (extend 50 . pointBox . fingerCurrent) $ M.elems ts
  case eventSource ev of
    MultiTouch -> case () of
      _ | eventType ev `elem` [Cancel,End]
          -> do case M.lookup (eventButton ev) touches of
                  Nothing -> cont touches
                  Just f -> if oldEnough f
                            then exit -- a 'real' touch is being let go; stop the multi-touch operation.
                            else cont $ M.delete (eventButton ev) touches
      _ | eventType ev `elem` [Begin,Update]
          -> do let touches' = M.alter (fingerAdd (eventCoord ev)) (eventButton ev) touches
                    pss = filter oldEnough $ M.elems touches'
                    ps0 = map fingerStart pss
                    ps1 = map fingerCurrent pss
                    s0 = peri ps0
                    s1 = peri ps1
                    factor = (s1 / (s0+1))
                    a0 = average ps0
                    a1 = average ps1
                    inSel = any (`inArea` selection) ps0
                -- debug
                inv touches
                when debugTouch $ stRender .= do
                  resetMatrix origTrans -- because the touch coordinates are in that matrix.
                  forM_ (M.elems touches') renderFinger
                inv touches'
                case (length pss,inSel) of
                  (1,True) -> transSel selection a0 a1 1
                  (2,True) -> transSel selection a0 a1 factor
                  (1,False) -> transSheet origTrans a0 a1 1
                  (n,False) | n > 1 -> transSheet origTrans a0 a1 factor
                  _ -> exit
                cont touches'
      _ -> cont touches
    Stylus -> rb
    Eraser -> rb
    _ -> do liftIO $ putStrLn $ "event from" ++ show (eventSource ev)
            cont touches

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
  let (Coord dx dy _ _) = a1 - scale factor a0
  stSelection .= fmap (translation factor dx dy) origSel
  invalidateAll -- optim. possible

simpleTouchProcess :: Translation -> Coord -> GtkP ()
simpleTouchProcess origTrans origCoord = do
  let cont = simpleTouchProcess origTrans origCoord
  ev <- waitInTrans origTrans "simple-touch"
  case eventSource ev of
    Touch -> case () of
      _ | eventType ev `elem` [Release]
          -> return () -- finish
      _ | eventType ev `elem` [Motion,Press]
          -> do transSheet origTrans origCoord (eventCoord ev) 1
                cont
      _ -> -- non-multi touch event
        cont
    Stylus -> Process.pushBack ev
    Eraser -> Process.pushBack ev
    _ -> cont

rollback ev origTrans = do
  Process.pushBack ev
  stTranslation .= origTrans

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

render r = do
  dw <- view ctxDrawWindow
  liftIO $ Gtk.renderWithDrawWindow dw r

shiftUndos n (us,d:ds) | n > 0 = shiftUndos (n-1) (d:us,ds)
shiftUndos n (u:us,ds) | n < 0 = shiftUndos (n+1) (us,u:ds)
shiftUndos n x = x

undoProcess y0 st = do
  ev <- wait "undo"
  (_,y) <- screenCoords $ eventCoord $ ev
  let (newRedo,kept) = shiftUndos dy st
      dy = (y-y0) `div` 10
  stNoteData .= kept
  stRedo .= newRedo
  invalidateAll
  case ev of
      Event {eventType = Press} -> waitForRelease (eventSource ev)
      _ -> undoProcess y0 st

distC :: (Int,Int) ->  (Int,Int) -> Int -> Bool
distC (x1,y1)(x2,y2) d = sq(x2-x1) + sq(y2-y1) > sq d
  where sq x = x*x

menu :: [(String,Coord -> GtkP ())] -> Coord -> GtkP ()
menu options c = do
  c' <- screenCoords c
  menu' c' c' options

menu' :: (Int, Int) -> (Int, Int) -> [(String, Coord -> GtkP ())] -> GtkP ()
menu' p c options = do
  let hideMenu = do stRender .= return (); invalidateAll; return ()
  if distC p c (round menuOuterCircle)
     then hideMenu
     else do
       let rMenu show p'' = renderMenu show p'' c $ map fst options
       stRender .= (rMenu True p >> return ())
       invalidateAll -- optimize
       Event {..} <- wait "menu"
       p' <- screenCoords eventCoord
       active <- render $ rMenu False p'
       -- liftIO $ print active
       case eventType of
         Press -> do
           hideMenu
           case active of
              Just i -> (map snd options !! i) eventCoord
              Nothing -> return ()
         _ -> menu' p' c options

penMenu = [ (name, \_ -> stPen .= pen) | (name,pen) <- configuredPens]


-- 1: shift
-- 256 mouse 1
-- 512 mouse 2 (mid)
-- 1024 mouse 3 (right)
mainProcess :: GtkP ()
mainProcess = do
  ev <- wait "top-level"
  -- when (eventType ev == Press) $ liftIO $ print ev
  sel <- use stSelection
  let pressure = coordZ$ eventCoord $ ev
      havePressure = pressure > 0.01
      haveSel = not . isEmptySetection $ sel
      inSel = haveSel && (eventCoord ev `inArea` sel)
  (cx,_) <- screenCoords (eventCoord ev)
  case ev of
    Event {eventSource = Stylus,..} | cx < 30 -> do
      menu ([("Delete",\_ -> do
                 deleteSelection
                 stSelection .= emptySelection
                 ) | haveSel] ++
            [("Pen",menu penMenu)
            ,("Undo",\c -> do
                 (_,y) <- screenCoords c
                 dones <- use stNoteData
                 redos <- use stRedo
                 undoProcess y (redos,dones))
            ,("Quit",menu [("Confirm",\_ -> quit)])]) eventCoord
    Event {eventSource = Stylus,..} | (eventType == Press && eventButton == 1) || eventModifiers == 256 -> do
      if haveSel
        then if inSel
                then moveSelWithPen sel eventCoord
                else do
                  deselect
                  waitForRelease Stylus
        else stroke eventCoord
    Event {eventSource = Eraser,..} | coordZ eventCoord > 0.01 || eventType == Press -> do
      if haveSel
        then if inSel
                then do
                  deleteSelection
                  waitForRelease Eraser
                else do
                  deselect
                  waitForRelease Eraser
        else do
          eraseNear (eventCoord)
          eraseProcess (eventSource ev)
    Event {eventSource = Stylus,eventModifiers=512,..} | havePressure  -> do
      lassoProcess (eventSource ev)
    Event {eventSource = Stylus,eventModifiers=1024,..} | havePressure  -> do
      if inSel
        then deSelectProcess (eventSource ev)
        else selectProcess (eventSource ev)
    Event {eventSource = MultiTouch} -> do
      when (eventType ev == Begin) $ do
        tr <- use stTranslation
        touchProcessEntry sel tr ev
    Event {eventSource = Touch,..} | eventModifiers .&. 256 /= 0 -> do
      when (eventType `elem` [Press,Motion]) $ do
        tr <- use stTranslation
        simpleTouchProcess tr eventCoord
    _ -> return ()
  mainProcess
