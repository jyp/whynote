{-# LANGUAGE RecordWildCards #-}
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

invalidateAll :: GtkP ()
invalidateAll = do
  Ctx {..} <- ask
  liftIO $ do
    w <- liftIO $ Gtk.drawWindowGetWidth _ctxDrawWindow
    h <- liftIO $ Gtk.drawWindowGetHeight _ctxDrawWindow
    Gtk.drawWindowInvalidateRect _ctxDrawWindow (Gtk.Rectangle 0 0 w h) False

invalidate :: Box -> GtkP ()
invalidate b0 = do
  let Box p0 p1 = extend 5 b0
  Ctx {..} <- ask
  (x0,y0) <- screenCoords p0
  (x1,y1) <- screenCoords p1
  let rect = (Gtk.Rectangle x0 y0 (x1-x0) (y1-y0))
  liftIO $ do
    Gtk.drawWindowInvalidateRect _ctxDrawWindow rect False

strokeLoop :: Source -> [Coord] -> GtkP Stroke
strokeLoop source c = do
  let res = Stroke (box $ Curve $ V.fromList c)
  stRender .= drawStroke res 
  invalidate $ boundingBox c
  ev <- wait "next stroke point"
  case eventSource ev == source of
    False -> strokeLoop source c -- ignore events from another source
    True -> case ev of
      Event {eventType  = Event.Release} -> return res
      Event {eventModifiers = 0} -> return res -- motion without any pressed key
      _ -> strokeLoop source (eventCoord ev:c)

stroke :: Source -> GtkP ()
stroke source = do
  strk <- strokeLoop source []
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

deselectProcess :: Source -> GtkP ()
deselectProcess source = do
  deselect
  waitForRelease source

lassoProcess :: Source -> GtkP ()
lassoProcess source = do
  deselect
  bounds <- lassoProcessLoop source []
  stRender .= return ()
  (inside,outside) <- lassoPartitionStrokes (box bounds) <$> use stNoteData
  stSelection .= Selection (extend 10 $ boundingBox inside) inside
  stNoteData .= outside
  invalidate $ boundingBox bounds
  return ()

addToSelection strks = do
  Selection _ s0 <- use stSelection
  let s1 = s0 ++ strks
      bbox1 = extend 10 $ boundingBox s1
  stSelection .= Selection bbox1 s1
  invalidate bbox1

selectNear :: Coord -> GtkP ()
selectNear p = do
  (selected,kept) <- partitionStrokesNear 10 p <$> use stNoteData
  addToSelection selected
  stNoteData .= kept

eraseNear :: Coord -> GtkP ()
eraseNear p = do
  (erased,kept) <- partitionStrokesNear 10 p <$> use stNoteData
  invalidate $ boxUnion $ map boundingBox erased
  stNoteData .= kept

neighbourhoodProcessLoop :: Source -> (Coord -> GtkP ()) -> GtkP ()
neighbourhoodProcessLoop source action = do
  ev <- wait "next neighbourhood point"
  case eventSource ev == source of
    False -> neighbourhoodProcessLoop source action -- ignore events from another source
    True -> case ev of
      Event {eventType  = Event.Release} -> return ()
      Event {eventModifiers = 0} -> return () -- motion without any pressed key
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

add cord Nothing = Just (cord,cord)
add cord (Just (c0,_)) = Just (c0,cord)

avg x y = average [x,y]
average xs = ((1/fromIntegral (length xs)) *) .>> foldr (+) zero xs
-- 
norm2 (Coord x1 y1 _ _) = x1*x1 + y1*y1
dist a b = sqrt (norm2 (a-b))
peri xs = sum $ zipWith dist xs (rot xs)

touchProcess :: Selection -> Translation -> M.Map Int (Coord,Coord) -> GtkP ()
touchProcess selection origTrans touches
  | M.null touches = return ()
  | otherwise = do
  let cont = touchProcess selection origTrans
  -- liftIO $ do
  --   putStrLn "touches"
  --   forM_ (M.assocs touches) print
  ev <- waitInTrans origTrans "multi-touch"
  case eventSource ev of
    MultiTouch -> case () of
      _ | eventType ev `elem` [Cancel,End]
          -> return () 
      _ | eventType ev `elem` [Begin,Update]
          -> do let touches' = M.alter (add (eventCoord ev)) (eventButton ev) touches
                    pss = M.elems touches'
                    (ps0,ps1) = unzip pss
                    s0 = peri ps0
                    s1 = peri ps1
                    factor = (s1 / (s0+1))
                    a0 = average ps0
                    a1 = average ps1
                    inSel = any (`inArea` selection) ps0
                case (M.size touches',inSel) of
                  (1,True) -> transSel selection a0 a1 1
                  (2,True) -> transSel selection a0 a1 factor
                  (2,False) -> transSheet origTrans a0 a1 1
                  (3,False) -> transSheet origTrans a0 a1 factor
                  _ -> return ()
                cont touches'
      _ -> cont touches
    Stylus -> return ()
    Eraser -> return ()
    _ -> cont touches


transSheet origTrans a0 a1 factor = do
  let Translation z0 dx0 dy0 = origTrans
  let d = a1 - a0
      (dx,dy) = xy d (,)
      z1 = max 0.1 (z0*factor)
      (a0x,a0y) = xy a0 (,)
      (a1x,a1y) = xy a1 (,)
  stTranslation .= Translation z1 (dx0 + z0 * a1x - z1 * a0x) (dy0 + z0 * a1y - z1 * a0y)
  invalidateAll

transSel origSel a0 a1 factor = do
  -- liftIO $ print (a0,a1,factor)
  let -- a0 * factor + d = a1
      (dx,dy) = xy (a1 - factor .* a0) (,)
  stSelection .= transform (Translation factor dx dy) origSel
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

distC :: (Int,Int) ->  (Int,Int) -> Int -> Bool
distC (x1,y1)(x2,y2) d = sq(x2-x1) + sq(y2-y1) > sq d
  where sq x = x*x

menu ::  Coord -> [(String,Coord -> GtkP ())] -> GtkP ()
menu c options = do
  c' <- screenCoords c
  menu' c' c' options

menu' p c options = do
  let exit = do stRender .= return (); invalidateAll; return ()
  if distC p c 100
     then exit
     else do
       let rMenu show p'' = renderMenu show p'' c $ map fst options
       stRender .= (rMenu True p >> return ())
       invalidateAll -- optimize
       Event {..} <- wait "menu"
       p' <- screenCoords eventCoord
       active <- render $ rMenu False p'
       liftIO $ print active
       case eventType of
         Press -> case active of
           Just i -> (map snd options !! i) eventCoord
           Nothing -> exit
         _ -> menu' p' c options

-- 1: shift
-- 256 mouse 1
-- 512 mouse 2 (mid)
-- 1024 mouse 3 (right)
mainProcess :: GtkP ()
mainProcess = do
  ev <- wait "top-level"
  when (eventType ev == Press) $
    liftIO $ print ev
  sel <- use stSelection
  let pressure = coordZ $ eventCoord $ ev
      havePressure = pressure > 0.01
      haveSel = not . isEmptySetection $ sel
      inSel = haveSel && (eventCoord ev `inArea` sel)
  (cx,_) <- screenCoords (eventCoord ev)
  case ev of
    Event {eventSource = Stylus,..} | cx < 30 -> do
       menu eventCoord [("Quit",\_ -> quit)]
    Event {eventSource = Stylus,..} | (eventType == Press && eventButton == 1) || (eventModifiers == 256 && havePressure) -> do
      if haveSel
        then if inSel 
                then moveSelWithPen sel eventCoord
                else deselectProcess Stylus
        else stroke (eventSource ev)
    Event {eventSource = Eraser,..} | coordZ eventCoord > 0.01 || eventType == Press -> do
      if haveSel
        then if inSel
                then do
                  stSelection .= emptySelection
                  waitForRelease Eraser
                else deselectProcess Eraser
        else do
          eraseNear (eventCoord)
          eraseProcess (eventSource ev)
    Event {eventSource = Stylus,eventModifiers=512,..} | havePressure  -> do
      lassoProcess (eventSource ev)
    Event {eventSource = Stylus,eventModifiers=1024,..} | havePressure  -> do
      selectProcess (eventSource ev)
    Event {eventSource = MultiTouch} -> do
      when (eventType ev == Begin) $ do
        tr <- use stTranslation
        sel <- use stSelection
        touchProcess sel tr $ M.singleton (eventButton ev) (eventCoord ev,eventCoord ev)
    Event {eventSource = Touch,..} | eventModifiers .&. 256 /= 0 -> do
      when (eventType `elem` [Press,Motion]) $ do
        tr <- use stTranslation
        simpleTouchProcess tr eventCoord
    _ -> return ()
  mainProcess

