{-# LANGUAGE RecordWildCards #-}
module App where

import qualified Prelude
import WNPrelude
import NoteData
import Event
import GtkProcess
import Render
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.Rendering.Cairo as Cairo
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Applicative
import Control.Lens
import qualified Data.Map.Strict as M
import qualified Process

invalidateAll :: GtkP ()
invalidateAll = do
  Ctx {..} <- ask
  liftIO $ do
    w <- liftIO $ Gtk.drawWindowGetWidth _ctxDrawWindow
    h <- liftIO $ Gtk.drawWindowGetHeight _ctxDrawWindow
    Gtk.drawWindowInvalidateRect _ctxDrawWindow (Gtk.Rectangle 0 0 w h) False

extra = Coord 5 5 0 0

invalidate :: Box -> GtkP ()
invalidate (Box p0 p1)= do
  Ctx {..} <- ask
  (x0,y0) <- screenCoords (p0 - extra)
  (x1,y1) <- screenCoords (p1 + extra)
  let rect = (Gtk.Rectangle x0 y0 (x1-x0) (y1-y0))
  liftIO $ do
    Gtk.drawWindowInvalidateRect _ctxDrawWindow rect False

render :: Cairo.Render () -> GtkP ()
render x = do
  stRender .= x

strokeLoop :: Source -> [Coord] -> GtkP [Coord]
strokeLoop source c = do
  render $ drawStroke $ Stroke c
  invalidate $ boundingBox c
  ev <- wait "next stroke point"
  case eventSource ev == source of
    False -> strokeLoop source c -- ignore events from another source
    True -> case ev of
      Event {eventType  = Event.Release} -> return c
      Event {eventModifiers = 0} -> return c -- motion without any pressed key
      _ -> strokeLoop source (eventCoord ev:c)

stroke :: Source -> GtkP ()
stroke source = do
  deselect
  strk <- strokeLoop source []
  stNoteData %= (Stroke strk:)
  stRender .= return ()
  return ()

lassoProcessLoop :: Source -> [Coord] -> GtkP [Coord]
lassoProcessLoop source c = do
  render $ drawLasso c
  invalidate $ boundingBox c
  ev <- wait "next lasso point"
  case eventSource ev == source of
    False -> lassoProcessLoop source c -- ignore events from another source
    True -> case ev of
      Event {eventType  = Event.Release} -> return $ c
      Event {eventModifiers = 0} -> return $ c -- motion without any pressed key
      _ -> lassoProcessLoop source (eventCoord ev:c)

deselect :: GtkP ()
deselect = do
  oldSel <- use stSelection
  stNoteData %= (oldSel++)
  stSelection .= []
  invalidate $ boundingBox oldSel
  
lassoProcess :: Source -> GtkP ()
lassoProcess source = do
  deselect
  bounds <- lassoProcessLoop source []
  stRender .= return ()
  (inside,outside) <- lassoPartitionStrokes bounds <$> use stNoteData
  stSelection .= inside
  stNoteData .= outside
  invalidate $ boundingBox bounds
  return ()

eraseNear :: Coord -> GtkP ()
eraseNear p = do
  (erased,kept) <- partitionStrokesNear 100 p <$> use stNoteData
  invalidate $ boxUnion $ map boundingBox erased
  stNoteData .= kept

eraseProcessLoop :: Source -> GtkP ()
eraseProcessLoop source = do
  ev <- wait "next erase point"
  case eventSource ev == source of
    False -> eraseProcessLoop source -- ignore events from another source
    True -> case ev of
      Event {eventType  = Event.Release} -> return ()
      Event {eventModifiers = 0} -> return () -- motion without any pressed key
      _ -> do eraseNear (eventCoord ev)
              eraseProcessLoop source

eraseProcess :: Source -> GtkP ()
eraseProcess source = do
  stRender .= return ()
  deselect
  eraseProcessLoop source
  return ()

add cord Nothing = Just (cord,cord)
add cord (Just (c0,_)) = Just (c0,cord)

avg x y = average [x,y]
average xs = ((1/fromIntegral (length xs)) *) .>> foldr (+) zero xs
-- 
norm2 (Coord x1 y1 _ _) = x1*x1 + y1*y1
dist a b = sqrt (norm2 (a-b))
peri a b c = dist a b + dist b c + dist c a

touchProcess :: Translation -> M.Map Int (Coord,Coord) -> GtkP ()
touchProcess origTrans touches
  | M.null touches = return ()
  | otherwise = do
  let cont = touchProcess origTrans
  -- liftIO $ do
  --   putStrLn "touches"
  --   forM_ (M.assocs touches) print
  ev <- waitInTrans origTrans "multi-touch"
  case eventSource ev of
    MultiTouch -> case () of
      _ | eventType ev `elem` [Cancel,End]
          -> return ()
             -- cont $ M.delete (eventButton ev) touches
      _ | eventType ev `elem` [Begin,Update]
          -> do let touches' = M.alter (add (eventCoord ev)) (eventButton ev) touches
                liftIO $ print $ M.keys touches'
                case M.elems touches' of
                  [(p0,p1),(q0,q1)] -> do
                    let s0 = dist p0 q0
                        s1 = dist p1 q1
                        a0 = avg p0 q0
                        a1 = avg p1 q1
                    transSheet origTrans a0 a1 (s1 / (s0+1))
                    cont touches'
                  -- [(p0,p1),(q0,q1),(r0,r1)] -> do
                  --   let s0 = peri p0 q0 r0
                  --       s1 = peri p1 q1 r1
                  --       c = average [p0,q0,r0]
                  --   zoomSheet origTrans c (s1 / (s0+1))
                  --   cont touches'
                  _ -> cont touches'
      _ -> cont touches
    Stylus -> rollback ev origTrans
    Eraser -> rollback ev origTrans
    _ -> cont touches


transSheet origTrans a0 a1 factor = do
  liftIO $ print (a0,a1,factor)
  let Translation z0 x0 y0 = origTrans
  let c = avg a0 a1
      d = a1 - a0
      (cx,cy) = xy c (,)
      (dx,dy) = xy d (,)
      f = z0*(1-factor)
  stTranslation .= Translation (z0*factor) (x0 + dx + cx*f) (y0 + dy + cy*f)
  invalidateAll

moveSheet origTrans delta = do
          let (dx,dy) = xy delta (,)
              Translation z0 x0 y0 = origTrans
          stTranslation .= Translation z0 (dx+x0) (dy+y0)
          invalidateAll

zoomSheet origTrans center factor = do
  let Translation z0 x0 y0 = origTrans
      (cx,cy) = xy center (,)
      f = z0*(1-factor)
  stTranslation .= Translation (z0*factor) (x0 + cx*f) (y0 + cy*f)
      -- we want: cx*z0 + x0 = cx*z0*factor + x1
      -- solve for x1: x1 = cx*z0 (1 - factor) + x0 
  invalidateAll

simpleTouchProcess :: Translation -> Coord -> GtkP ()
simpleTouchProcess origTrans origCoord = do
  let cont = simpleTouchProcess origTrans origCoord
  ev <- waitInTrans origTrans "simple-touch"
  case eventSource ev of
    Touch -> case () of
      _ | eventType ev `elem` [Release]
          -> return () -- finish
      _ | eventType ev `elem` [Motion,Press]
          -> do moveSheet origTrans (eventCoord ev - origCoord)
                cont
      _ -> -- non-multi touch event
        cont
    Stylus -> Process.pushBack ev
    Eraser -> Process.pushBack ev
    _ -> cont

rollback ev origTrans = do
  Process.pushBack ev
  stTranslation .= origTrans
  
mainProcess :: GtkP ()
mainProcess = do
  ev <- wait "top-level"
  Ctx {..} <- ask
  liftIO $ print ev
  let pressure = coordZ $ eventCoord $ ev
      havePressure = pressure > 0.01
  case ev of
    Event {eventSource = Stylus,..} | (eventType == Press && eventButton == 1) || (eventModifiers == 256 && havePressure) -> do
      stroke (eventSource ev)
    Event {eventSource = Eraser,..} | coordZ eventCoord > 0.01 || eventType == Press -> do
      eraseNear (eventCoord)
      eraseProcess (eventSource ev)
    Event {eventSource = Stylus,eventModifiers=1024,..} | havePressure  -> do
      lassoProcess (eventSource ev)
    Event {eventSource = MultiTouch} -> do
      when (eventType ev == Begin) $ do
        tr <- use stTranslation
        touchProcess tr $ M.singleton (eventButton ev) (eventCoord ev,eventCoord ev)
    Event {eventSource = Touch} -> do
      when (eventType ev `elem` [Press,Motion]) $ do
        tr <- use stTranslation
        simpleTouchProcess tr (eventCoord ev)
    _ -> return ()
  mainProcess

