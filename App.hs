{-# LANGUAGE RecordWildCards #-}
module App where

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
          -> cont $ M.delete (eventButton ev) touches
      _ | eventType ev `elem` [Begin,Update]
          -> case M.lookup (eventButton ev) touches of
               Nothing -> cont $ M.insert (eventButton ev) (eventCoord ev,eventCoord ev) touches
               Just (orig,_) -> do
                 case M.size touches of
                   1 -> do
                     moveSheet origTrans (eventCoord ev - orig)
                   _ -> return ()
                 cont $ M.insert (eventButton ev) (orig,eventCoord ev) touches
      _ -> -- non-multi touch event
        cont touches
    Stylus -> rollback ev origTrans
    Eraser -> rollback ev origTrans
    _ -> cont touches

moveSheet origTrans delta = do
          let (dx,dy) = xy delta (,)
              (x0,y0) = origTrans
          stTranslation .= (dx+x0,dy+y0)
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
      -- liftIO $ print ev
      when (eventType ev == Begin) $ do
        tr <- use stTranslation
        touchProcess tr $ M.singleton (eventButton ev) (eventCoord ev,eventCoord ev)
    Event {eventSource = Touch} -> do
      when (eventType ev `elem` [Press,Motion]) $ do
        tr <- use stTranslation
        simpleTouchProcess tr (eventCoord ev)
    _ -> return ()
  mainProcess

