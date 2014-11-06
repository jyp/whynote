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
  Ctx {..} <- ask
  stRender .= x

strokeProcess :: Source -> [Coord] -> GtkP [Coord]
strokeProcess source c = do
  render $ drawStroke c
  invalidate $ boundingBox c
  ev <- wait "next stroke point"
  case eventSource ev == source of
    False -> strokeProcess source c -- ignore events from another source
    True -> case ev of
      Event {eventType  = Event.Release} -> return c
      Event {eventModifiers = 0} -> return c -- motion without any pressed key
      _ -> strokeProcess source (eventCoord ev:c)

strokeProcessStart :: Source -> GtkP ()
strokeProcessStart source = do
  Ctx {..} <- ask
  strk <- strokeProcess source []
  stNoteData %= (strk:)
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
      Event {eventType  = Event.Release} -> return c
      Event {eventModifiers = 0} -> return c -- motion without any pressed key
      _ -> lassoProcessLoop source (eventCoord ev:c)

lassoProcessStart :: Source -> GtkP ()
lassoProcessStart source = do
  Ctx {..} <- ask
  lasso <- lassoProcessLoop source []
  stNoteData %= (strokesOutside lasso)
  stRender .= return ()
  invalidate $ boundingBox lasso
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
  Ctx {..} <- ask
  eraseProcessLoop source
  return ()

touchProcess :: Translation -> M.Map Int (Coord,Coord) -> GtkP ()
touchProcess origTrans touches
  | M.null touches = return ()
  | otherwise = do
  let cont = touchProcess origTrans
  liftIO $ do
    putStrLn "touches"
    forM_ (M.assocs touches) print
  ev <- waitInTrans origTrans "touch"
  case eventSource ev of
    Touch -> case () of
      _ | eventType ev `elem` [Cancel,End]
          -> cont $ M.delete (eventButton ev) touches
      _ | eventType ev `elem` [Begin,Update]
          -> case M.lookup (eventButton ev) touches of
               Nothing -> cont $ M.insert (eventButton ev) (eventCoord ev,eventCoord ev) touches
               Just (orig,_) -> do
                 case M.size touches of
                   1 -> do
                     let (dx,dy) = xy (,) (eventCoord ev - orig)
                         (x0,y0) = origTrans
                     stTranslation .= (dx+x0,dy+y0)
                     invalidateAll
                   _ -> return ()
                 cont $ M.insert (eventButton ev) (orig,eventCoord ev) touches
      _ -> -- non-multi touch event
        cont touches
    Stylus -> do
      liftIO $ putStrLn "rollback"
      stTranslation .= origTrans
    _ -> cont touches

mainProcess :: GtkP ()
mainProcess = do
  Ctx {..} <- ask
  ev <- wait "top-level"
  case ev of
    Event {eventSource = Stylus,eventType = Press, eventButton = 1} -> do
      strokeProcessStart (eventSource ev)
    Event {eventSource = Eraser,eventType = Press, eventButton = 1} -> do
      eraseNear (eventCoord ev)
      eraseProcess (eventSource ev)
    Event {eventSource = Stylus,eventModifiers=1024,..} | coordZ eventCoord > 0.01  -> do
      lassoProcessStart (eventSource ev)
    Event {eventSource = Touch} -> do
      liftIO $ print ev
      when (eventType ev == Begin) $ do
        tr <- use stTranslation
        touchProcess tr $ M.singleton (eventButton ev) (eventCoord ev,eventCoord ev)
    _ -> return ()
  mainProcess

