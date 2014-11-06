{-# LANGUAGE RecordWildCards #-}
module App where

import NoteData
import Event
import GtkProcess
import Process
import Render
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.Rendering.Cairo as Cairo
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Applicative
import Control.Lens

testProcess :: GtkP ()
testProcess = do
  ev <- wait "event"
  when (eventModifiers ev /= 0) $ do
    render (drawEv ev)
    invalidateAll
  testProcess

invalidateAll :: GtkP ()
invalidateAll = do
  Ctx {..} <- ask
  liftIO $ do
    w <- liftIO $ Gtk.drawWindowGetWidth _ctxDrawWindow
    h <- liftIO $ Gtk.drawWindowGetHeight _ctxDrawWindow
    Gtk.drawWindowInvalidateRect _ctxDrawWindow (Gtk.Rectangle 0 0 w h) False

screenCoords :: Coord -> GtkP (Int,Int)
screenCoords (Coord x y _ _) = do
  -- FIXME: apply transformation matrix
  return (round x, round y)

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
  liftIO $ Gtk.widgetGrabFocus _ctxCanvas
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
  liftIO $ Gtk.widgetGrabFocus _ctxCanvas
  lasso <- lassoProcessLoop source []
  stNoteData %= (strokesOutside lasso)
  stRender .= return ()
  invalidate $ boundingBox lasso
  return ()


mainProcess :: GtkP ()
mainProcess = do
  Ctx {..} <- ask
  ev <- wait "top-level"
  case ev of
    Event {eventSource = Stylus,eventType = Press, eventButton = 1} -> do
      strokeProcessStart (eventSource ev)
      mainProcess
    Event {eventSource = Stylus,eventModifiers=1024,..} | coordZ eventCoord > 0.01  -> do
      liftIO $ print $ ev
      lassoProcessStart (eventSource ev)
      mainProcess
    _ -> mainProcess

