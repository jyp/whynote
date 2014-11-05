{-# LANGUAGE RecordWildCards #-}
module App where

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

testProcess :: GtkP ()
testProcess = do
  ev <- wait "event"
  when (eventModifiers ev /= 0) $
    render (drawEv ev)
  testProcess

render :: Cairo.Render () -> GtkP ()
render x = do
  Context {..} <- ask
  liftIO $ Gtk.renderWithDrawWindow ctxDrawWindow x
  h <- liftIO $ Gtk.drawWindowGetWidth ctxDrawWindow
  w <- liftIO $ Gtk.drawWindowGetHeight ctxDrawWindow
  liftIO $ Gtk.widgetQueueDrawArea ctxCanvas 0 0 200 200

strokeProcess :: Source -> [PointerCoord] -> GtkP [PointerCoord]
strokeProcess source c = do
  render $ drawStroke c
  ev <- wait "next stroke point"
  case eventSource ev == source of
    False -> strokeProcess source c -- ignore events from another source
    True -> case eventType ev of
      Event.Release -> return c -- done
      _ -> strokeProcess source (eventCoord ev:c)

strokeProcessStart :: Source -> GtkP ()
strokeProcessStart source = do
  canvas <- ctxCanvas <$> ask
  liftIO $ Gtk.widgetGrabFocus canvas
  strk <- strokeProcess source []
  -- storeStroke strk
  return ()

mainProcess :: GtkP ()
mainProcess = do
  ev <- wait "top-level"
  case ev of
    Event {eventType = Press, eventButton = 0} -> strokeProcessStart (eventSource ev)
    _ -> mainProcess

