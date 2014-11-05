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
import Data.IORef

testProcess :: GtkP ()
testProcess = do
  ev <- wait "event"
  when (eventModifiers ev /= 0) $
    render (drawEv ev)
  testProcess

invalidate :: GtkP ()
invalidate = do
  Context {..} <- ask
  liftIO $ do
    w <- liftIO $ Gtk.drawWindowGetWidth ctxDrawWindow
    h <- liftIO $ Gtk.drawWindowGetHeight ctxDrawWindow
    Gtk.drawWindowInvalidateRect ctxDrawWindow (Gtk.Rectangle 0 0 w h) False

render :: Cairo.Render () -> GtkP ()
render x = do
  Context {..} <- ask
  liftIO $ writeIORef ctxRender x
  invalidate

strokeProcess :: Source -> [PointerCoord] -> GtkP [PointerCoord]
strokeProcess source c = do
  render $ drawStroke c
  ev <- wait "next stroke point"
  case eventSource ev == source of
    False -> strokeProcess source c -- ignore events from another source
    True -> case ev of
      Event {eventType  = Event.Release} -> return c
      Event {eventModifiers = 0} -> return c -- motion without any pressed key
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
  Context {..} <- ask
  ev <- wait "top-level"
  case ev of
    Event {eventSource = Stylus,eventType = Press, eventButton = 1} -> do
      strokeProcessStart (eventSource ev)
      mainProcess
    _ -> mainProcess

