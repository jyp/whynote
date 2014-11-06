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

strokeProcess :: Source -> [Coord] -> GtkP [Coord]
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
  Context {..} <- ask
  liftIO $ Gtk.widgetGrabFocus ctxCanvas
  strk <- strokeProcess source []
  liftIO $ do
    modifyIORef ctxNoteData (strk:)
    writeIORef ctxRender $ return ()
  return ()

lassoProcessLoop :: Source -> [Coord] -> GtkP [Coord]
lassoProcessLoop source c = do
  render $ drawLasso c
  ev <- wait "next lasso point"
  case eventSource ev == source of
    False -> lassoProcessLoop source c -- ignore events from another source
    True -> case ev of
      Event {eventType  = Event.Release} -> return c
      Event {eventModifiers = 0} -> return c -- motion without any pressed key
      _ -> lassoProcessLoop source (eventCoord ev:c)

lassoProcessStart :: Source -> GtkP ()
lassoProcessStart source = do
  Context {..} <- ask
  liftIO $ Gtk.widgetGrabFocus ctxCanvas
  lasso <- lassoProcessLoop source []
  liftIO $ do
    modifyIORef ctxNoteData (strokesOutside lasso)
    writeIORef ctxRender $ return ()
  invalidate
  return ()


mainProcess :: GtkP ()
mainProcess = do
  Context {..} <- ask
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

