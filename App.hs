module App where

import Event
import Process
import Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo as Cairo hiding (liftIO)
import Control.Monad
import Control.Monad.IO.Class (liftIO)

drawEv :: Event -> Render ()
drawEv ev@Event{eventCoord = PointerCoord x y z t} = do
    setSourceRGB 0 0 0
    setLineWidth 2
    moveTo x y
    arc x y (2 * z) 0 3
    stroke

testProcess dw = do
  ev <- wait "event"
  liftIO $ when (eventModifiers ev /= 0) $
    renderWithDrawWindow dw (drawEv ev)
  testProcess dw

{-
strokeProcess ps'@(p1:ps)  = do
  drawStroke ps'
  ev <- waitEvent
  case evType ev of
    EMotion -> do
      let p0 = pos ev
          d = norm $ p1 - p0
      if d > 0.1
         then strokeProcess (p0:p1:ps)
         else strokeProcess ps'
    ERelease -> do
      return ps'


strokeProcessStart = do
  grabFocus
  strk <- strokeProcess
  storeStroke strk

mainProcess = do
  ev <- waitEvent
  case evType ev of
    EButton -> strokeProcess (pos ev)
    _ -> mainProcess

-}
