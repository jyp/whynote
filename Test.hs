import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Device
import Control.Monad.Reader
import Config

main :: IO ()
main= do
     initGUI
     window <- windowNew
     set window [windowTitle := "WhyNote",
                 windowDefaultWidth := 300, windowDefaultHeight := 200]

     canvas <- drawingAreaNew
     devices <- initDevice (castToWidget canvas) wnConfig
     print devices
     containerAdd window canvas
     widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

     widgetShowAll window
     Just drawin <- widgetGetWindow canvas
     on canvas draw $ do
       liftIO $ renderWithDrawWindow drawin myDraw
       return ()
     debugShit (castToWidget canvas)
     widgetAddEvents canvas [TouchMask, ButtonMotionMask]

     on canvas buttonPressEvent $ do
       ev <- ask
       liftIO $ putStrLn "Button"
       liftIO $ print =<< getPointer devices ev
       return True

     on canvas motionNotifyEvent $ do
       ev <- ask
       liftIO $ putStrLn "Motion"
       liftIO $ print =<< getPointer devices ev
       return True

     on window deleteEvent $ return False
     on window destroyEvent (do liftIO mainQuit; return True)
     mainGUI

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

myDraw :: Render ()
myDraw = do
    setSourceRGB 1 1 0
    setLineWidth 5

    moveTo 120 60
    lineTo 60 110
    lineTo 180 110
    closePath

    stroke
