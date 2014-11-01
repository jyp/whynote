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

     on canvas buttonPressEvent $ do
       ev <- ask
       liftIO $ print (gdk_event_get_source_device ev)
       liftIO . print =<< eventButton
       return True

     on window deleteEvent $ return False
     on window destroyEvent (do liftIO mainQuit; return True)
     mainGUI

myDraw :: Render ()
myDraw = do
    setSourceRGB 1 1 0
    setLineWidth 5

    moveTo 120 60
    lineTo 60 110
    lineTo 180 110
    closePath

    stroke
