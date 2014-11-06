import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Device
import Render
import NoteData
import Control.Monad.Reader
import Config
import Process
import GtkProcess
import App
import Data.IORef

main :: IO ()
main = do
     initGUI
     cfg <- loadConfig
     window <- windowNew
     set window [windowTitle := "WhyNote",
                 windowDefaultWidth := 300, windowDefaultHeight := 200]

     canvas <- drawingAreaNew
     devices <- initDevice (castToWidget canvas) cfg
     print devices
     containerAdd window canvas
     widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

     widgetShowAll window
     Just drawin <- widgetGetWindow canvas
     render <- newIORef $ return ()
     scribbles <- newIORef emptyNoteData
     let ctx = Context drawin canvas render scribbles
     setup <- exec $ runGtkP ctx mainProcess
     continuation <- newIORef setup


     widgetAddEvents canvas [TouchMask, PointerMotionMask]

     on canvas draw $ liftIO $ do
       state <- readIORef continuation
       tmpS <- readIORef render
       s <- readIORef scribbles
       renderWithDrawWindow drawin $ do
         moveTo 0 20
         showText $ show state
         tmpS
         renderNoteData s
       return ()

     let handleEvent :: EventM t Bool
         handleEvent = do
           ev <- ask
           liftIO $ do
             ev' <- getPointer devices ev
             oldState <- readIORef continuation
             newState <- resume oldState ev'
             -- print ev'
             -- print newState
             writeIORef continuation newState
           return True

     on canvas motionNotifyEvent handleEvent
     on canvas buttonPressEvent handleEvent

     on window deleteEvent $ return False
     on window destroyEvent (do liftIO mainQuit; return True)
     mainGUI
