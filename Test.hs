{-# LANGUAGE RecordWildCards #-}
import Graphics.UI.Gtk as Gtk
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

touchEvent :: WidgetClass self => Signal self (EventM EAny Bool)
touchEvent = Signal (eventM "touch_event" [TouchMask])

main :: IO ()
main = do
     initGUI
     cfg <- loadConfig
     window <- windowNew
     set window [windowTitle := "WhyNote",
                 windowDefaultWidth := 300, windowDefaultHeight := 200]

     canvas <- drawingAreaNew
     set canvas [widgetCanFocus := True]
     Gtk.widgetGrabFocus canvas
     devices <- initDevice (castToWidget canvas) cfg
     print devices
     containerAdd window canvas
     widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

     widgetShowAll window
     Just drawin <- widgetGetWindow canvas
     setEventCompression drawin False
     let ctx = Ctx drawin canvas
     setup <- exec $ runGtkP ctx mainProcess
     continuation <- newIORef setup

     widgetAddEvents canvas [PointerMotionMask, TouchMask]

     on canvas draw $ liftIO $ do
       Wait (St {..}) msg _ <- readIORef continuation
       renderWithDrawWindow drawin $ do
         moveTo 0 20
         showText $ msg
         _stRender
         renderNoteData _stNoteData
       return ()

     let handleEvent :: EventM t Bool
         handleEvent = do
           ev <- ask
           liftIO $ do
             ev' <- getPointer devices ev
             -- print ev'
             oldState <- readIORef continuation
             newState <- resume oldState ev'
             -- print newState
             writeIORef continuation newState
           return True

     on canvas touchEvent $ do
       liftIO $ putStrLn "TOUCH"
       return True
     on canvas motionNotifyEvent handleEvent
     on canvas buttonPressEvent handleEvent

     on window deleteEvent $ return False
     on window destroyEvent (do liftIO mainQuit; return True)
     mainGUI
