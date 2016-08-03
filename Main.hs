{-# LANGUAGE RecordWildCards #-}
import Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo
import Device
import NoteData
import Control.Monad.Reader
import Config
import Process
import GtkProcess
import App
import Data.IORef
import Event
import Data.Time.LocalTime
import Data.Time.Format
import Control.Concurrent (forkIO)


main :: IO ()
main = do
     args <- initGUI
     (initData,fname) <- case args of
       [] -> do
         time <- getZonedTime
         return $ (emptyNoteData,(formatTime defaultTimeLocale "%y%m%d-%H%M" time) ++ ".wnote")
       [fname] -> do
         dat <- loadState fname
         return (dat,fname)
       _ -> error "usage: whynote [file.wnote]"

     WNConfig devicesCfg <- loadConfig
     window <- windowNew
     set window [windowTitle := "WhyNote",
                 windowDefaultWidth := 1000, windowDefaultHeight := 600]

     canvas <- drawingAreaNew
     set canvas [widgetCanFocus := True]
     Gtk.widgetGrabFocus canvas
     devices <- initDevice (castToWidget canvas) devicesCfg
     print devices
     containerAdd window canvas
     widgetModifyBg canvas StateNormal (Gtk.Color 65535 65535 65535)

     widgetShowAll window
     Just drawin <- widgetGetWindow canvas
     setEventCompression drawin False
     continuation <- newIORef (error "DO NOT ACCESS INITIAL CONTI")
     let pushEvent ev = do
           oldState <- readIORef continuation
           newState <- resume oldState ev
           writeIORef continuation newState
         ctx = Ctx drawin canvas pushEvent
     setup <- exec (runGtkP ctx (initSt initData) mainProcess)
     writeIORef continuation setup
     staleSt <- newIORef initStaleSt

     widgetAddEvents canvas [PointerMotionMask, TouchMask]

     on canvas draw $ liftIO $ do
       k <- readIORef continuation
       renderWithDrawWindow drawin $ renderAll k

     let debugState = do
           cont <- readIORef continuation
           putStrLn $ "Current state: " ++ show cont

     on canvas keyPressEvent $ liftIO $ do
       debugState
       return False


     nextSaveTime <- newIORef (0 :: TimeStamp)
     let handleEvent :: EventM t Bool
         handleEvent = do
           ev <- ask
           liftIO $ do
             ev' <- getPointer devices ev
             let t = Event.eventTime ev'
             oldStaleSt <- readIORef staleSt
             let (newStaleSt,freshEvent) = computeStaleTouches ev' oldStaleSt
             writeIORef staleSt newStaleSt
             oldState <- readIORef continuation
             case oldState of
               Done s -> do
                 writeState fname s
                 mainQuit
               Wait s msg _ -> do
                 putStrLn msg
                 nextSave <- readIORef nextSaveTime
                 -- Save the file every second
                 when (t > nextSave) $ do
                    _ <- forkIO $ writeState fname s -- FIXME: have a thread in charge of disk.
                    writeIORef nextSaveTime (t + 5000)
                 when freshEvent (pushEvent ev')
               _ -> error ("Main: did not expect state: " ++ show oldState)
           return True

     on canvas touchEvent handleEvent
     on canvas motionNotifyEvent handleEvent
     on canvas buttonPressEvent handleEvent
     on window objectDestroy $ do
       oldState <- readIORef continuation
       case oldState of
         Wait s _ _ -> writeState fname s
         _ -> return ()
       mainQuit
     mainGUI
