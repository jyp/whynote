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
import Data.Word
import Event
import Data.Time.LocalTime
import System.Locale (defaultTimeLocale)
import Data.Time.Format
import System.Mem
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
       _ -> error "Give 0 or 1 argument"

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
     let ctx = Ctx drawin canvas
     setup <- exec $ runGtkP ctx (initSt initData) mainProcess
     continuation <- newIORef setup

     widgetAddEvents canvas [PointerMotionMask, TouchMask]

     on canvas draw $ liftIO $ do
         k <- readIORef continuation
         case k of
           Wait st msg _ -> renderWithDrawWindow drawin $ renderAll st msg
           _ -> return ()

     on canvas keyPressEvent $ liftIO $ do
       cont <- readIORef continuation
       putStrLn $ "Current state: " ++ show cont
       return False

     nextSaveTime <- newIORef (0 :: TimeStamp)
     lastStylusTime <- newIORef (0 :: TimeStamp)
     let handleEvent :: EventM t Bool
         handleEvent = do
           ev <- ask
           liftIO $ do
             ev' <- getPointer devices ev
             let t = coordT . eventCoord $ ev'
             -- Touch rejection when stylus/eraser is active
             when (eventSource ev' `elem` [Stylus,Eraser]) $ do
               writeIORef lastStylusTime t
             time0 <- readIORef lastStylusTime
             unless (eventSource ev' `elem` [Touch,MultiTouch]
                     && t - time0 < 150) $ do
               oldState <- readIORef continuation
               case oldState of
                 Done s -> do
                   writeState fname s
                   mainQuit
                 Wait s _ k -> do
                   nextSave <- readIORef nextSaveTime
                   -- Save the file every second
                   when (t > nextSave) $ do
                      forkIO $ writeState fname s
                      writeIORef nextSaveTime (t + 5000)
                   newState <- resume oldState ev'
                   -- print newState
                   writeIORef continuation newState
                   -- System.Mem.performGC
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
