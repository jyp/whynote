{-# LANGUAGE RecordWildCards #-}
import Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo
import Device
import Control.Monad.Reader
import Config
import Process
import GtkProcess
import App
import Data.IORef
import Data.Time.LocalTime
import Data.Time.Format
import Control.Concurrent

-- | Dedicated process to constently autosave
saveHandler :: String -> MVar (St,Maybe (MVar ())) -> IO a
saveHandler fname v = do
  sem <- newEmptyMVar --ask to be woken up
  modifyMVar_ v $ \(st,Nothing) -> return (st,Just sem)
  readMVar sem -- wait for data to save
  s <- withMVar v (return . fst) -- get the data
  writeState fname s
  threadDelay 5000 -- wait some time so we're not bogging the disk
  saveHandler fname v

main :: IO ()
main = do
     args <- initGUI
     (initData,fname) <- case args of
       [] -> do
         time <- getZonedTime
         return ([],(formatTime defaultTimeLocale "%y%m%d-%H%M" time) ++ ".ynote")
       [fname] -> do
         dat <- loadState fname
         return (dat,fname)
       _ -> error "usage: whynote [file.ynote]"

     saveChan <- newMVar (error "saveChan: no value yet",Nothing)
     saveProcess <- forkIO (saveHandler fname saveChan)
     let killSave s = killThread saveProcess >> writeState fname s

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
     widgetModifyBg canvas StateNormal (Gtk.Color 65535 65535 65535) -- FIXME: does not appear to work, the background is grey

     widgetShowAll window
     Just drawin <- widgetGetWindow canvas
     setEventCompression drawin False
     continuation <- newIORef (error "DO NOT ACCESS INITIAL CONTI")
     let pushEvent ev = do
           oldState <- readIORef continuation
           (s,newCont) <- resume oldState ev
           case newCont of
             Done -> killSave s >> mainQuit
             _ -> do sem <- modifyMVar saveChan (\(_,sem) -> return ((s,Nothing),sem))
                     forM_ sem $ \sm -> putMVar sm () -- wakeup the writer if necessary
                     writeIORef continuation (s,newCont)
         ctx = Ctx drawin canvas pushEvent
     setup <- exec (initSt initData,runGtkP ctx whynote)
     writeIORef continuation setup

     widgetAddEvents canvas [PointerMotionMask, TouchMask]

     on canvas draw $ liftIO $ do
       renderWithDrawWindow drawin . renderAll . fst =<< readIORef continuation
     let debugState = do
           (_,cont) <- readIORef continuation
           putStrLn $ "Current state: " ++ show cont

     on canvas keyPressEvent $ liftIO $ do
       debugState
       return False
     let handleEvent :: EventM t Bool
         handleEvent = do
           ev <- ask
           liftIO $ do
             ev' <- getPointer devices ev
             pushEvent ev'
           return True

     on canvas touchEvent handleEvent
     on canvas motionNotifyEvent handleEvent
     on canvas buttonPressEvent handleEvent
     on window objectDestroy $ do
       (s,_) <- readIORef continuation
       killSave s
       mainQuit
     mainGUI
