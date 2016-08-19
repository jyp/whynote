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
saveTicker ::  MVar (Maybe b)-> IO a
saveTicker ck = loop $ do
  threadDelay 5000000 -- wait some time so we're not bogging the disk (μsec)
  putMVar ck Nothing

saveHandler :: String -> MVar (Maybe (MVar ())) -> MVar (Maybe St) -> IO ()
saveHandler fname ck st = do
  stop <- takeMVar ck -- wait for data to save
  s <- withMVar st return  -- get the data
  case s of Just x -> writeState fname x; _ -> return ()
  case stop of
    Nothing -> saveHandler fname ck st
    Just done -> putMVar done ()

main :: IO ()
main = do
     args <- initGUI
     (initData,fname) <- case args of
       [] -> do
         time <- getZonedTime
         return ([],(formatTime defaultTimeLocale "%y%m%d-%H%M" time) ++ ".y0")
       [fname] -> do
         dat <- loadState fname
         return (dat,fname)
       _ -> error "usage: whynote [file.y0]"

     saveChan <- newMVar Nothing
     ck <- newEmptyMVar
     _ <- forkIO (saveHandler fname ck saveChan)
     tickProcess <- forkIO (saveTicker ck)
     let setState s = modifyMVar_ saveChan (\_ -> return (Just s))
         killSave s = do killThread tickProcess
                         setState s
                         done <- newEmptyMVar
                         putMVar ck (Just done)
                         takeMVar done

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
             _ -> do setState s
                     writeIORef continuation (s,newCont)
         ctx = Ctx drawin canvas pushEvent
     setup <- exec (initSt initData,runGtkP ctx whynote)
     writeIORef continuation setup

     widgetAddEvents canvas [PointerMotionMask, TouchMask]

     on canvas draw $ liftIO $ do
       renderWithDrawWindow drawin . renderAll ctx . fst =<< readIORef continuation
     let debugState = do
           (_,cont) <- readIORef continuation
           putStrLn $ "Current state: " ++ show cont

     on canvas keyPressEvent $ liftIO $ do
       debugState
       return False
     let handleEvent :: EventM t Bool
         handleEvent = do
           ev <- ask
           liftIO (pushEvent =<< getPointer devices ev)
           return True

     on canvas touchEvent handleEvent
     on canvas motionNotifyEvent handleEvent
     on canvas buttonPressEvent handleEvent
     on window objectDestroy $ do
       (s,_) <- readIORef continuation
       killSave s
       mainQuit
     mainGUI
