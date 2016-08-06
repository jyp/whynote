{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, TemplateHaskell, RecordWildCards, GADTs, FlexibleContexts, ScopedTypeVariables, DeriveFunctor #-}
module GtkProcess where

import Prelude ()
import WNPrelude
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Graphics.Rendering.Cairo
import qualified Graphics.UI.Gtk as Gtk

import Process hiding (wait)
import qualified Process
import Event
import NoteData
import Render
import File

$(makeLenses ''PenOptions)

data Ctx
  = Ctx { _ctxDrawWindow :: Gtk.DrawWindow
        , _ctxCanvas :: Gtk.DrawingArea
        , _eventHandler :: Event -> IO ()
        }

$(makeLenses ''Ctx)
$(makeLenses ''Translation)

data SoftButton = Erase
data St =
  St { _stRender      :: Render () -- ^ extra stuff to render
     , _stNoteData    :: NoteData
     , _stRedo        :: [Stroke]
     , _stSelection   :: Selection
     , _stTranslation :: Translation -- currently applied transformation of the logical canvas
     , _stPen         :: PenOptions -- current pen
     , _stButtons     :: [SoftButton]
     }

newtype GtkP a = GtkP {gtkP :: ReaderT Ctx (P St Event) a}
  deriving (Monad, MonadIO, MonadReader Ctx, MonadProcess Event, Functor, Applicative, MonadState St)

$(makeLenses ''St)

initSt :: NoteData -> St
initSt dat = St{_stRender = return ()
               ,_stNoteData = dat
               ,_stSelection = emptySelection
               ,_stTranslation = Translation 1 0 0
               ,_stRedo = []
               ,_stPen = defaultPen
               }

-- | Touches occuring after a stylus event are bogus and should be discarded
-- ('palm rejection').
palmRejection :: Int -> Int -> GtkP ()
palmRejection lastTouch lastStaleTouch = do
  e@Event{..} <- waitInTrans zero "stylus reject touch"
  case () of
    _ | eventSource `elem` [Stylus,Eraser] -> do
          Process.pushBack e
          palmRejection lastTouch lastTouch
    _ | eventSource == MultiTouch -> do
          when (eventButton > lastStaleTouch) (Process.pushBack e)
          palmRejection (max eventButton lastTouch) lastStaleTouch
    _ -> Process.pushBack e >> palmRejection lastTouch lastStaleTouch


runGtkP :: Ctx -> GtkP a -> Process St Event
runGtkP ctx (GtkP p) = run (runReaderT p ctx)

translateEvent :: Translation -> Event -> Event
translateEvent tr Event {..} = Event{eventCoord = apply tr eventCoord,..}

quit :: GtkP ()
quit = GtkP $ lift $ processDone

invalidateAll :: GtkP ()
invalidateAll = do
  Ctx {..} <- ask
  liftIO $ do
    w <- liftIO $ Gtk.drawWindowGetWidth _ctxDrawWindow
    h <- liftIO $ Gtk.drawWindowGetHeight _ctxDrawWindow
    Gtk.drawWindowInvalidateRect _ctxDrawWindow (Gtk.Rectangle 0 0 w h) False

invalidateIn :: Translation -> Box -> GtkP ()
invalidateIn tr b0 = do
  let Box (x0,y0) (x1,y1) = fmap (over both round . coordToPt) $ extend 5 $ fmap (apply tr) b0
  Ctx {..} <- ask
  let rect = (Gtk.Rectangle x0 y0 (x1-x0) (y1-y0))
  liftIO $ Gtk.drawWindowInvalidateRect _ctxDrawWindow rect False

invalidate :: Box -> GtkP ()
invalidate bx = do
  tr <- use stTranslation
  invalidateIn tr bx

wait :: String -> GtkP Event
wait msg = do
  tr <- use stTranslation
  waitInTrans tr msg

waitInTrans :: Translation -> String -> GtkP Event
waitInTrans tr msg = translateEvent (negate tr) <$> Process.wait msg (const True)

waitOn :: String -> (Event -> Bool) -> GtkP Event
waitOn = Process.wait

-- | Send a wakeup event in the given about of time. The timestamp of the wakeup
-- event is based on the timestamp of the input event.
wakeup :: Event -> Word32 -> GtkP ()
wakeup ev msec = do
  handleEvent <- view eventHandler
  _ <- liftIO $ flip Gtk.timeoutAdd (fromIntegral msec) $ do
    handleEvent Event {eventCoord = (eventCoord ev) {coordT = eventTime ev + fromIntegral (msec)},
                       eventSource = Timeout,
                       eventButton = 0,
                       eventType = End,
                       eventModifiers = 0}
    return False
  return ()

pushBack :: Translation -> Event -> GtkP ()
pushBack tr ev = Process.pushBack (translateEvent tr ev)

writeState :: String -> St -> IO ()
writeState fname St {..} = writeNote fname (NoteFile _stNoteData)

loadState :: String -> IO NoteData
loadState fname = do
  NoteFile s <- readNote fname
  return s

renderNow :: Render a -> GtkP a
renderNow r = do
  dw <- view ctxDrawWindow
  liftIO $ Gtk.renderWithDrawWindow dw r

getWinSize :: Ctx -> IO (Int,Int)
getWinSize Ctx {..} = (,) <$> (Gtk.drawWindowGetWidth _ctxDrawWindow) <*> (Gtk.drawWindowGetHeight _ctxDrawWindow)
