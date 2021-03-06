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
import Config

$(makeLenses ''PenOptions')
$(makeLenses ''Dilation)

data Ctx
  = Ctx { _ctxDrawWindow :: Gtk.DrawWindow
        , _ctxCanvas :: Gtk.DrawingArea
        , _eventHandler :: Event -> IO ()
        }

$(makeLenses ''Ctx)

data SoftButton = Erase | Lasso | Select deriving (Eq,Show)
data St =
  St { _stGui  :: ![GuiElement] -- ^ extra stuff to render
     , _stNoteData    :: !NoteData
     , _stRedo        :: ![Stroke]
     , _stSelection   :: !Selection
     , _stDilation :: !Dilation -- currently applied transformation of the logical canvas
     , _stPen         :: !PenOptions -- current pen
     , _stButtons     :: ![SoftButton]
     }

newtype GtkP a = GtkP {gtkP :: ReaderT Ctx (P St Event) a}
  deriving (Monad, MonadIO, MonadReader Ctx, MonadProcess Event, Functor, Applicative, MonadState St)

$(makeLenses ''St)

initSt :: NoteData -> St
initSt dat = St{_stGui = []
               ,_stNoteData = dat
               ,_stSelection = emptySelection
               ,_stDilation = zero
               ,_stRedo = []
               ,_stPen = defaultPen
               ,_stButtons = []
               }

-- | Touches occuring after a stylus event are bogus and should be discarded
-- ('palm rejection').
palmRejection :: Int -> Int -> GtkP ()
palmRejection lastTouch lastStaleTouch = do
  e@Event{..} <- waitInTrans zero "stylus reject touch"
  case () of
    _ | eventSource `elem` [Stylus,Eraser] -> do
          reject e -- note: rejected by rejecton process means available for the rest
          palmRejection lastTouch lastTouch
    _ | eventSource == MultiTouch -> do
          when (eventButton > lastStaleTouch) (reject e)
          palmRejection (max eventButton lastTouch) lastStaleTouch
    _ -> reject e >> palmRejection lastTouch lastStaleTouch


runGtkP :: Ctx -> GtkP a -> Process St Event
runGtkP ctx (GtkP p) = run (runReaderT p ctx)

quit :: GtkP ()
quit = GtkP $ lift $ processDone

invalidateAll :: GtkP ()
invalidateAll = do
  Ctx {..} <- ask
  liftIO $ do
    w <- liftIO $ Gtk.drawWindowGetWidth _ctxDrawWindow
    h <- liftIO $ Gtk.drawWindowGetHeight _ctxDrawWindow
    Gtk.drawWindowInvalidateRect _ctxDrawWindow (Gtk.Rectangle 0 0 w h) False

invalidate :: Box -> GtkP ()
invalidate b0 = do
  let Box (x0,y0) (x1,y1) = fmap (over both round . coordToPt) $ extend 5 $ b0
  Ctx {..} <- ask
  let rect = (Gtk.Rectangle x0 y0 (x1-x0) (y1-y0))
  liftIO $ Gtk.drawWindowInvalidateRect _ctxDrawWindow rect False

wait :: String -> GtkP Event
wait msg = do
  tr <- use stDilation
  waitInTrans tr msg

waitInTrans :: Dilation -> String -> GtkP Event
waitInTrans tr msg = (apply (negate tr) <$>) <$> Process.wait msg (const True)

waitOn :: String -> (Event -> Bool) -> GtkP Event
waitOn = Process.wait

-- | Send a wakeup event in the given about of time. The timestamp of the wakeup
-- event is based on the timestamp of the input event.
wakeup :: Event -> Word32 -> GtkP ()
wakeup ev msec = do
  handleEvent <- view eventHandler
  _ <- liftIO $ flip Gtk.timeoutAdd (fromIntegral msec) $ do
    handleEvent Event {eventSample = (eventSample ev) {sampleT = eventTime ev + fromIntegral (msec)},
                       eventSource = Timeout,
                       eventButton = 0,
                       eventType = End,
                       eventModifiers = 0}
    return False
  return ()

writeState :: String -> St -> IO ()
writeState fname St {..} = writeNote fname (NoteFile _stNoteData)

loadState :: String -> IO NoteData
loadState fname = do
  NoteFile s <- readNote fname
  return s

setGui :: [GuiElement] -> GtkP ()
setGui new = do
  old <- use stGui
  stGui .= new
  invalidate (boundingBox old \/ boundingBox new)

testMenu (Menu a0 p c opts) = do
  dw <- view ctxDrawWindow
  liftIO $ Gtk.renderWithDrawWindow dw $ renderMenu False a0 p c opts

getWinSize :: Ctx -> IO (Int,Int)
getWinSize Ctx {..} = (,) <$> (Gtk.drawWindowGetWidth _ctxDrawWindow) <*> (Gtk.drawWindowGetHeight _ctxDrawWindow)

loop :: Monad m => m a -> m b
loop x = x >> loop x
