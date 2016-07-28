{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, TemplateHaskell, RecordWildCards, GADTs, FlexibleContexts #-}
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
        }

$(makeLenses ''Ctx)
$(makeLenses ''Translation)


data St =
  St { _stRender      :: Render ()
     , _stNoteData    :: NoteData
     , _stRedo        :: [Stroke]
     , _stSelection   :: Selection
     , _stTranslation :: Translation -- currently applied transformation of the logical canvas
     , _stPen         :: PenOptions -- current pen
     }

$(makeLenses ''St)

initSt :: NoteData -> St
initSt dat =
  St{_stRender = return ()
    ,_stNoteData = dat
    ,_stSelection = emptySelection
    ,_stTranslation = Translation 1 0 0
    ,_stRedo = []
    ,_stPen = defaultPen
    }

data StaleSt = StaleSt
 { _stLastTouch   :: Int        -- button number of the latest finger touch
 , _stLastStaleTouch :: Int -- button number of the latest finger touch when stylus was used
 }

-- | Given event and state, compute if a touch event should be kept, and update relevant bookkeeping info.
computeStaleTouches :: Event -> StaleSt -> (StaleSt,Bool)
computeStaleTouches Event {..} StaleSt {..}
  | eventSource `elem` [Stylus,Eraser] = (StaleSt {_stLastStaleTouch = _stLastTouch,..},True)
  | eventSource == MultiTouch = (StaleSt{_stLastTouch = max eventButton _stLastTouch,..},eventButton > _stLastStaleTouch)
  | otherwise = (StaleSt{..},True)

initStaleSt :: StaleSt
initStaleSt = StaleSt {_stLastTouch = -1 ,_stLastStaleTouch = -1}


newtype GtkP a = GtkP {gtkP :: ReaderT Ctx (P St Event) a}
  deriving (Monad, MonadIO, MonadReader Ctx, MonadProcess Event, Functor, Applicative, MonadState St)

runGtkP :: Ctx -> St -> GtkP a -> Process St Event
runGtkP ctx st (GtkP p) = run st (runReaderT p ctx)

-- | apply the translation matrix
screenCoords :: Coord -> GtkP (Int,Int)
screenCoords (Coord x y _ _) = do
  Translation z dx dy <- use stTranslation
  return (round (dx + z*x), round (dy + z*y))

translateEvent :: Translation -> Event -> Event
translateEvent tr Event {..} = Event{eventCoord = apply tr eventCoord,..}

quit :: GtkP ()
quit = do
  GtkP $ lift $ processDone

waitInTrans :: Translation -> String -> GtkP Event
waitInTrans tr msg = translateEvent (negate tr) <$> Process.wait msg

wait :: String -> GtkP Event
wait msg = do
  tr <- use stTranslation
  waitInTrans tr msg

pushBack :: Translation -> Event -> GtkP ()
pushBack tr ev = Process.pushBack (translateEvent tr ev)

renderAll :: St -> String -> Render ()
renderAll St{..} _msg = do
   -- moveTo 0 10
   -- showText $ msg
   resetMatrix  _stTranslation
   renderNoteData _stNoteData
   renderSelection _stSelection
   _stRender

writeState :: String -> St -> IO ()
writeState fname St {..} = writeNote fname (NoteFile _stNoteData)

loadState :: String -> IO NoteData
loadState fname = do
  NoteFile s <- readNote fname
  return s
