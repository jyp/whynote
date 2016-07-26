{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, TemplateHaskell, RecordWildCards, GADTs, FlexibleContexts #-}
module GtkProcess where

import qualified Prelude
import WNPrelude
import Control.Applicative
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

newtype GtkP a = GtkP {gtkP :: ReaderT Ctx (P St Event) a}
  deriving (Monad, MonadIO, MonadReader Ctx, MonadProcess Event, Functor, Applicative, MonadState St)

runGtkP :: Ctx -> St -> GtkP a -> Process St Event
runGtkP ctx st (GtkP p) = run st (runReaderT p ctx)

-- apply the above matrix
screenCoords :: Coord -> GtkP (Int,Int)
screenCoords (Coord x y _ _) = do
  Translation z dx dy <- use stTranslation
  return (round (dx + z*x), round (dy + z*y))

translateEvent :: Translation -> Event -> Event
translateEvent (Translation z dx dy) Event {eventCoord = Coord{..},..} = Event{..}
  where eventCoord = Coord{coordX = f*(coordX - dx), coordY =  f*(coordY - dy),..}
        f = 1/z

-- rectToBox :: Translation -> Gtk.Rectangle -> Box
-- rectToBox (Translation z dx dy) (Gtk.Rectangle x y w h) = Box (Coord (tx x) (ty y) 1 0) (Coord (tx (x+w)) (ty (y+h)) 1 0)
--   where tx a = dx + f*fromIntegral a
--         ty a = dy + f*fromIntegral a
--         f = 1/z

quit :: GtkP ()
quit = do
  GtkP $ lift $ processDone

waitInTrans tr msg = translateEvent tr <$> Process.wait msg

wait msg = do
  tr <- use stTranslation
  waitInTrans tr msg

renderAll St{..} msg = do
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
