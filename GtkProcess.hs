{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, RecordWildCards #-}
module GtkProcess where

import qualified Prelude
import WNPrelude
import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Matrix
import qualified Graphics.UI.Gtk as Gtk

import Process hiding (wait)
import qualified Process
import Event
import NoteData
import Render

data Ctx
  = Ctx { _ctxDrawWindow :: Gtk.DrawWindow
        , _ctxCanvas :: Gtk.DrawingArea
        }

$(makeLenses ''Ctx)


data St =
  St { _stRender :: Render ()
     , _stNoteData :: NoteData
     , _stSelection :: Selection
     , _stTranslation :: Translation
     }

$(makeLenses ''St)

initSt :: St
initSt = St{_stRender = return ()
           ,_stNoteData = emptyNoteData
           ,_stSelection = emptySelection
           ,_stTranslation = Translation 1 0 0
           }

newtype GtkP a = GtkP {gtkP :: ReaderT Ctx (P St Event) a}
  deriving (Monad, MonadIO, MonadReader Ctx, MonadProcess Event, Functor, Applicative, MonadState St)

runGtkP :: Ctx -> GtkP a -> Process St Event
runGtkP ctx (GtkP p) = run initSt (runReaderT p ctx)

makeTranslationMatrix :: Translation -> Matrix
makeTranslationMatrix (Translation z dx dy) = Matrix z 0 0 z dx dy

-- apply the above matrix
screenCoords :: Coord -> GtkP (Int,Int)
screenCoords (Coord x y _ _) = do
  Translation z dx dy <- use stTranslation
  return (round (dx + z*x), round (dy + z*y))

translateEvent :: Translation -> Event -> Event
translateEvent (Translation z dx dy) Event {eventCoord = Coord{..},..} = Event{..}
  where eventCoord = Coord{coordX = (coordX - dx)/z, coordY =  (coordY - dy)/z,..}

waitInTrans tr msg = translateEvent tr <$> Process.wait msg

wait msg = do
  tr <- use stTranslation
  waitInTrans tr msg

renderAll St{..} msg = do
   moveTo 0 10
   showText $ msg
   setMatrix $ makeTranslationMatrix _stTranslation
   _stRender
   renderNoteData _stNoteData
   renderSelection _stSelection
