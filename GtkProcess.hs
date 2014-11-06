{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, RecordWildCards #-}
module GtkProcess where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Matrix
import Graphics.UI.Gtk

import Process hiding (wait)
import qualified Process
import Event
import NoteData

data Ctx
  = Ctx { _ctxDrawWindow :: DrawWindow
        , _ctxCanvas :: DrawingArea
        }

$(makeLenses ''Ctx)

type Translation = (Double,Double)

data St =
  St { _stRender :: Render ()
     , _stNoteData :: NoteData
     , _stTranslation :: Translation
     }

$(makeLenses ''St)

initSt :: St
initSt = St{_stRender = return ()
           ,_stNoteData = emptyNoteData
           ,_stTranslation = (0,0)
           }

newtype GtkP a = GtkP {gtkP :: ReaderT Ctx (P St Event) a}
  deriving (Monad, MonadIO, MonadReader Ctx, MonadProcess Event, Functor, Applicative, MonadState St)

runGtkP :: Ctx -> GtkP a -> Process St Event
runGtkP ctx (GtkP p) = run initSt (runReaderT p ctx)

makeTranslationMatrix :: (Double, Double) -> Matrix
makeTranslationMatrix (dx,dy) = Matrix 1 0 0 1 dx dy

-- apply the above matrix
screenCoords :: Coord -> GtkP (Int,Int)
screenCoords (Coord x y _ _) = do
  (dx,dy) <- use stTranslation
  return (round (dx + x), round (dy + y))

translateEvent :: Translation -> Event -> Event
translateEvent (dx,dy) Event {eventCoord = Coord{..},..} = Event{..}
  where eventCoord = Coord{coordX = coordX - dx, coordY =  coordY - dy,..}

waitInTrans tr msg = translateEvent tr <$> Process.wait msg

wait msg = do
  tr <- use stTranslation
  waitInTrans tr msg
