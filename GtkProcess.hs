{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module GtkProcess where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk

import Process
import Event
import NoteData

data Ctx
  = Ctx { _ctxDrawWindow :: DrawWindow
            , _ctxCanvas :: DrawingArea
            }

$(makeLenses ''Ctx)

data St =
  St { _stRender :: Render ()
     , _stNoteData :: NoteData
     }

$(makeLenses ''St)

initSt :: St
initSt = St{_stRender = return ()
           ,_stNoteData = emptyNoteData
           }

newtype GtkP a = GtkP {gtkP :: ReaderT Ctx (P St Event) a}
  deriving (Monad, MonadIO, MonadReader Ctx, MonadProcess Event, Functor, Applicative, MonadState St)

runGtkP :: Ctx -> GtkP a -> Process St Event
runGtkP ctx (GtkP p) = run initSt (runReaderT p ctx)
