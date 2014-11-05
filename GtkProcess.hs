{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GtkProcess where

import Process
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Control.Monad.Reader
import Control.Applicative
import Event
import Data.IORef

data Context
  = Context { ctxDrawWindow :: DrawWindow
            , ctxCanvas :: DrawingArea
            , ctxRender :: IORef (Render ())
            }

newtype GtkP a = GtkP {gtkP :: ReaderT Context (P Event) a}
  deriving (Monad, MonadIO, MonadReader Context, MonadProcess Event, Functor, Applicative)

runGtkP :: Context -> GtkP a -> Process Event
runGtkP ctx (GtkP p) = run (runReaderT p ctx)
