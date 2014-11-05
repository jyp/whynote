{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GtkProcess where

import Process
import Graphics.UI.Gtk
import Control.Monad.Reader
import Control.Applicative
import Event

data Context
  = Context { ctxDrawWindow :: DrawWindow
            , ctxCanvas :: DrawingArea
            }

newtype GtkP a = GtkP {gtkP :: ReaderT Context (P Event) a}
  deriving (Monad, MonadIO, MonadReader Context, MonadProcess Event, Functor, Applicative)

runGtkP :: Context -> GtkP a -> Process Event
runGtkP ctx (GtkP p) = run (runReaderT p ctx)
