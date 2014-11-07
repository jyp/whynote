{-# LANGUAGE GADTs, RankNTypes, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
module Process where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative

class Monad m => MonadProcess e m | m -> e where
  wait :: String -> m e
  pushBack :: e -> m ()
  
data Process s e where
  Wait :: s -> String -> (s -> e -> Process s e) -> Process s e
  Do :: IO a -> (a -> Process s e) -> Process s e
  Done :: Process s e

pushBackInternal :: e -> Process s e -> Process s e
pushBackInternal e k0 = case k0 of
  Done -> Done
  Wait s _ k -> k s e
  Do act k -> Do act (\a -> pushBackInternal e (k a))

instance Show (Process s e) where
  show Done = "<DONE>"
  show (Do _ _) = "<DO>"
  show (Wait _ s _) = "<WAIT " ++ s ++ ">"

newtype P s e x = P {fromP :: (x -> s -> Process s e) -> s -> Process s e}

instance MonadIO (P s e) where
  liftIO x = P (\k s -> Do x (flip k s))

instance Functor (P s e) where
  fmap f (P x) = P $ \k -> x (k . f)

instance Applicative (P s e) where
  pure = return
  (<*>) = ap
  
instance Monad (P s e) where
  return x = P (\k -> k x)
  P a >>= f = P (\k -> a (\a' -> fromP  (f a') k))

instance MonadProcess e (P s e)  where
  wait msg = P $ \k s -> Wait s msg (flip k)
  pushBack ev = P $ \k s -> pushBackInternal ev (k () s)

instance MonadProcess e m => MonadProcess e (ReaderT r m) where
  wait s = ReaderT (\_ -> wait s)
  pushBack ev = ReaderT $ \_ -> pushBack ev

instance MonadState s (P s e) where
  get = P $ \k s -> k s s
  put s' = P $ \k _s -> k () s'

waitP :: MonadProcess b m => String -> (b -> Bool) -> m b
waitP s p = do
  ev <- wait s
  if p ev
     then return ev
     else waitP s p

run :: s -> P s e a -> Process s e
run s0 (P x) = x (\_ _ -> Done) s0

exec :: Process s e -> IO (Process s e)
exec (Do x k) = do
  x' <- x
  exec (k x')
exec p = return p

resume :: Process s e -> e -> IO (Process s e)
resume (Wait s _ k) ev = exec (k s ev)
resume done _ = return done



