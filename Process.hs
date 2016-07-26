{-# LANGUAGE GADTs, RankNTypes, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
module Process where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State

class Monad m => MonadProcess e m | m -> e where
  wait :: String -> m e
  pushBack :: e -> m ()

data Process s e where
  Wait :: s -> String -> (e -> Process s e) -> Process s e
  Do :: IO a -> (a -> Process s e) -> Process s e
  Done :: s -> Process s e

pushBackInternal :: e -> Process s e -> Process s e
pushBackInternal e k0 = case k0 of
  Done s -> Done s
  Wait _ _ k -> k e
  Do act k -> Do act (\a -> pushBackInternal e (k a))

instance Show (Process s e) where
  show (Done _) = "<DONE>"
  show (Do _ _) = "<DO>"
  show (Wait _ s _) = "<WAIT " ++ s ++ ">"

-- unfortunately we cannot use the continuation monad from mtl... because of its
-- using transformers.
newtype Cont r x = P {fromP :: (x -> r) -> r}
type P s e = Cont (s -> Process s e)

instance MonadIO (P s e) where
  liftIO x = P (\k s -> Do x (flip k s))

instance Functor (Cont r) where
  fmap f (P x) = P $ \k -> x (k . f)

instance Applicative (Cont r) where
  pure = return
  (<*>) = ap

instance Monad (Cont r) where
  return x = P (\k -> k x)
  P a >>= f = P (\k -> a (\a' -> fromP  (f a') k))

instance MonadProcess e (P s e)  where
  wait msg = P $ \k s -> Wait s msg (flip k s)
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

processDone :: P s e x
processDone = P $ \_ s -> Done s

run :: s -> P s e a -> Process s e
run s0 (P p) = p (\_ s -> Done s) s0

exec :: Process s e -> IO (Process s e)
exec (Do x k) = do
  x' <- x
  exec (k x')
exec p = return p

resume :: Process s e -> e -> IO (Process s e)
resume (Wait _ _ k) ev = exec (k ev)
resume done _ = return done
