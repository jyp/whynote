{-# LANGUAGE GADTs, RankNTypes, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances, FlexibleContexts #-}
module Process where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State

class Monad m => MonadProcess e m | m -> e where
  wait :: String -> (e -> Bool) -> m e
  pushBack :: e -> m ()
  forkHandler :: m () -> m ()

data Process s e where
  Wait :: s -> String -> (e -> Bool) -> (e -> Process s e) -> Process s e
  Do :: IO a -> (a -> Process s e) -> Process s e -- FIXME: do not allow arb. IO but only relevant functions
  Done :: s -> Process s e

(||>) :: Process s e -> Process s e -> Process s e
Do a k ||> p = Do a (\x -> k x ||> p)
p ||> Do a k = Do a (\x -> p ||> k x)
Done _ ||> p = p
_ ||> Done s = Done s
Wait s msg1 cond1 k1 ||> Wait _ msg2 cond2 k2
  = Wait s (msg1 ++ " | " ++ msg2) (\e -> cond1 e || cond2 e) $ \e ->
      if cond1 e then k1 e ||> Wait s msg2 cond2 k2
                 else Wait s msg1 cond1 k1 ||> k2 e

pushBackInternal :: e -> Process s e -> Process s e
pushBackInternal e k0 = case k0 of
  Done s -> Done s
  Wait _ _ c k | c e -> k e
               | otherwise -> error "unacceptable event"
  Do act k -> Do act (\a -> pushBackInternal e (k a))

instance Show (Process s e) where
  show (Done _) = "<DONE>"
  show (Do _ _) = "<DO>"
  show (Wait _ s _ _) = "<WAIT " ++ s ++ ">"

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
  wait msg cond = P $ \k s -> Wait s msg cond (flip k s)
  pushBack ev = P $ \k s -> pushBackInternal ev (k () s)
  forkHandler (P p) = P $ \k s -> p (\() -> Done) s ||> k () s

instance MonadProcess e m => MonadProcess e (ReaderT r m) where
  wait s cond = ReaderT (\_ -> wait s cond)
  pushBack ev = ReaderT $ \_ -> pushBack ev
  forkHandler (ReaderT a) = ReaderT (\x -> forkHandler (a x))

instance MonadState s (P s e) where
  get = P $ \k s -> k s s
  put s' = P $ \k _s -> k () s'

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
resume (Wait _ _ c k) ev | c ev = exec (k ev)
                         | otherwise = error "unacceptable event"
resume done _ = return done
