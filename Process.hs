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
  Wait :: String -> (e -> Bool) -> (e -> Process s e) -> Process s e
  Do :: IO a -> (a -> Process s e) -> Process s e -- FIXME: do not allow arb. IO but only relevant functions
  PushBack :: e -> Process s e -> Process s e
  Done :: Process s e
  Get :: (s -> Process s e) -> Process s e
  Set :: s -> Process s e -> Process s e

(||>) :: Process s e -> Process s e -> Process s e
PushBack e k ||> p = k ||> feed e p
p ||> PushBack e q = p ||> feed e q
Get k ||> p = Get $ \x -> k x ||> p
Set s p ||> q = Set s (p ||> q)
p ||> Get k = Get $ \x -> p ||> k x
p ||> Set s q = Set s (p ||> q)
Do a k ||> p = Do a (\x -> k x ||> p)
p ||> Do a k = Do a (\x -> p ||> k x)
Done ||> p = p
_ ||> Done = Done
Wait msg1 cond1 k1 ||> Wait msg2 cond2 k2
  = Wait (msg1 ++ " | " ++ msg2) (\e -> cond1 e || cond2 e) $ \e ->
      if cond1 e then k1 e ||> Wait msg2 cond2 k2
                 else Wait msg1 cond1 k1 ||> k2 e

feed :: e -> Process s e -> Process s e
feed e k0 = case k0 of
  Get k -> Get $ \s -> feed e (k s)
  Set s k -> Set s $ feed e k
  Done -> Done
  PushBack _ _ -> error "Too many events accumulated."
  Wait _ c k | c e -> k e
             | otherwise -> error "unacceptable event"
  Do act k -> Do act (\a -> feed e (k a))

instance Show (Process s e) where
  show (Wait s _ _) = "<WAIT " ++ s ++ ">"
  show _ = "<P>"

-- unfortunately we cannot use the continuation monad from mtl... because it's
-- using transformers.
newtype Cont r x = P {fromP :: (x -> r) -> r}
type P s e = Cont (Process s e)

instance MonadIO (P s e) where
  liftIO x = P (Do x)

instance Functor (Cont r) where
  fmap f (P x) = P $ \k -> x (k . f)

instance Applicative (Cont r) where
  pure = return
  (<*>) = ap

instance Monad (Cont r) where
  return x = P (\k -> k x)
  P a >>= f = P (\k -> a (\a' -> fromP  (f a') k))

instance MonadProcess e (P s e)  where
  wait msg cond = P $ Wait msg cond
  pushBack ev = P $ \k -> PushBack ev (k ())
  forkHandler (P p) = P $ \k -> p (\() -> Done) ||> k ()

instance MonadProcess e m => MonadProcess e (ReaderT r m) where
  wait s cond = ReaderT (\_ -> wait s cond)
  pushBack ev = ReaderT (\_ -> pushBack ev)
  forkHandler (ReaderT a) = ReaderT (\x -> forkHandler (a x))

instance MonadState s (P s e) where
  get = P $ \k -> Get k
  put s' = P $ \k -> Set s' (k ())

processDone :: P s e x
processDone = P $ \_ -> Done

run :: P s e a -> Process s e
run (P p) = p (\_ -> Done)

exec :: (s,Process s e) -> IO (s,Process s e)
exec (s,PushBack e k) = exec (s,feed e k)
exec (s,Do x k) = do
  x' <- x
  exec (s,k x')
exec (_,Set s p) = exec (s,p)
exec (s,Get k) = exec (s,k s)
exec p = return p

resume :: (s,Process s e) -> e -> IO (s,Process s e)
resume (s,p) ev = exec (s,feed ev p)
