{-# LANGUAGE GADTs, RankNTypes, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
module Process where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Applicative

class Monad m => MonadProcess e m | m -> e where
  wait :: String -> m e
  
  
data Process e where
  Wait :: String -> (e -> Process e) -> Process e
  Do :: IO a -> (a -> Process e) -> Process e
  Done :: Process e

instance Show (Process e) where
  show Done = "<DONE>"
  show (Do _ _) = "<DO>"
  show (Wait s _) = "<WAIT " ++ s ++ ">"

newtype P e x = P {fromP :: (x -> Process e) -> Process e}

instance MonadIO (P e) where
  liftIO x = P (\k -> Do x k)

-- Goal : Process e
-- k : (b -> Process e)
-- f : a -> b
-- fromP x : (a -> Process e) -> Process e
instance Functor (P e) where
  fmap f (P x) = P $ \k -> x (k . f)

instance Applicative (P e) where
  pure = return
  (<*>) = ap
  
instance Monad (P e) where
  return x = P (\k -> k x)
  P a >>= f = P (\k -> a (\a' -> fromP  (f a') k))

instance MonadProcess e (P e)  where
  wait s = P $ \k -> Wait s k

instance MonadProcess e m => MonadProcess e (ReaderT r m) where
  wait s = ReaderT (\_ -> wait s)

waitP :: MonadProcess b m => String -> (b -> Bool) -> m b
waitP s p = do
  ev <- wait s
  if p ev
     then return ev
     else waitP s p


run :: P e a -> Process e
run (P x) = x (\_ -> Done)

exec :: Process t -> IO (Process t)
exec (Do x k) = do
  x' <- x
  exec (k x')
exec p = return p

resume :: Process t -> t -> IO (Process t)
resume (Wait _ k) ev = exec (k ev)
resume done _ = return done
