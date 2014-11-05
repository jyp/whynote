{-# LANGUAGE GADTs, RankNTypes #-}
module Process where

import Event
import Control.Monad.IO.Class

data Process where
  Wait :: String -> (Event -> Process) -> Process
  Do :: IO a -> (a -> Process) -> Process
  Done :: Process

instance Show Process where
  show Done = "<DONE>"
  show (Do _ _) = "<DO>"
  show (Wait s _) = "<WAIT " ++ s ++ ">"

newtype P x = P {fromP :: (x -> Process) -> Process}

instance MonadIO P where
  liftIO x = P (\k -> Do x k)

instance Monad P where
  return x = P (\k -> k x)
  P a >>= f = P (\k -> a (\a' -> fromP  (f a') k))

wait :: String -> P Event
wait s = P $ \k -> Wait s k

waitP :: String -> (Event -> Bool) -> P Event
waitP s p = do
  ev <- wait s
  if p ev
     then return ev
     else waitP s p

run :: P () -> Process
run (P x) = x (\_ -> Done)

exec :: Process -> IO Process
exec (Do x k) = do
  x' <- x
  exec (k x')
exec p = return p

resume :: Process -> Event -> IO Process
resume (Wait _ k) ev = exec (k ev)
resume done _ = return done
