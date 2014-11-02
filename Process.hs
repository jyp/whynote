{-# LANGUAGE GADTs #-}
module Process where

import Foreign

data Process x where
  Wait :: String -> (Ptr t -> Process x) -> Process x
  Do :: IO a -> (a -> Process x) -> Process x
  Done :: x -> Process x

instance Monad Process where
  return = Done
  Do x k >>= f = Do x $ \a -> (k a >>= f)
  Wait s k >>= f = Wait s (\a -> k a >>= f)


waitEv :: String -> (Ptr t -> Bool) -> (Ptr t -> Process x) -> Process x
waitEv msg p k = do
  Wait msg $ \ev -> case p ev of
    True -> k ev
    False -> waitEv msg p k

