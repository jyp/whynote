module WNPrelude
       (module Control.Applicative,
        module Algebra.Classes,
        module Data.Foldable,
        module P,
        loop,
        )
       where

import Algebra.Classes
import Control.Applicative
import Data.Foldable (foldr,toList)
import Prelude as P hiding (Integral(..),Num(..),gcd,Fractional(..),(^))

loop :: Monad m => m a -> m b
loop x = x >> loop x
