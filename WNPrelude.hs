module WNPrelude
       (module Control.Applicative,
        module Prelude.YAP,
        module Data.YAP.Algebra,
        module Data.Foldable
        )
       where

import Prelude.YAP hiding (foldr)
import Control.Applicative
import Data.YAP.Algebra
import Data.Word
import Data.Foldable (foldr,toList)

import qualified Prelude as P

instance AbelianGroup Word32 where
  zero = 0
  (+) = (P.+)
  (-) = (P.-)
