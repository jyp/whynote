module WNPrelude
       (module Control.Applicative,
        module Prelude.YAP,
        module Data.YAP.Algebra,
        )
       where

import Prelude.YAP
import Control.Applicative
import Data.YAP.Algebra
import Data.Word

import qualified Prelude as P

instance AbelianGroup Word32 where
  zero = 0
  (+) = (P.+)
  (-) = (P.-)
