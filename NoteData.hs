{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}
module NoteData where
import Prelude ()
import WNPrelude
import Control.Applicative
import Data.Aeson

import Data.List
import Data.Word
data Coord = Coord { coordX :: Double
                   , coordY :: Double
                   , coordZ :: Double
                   , coordT :: Word32
                   }
           deriving (Show,Eq,Ord)

instance AbelianGroup Coord where
  Coord x1 y1 z1 t1 + Coord x2 y2 z2 t2 = Coord (x1+x2)(y1+y2)(z1+z2)(t1+t2)
  Coord x1 y1 z1 t1 - Coord x2 y2 z2 t2 = Coord (x1-x2)(y1-y2)(z1-z2)(t1-t2)
  zero = Coord 0 0 0 0

s .>> (Coord x y z t) = Coord (s x) (s y) (s z) t
s .* v = (s *) .>> v

xy (Coord x y _ _) f  = f x y

type Selection = (Box, [Stroke])
emptySelection = (Box zero zero, [])
isEmptySetection = null . snd

------------------
-- Boxes

extend :: Double -> Box -> Box
extend d (Box p1 p2) = Box (p1-x)(p2+x)
  where x = Coord d d 0 0
        
class HasBox a where
  boundingBox :: a -> Box

instance HasBox a => HasBox [a] where
  boundingBox = boxUnion . map boundingBox

instance HasBox Coord where
  boundingBox c = Box c c

class Area a where
  inArea :: Coord -> a -> Bool

instance Area Box where
  inArea (Coord x y _ _) (Box p1 p2) =
    p1 `xy` \x1 y1 -> p2 `xy` \x2 y2 -> x1 <= x && x <= x2 && y1 <= y && y <= y2

data Translation = Translation {trZoom, trX, drY :: Double}

class TwoD a where
  transform :: Translation -> a -> a

instance TwoD Coord where
  transform (Translation zz dx dy) (Coord x y z t) = Coord (x * zz + dx) (y * zz + dy) (z * zz) t

instance TwoD Box where
  transform tr (Box p1 p2) = Box (transform tr p1) (transform tr p2)

instance (TwoD a, TwoD b) => TwoD (a,b) where
  transform tr (a,b) = (transform tr a, transform tr b)

instance TwoD a => TwoD [a] where
  transform tr = map (transform tr)

data Box = Box Coord Coord
type Curve = [Coord]
newtype Stroke = Stroke Curve
  deriving (HasBox, TwoD)
type NoteData = [Stroke]

emptyNoteData :: NoteData
emptyNoteData = []

unzipCoords :: [Coord] -> ([Double],[Double],[Double],[Word32])
unzipCoords [] = ([],[],[],[])
unzipCoords (Coord x y z t:ps) = (x:xs, y:ys, z:zs, t:ts)
  where (xs,ys,zs,ts) = unzipCoords ps

boxCoords :: Box -> (Coord,Coord)
boxCoords (Box p1 p2) = (p1,p2)

boxUnion :: [Box] -> Box
boxUnion [] = Box zero zero
boxUnion boxes = Box (Coord (minimum loxs)(minimum loys)(minimum lozs)(minimum lots))
                     (Coord (maximum hixs)(maximum hiys)(maximum hizs)(maximum hits))
  where (los,his) = unzip $ map boxCoords $ boxes
        (loxs,loys,lozs,lots) = unzipCoords los
        (hixs,hiys,hizs,hits) = unzipCoords his

-- | Quadrand where the coord lies
quadrant :: Coord -> Int
quadrant (Coord x y _ _) = if x > 0
                              then if y > 0 then 0 else 3
                              else if y > 0 then 1 else 2

-- | Quadrant variation
diffQuadr x y | z < negate 2 = z + 4
              | z > 2 = z - 4
              | otherwise = z
  where z = x - y

rot (x:xs) = xs ++ [x]

instance Area Curve where
  inArea _ [] = False
  inArea p strk = odd winding
    where qs = map (quadrant . (\p' -> p' - p)) strk
          dqs = zipWith diffQuadr qs (rot qs)
          winding = sum dqs `div` 4

strokeInArea :: Stroke -> Curve -> Bool
(Stroke s1) `strokeInArea` s2 = all (`inArea` s2) s1

lassoPartitionStrokes :: Curve -> [Stroke] -> ([Stroke],[Stroke])
lassoPartitionStrokes strk = partition (`strokeInArea` strk)

pointNear d2 p1 p2 = dx*dx + dy*dy < d2
  where Coord dx dy _ _ = p2 - p1

strokeNear d2 p (Stroke strk) = any (pointNear d2 p) strk

partitionStrokesNear :: Double -> Coord -> [Stroke] -> ([Stroke],[Stroke])
partitionStrokesNear d2 p strks = partition (strokeNear d2 p) strks


----
-- Serialisation

instance ToJSON Coord where
  toJSON (Coord x y z t) = object ["x" .= x, "y" .= y, "z" .= z, "t" .= t]

instance FromJSON Coord where
  parseJSON (Object v) = Coord <$> v.: "x" <*> v.: "y" <*> v.: "z" <*> v.: "t"
  parseJSON _ = fail "Coord object expected"

instance FromJSON Stroke where
    parseJSON (Object a) = Stroke <$> a.: "points"
    parseJSON _ = empty

instance ToJSON Stroke where
   toJSON (Stroke a) = object ["points" .=  a]
