{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module NoteData where

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

xy (Coord x y _ _) f  = f x y

instance Num Coord where
  negate (Coord x y z t) = Coord (negate x)(negate y)(negate z)(negate t)
  Coord x0 y0 z0 t0 + Coord x1 y1 z1 t1 = Coord (x0+x1)(y0+y1)(z0+z1)(t0+t1)

zero = Coord 0 0 0 0

class HasBox a where
  boundingBox :: a -> Box

instance HasBox a => HasBox [a] where
  boundingBox = boxUnion . map boundingBox

instance HasBox Coord where
  boundingBox c = Box c c

data Box = Box Coord Coord
type Curve = [Coord]
newtype Stroke = Stroke Curve
  deriving (HasBox)
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

pointInside :: Coord -> Curve -> Bool
pointInside _ [] = False
pointInside p strk = odd winding
  where qs = map (quadrant . (\p' -> p' - p)) strk
        dqs = zipWith diffQuadr qs (rot qs)
        winding = sum dqs `div` 4

strokeInside :: Stroke -> Curve -> Bool
(Stroke s1) `strokeInside` s2 = all (`pointInside` s2) s1

lassoPartitionStrokes :: Curve -> [Stroke] -> ([Stroke],[Stroke])
lassoPartitionStrokes strk = partition (`strokeInside` strk)

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
