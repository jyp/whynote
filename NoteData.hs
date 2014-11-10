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

data Selection = Selection Box [Stroke]
instance Area Selection where
  inArea p (Selection b _) = inArea p b
  nearArea d p (Selection b _) = nearArea d p b
emptySelection = Selection (Box zero zero) []
isEmptySetection (Selection _ xs) = null xs

------------------
-- Boxes

data Boxed x = Boxed Box x

box :: HasBox x => x -> Boxed x
box x = Boxed (boundingBox x) x

extend :: Double -> Box -> Box
extend d (Box p1 p2) = Box (p1-x)(p2+x)
  where x = Coord d d 0 0

class HasBox a where
  boundingBox :: a -> Box

instance HasBox (Boxed x) where
  boundingBox (Boxed b _) = b

instance HasBox Box where
  boundingBox = id

instance HasBox a => HasBox [a] where
  boundingBox = boxUnion . map boundingBox

instance HasBox Coord where
  boundingBox c = Box c c

class Area a where
  inArea :: Coord -> a -> Bool
  nearArea :: Double -> Coord -> a -> Bool

instance Area a => Area (Boxed a) where
  inArea p (Boxed b a) = inArea p b && inArea p a
  nearArea d p (Boxed b a) = nearArea d p b && nearArea d p a

instance Area Box where
  inArea (Coord x y _ _) (Box p1 p2) =
    p1 `xy` \x1 y1 -> p2 `xy` \x2 y2 -> x1 <= x && x <= x2 && y1 <= y && y <= y2
  nearArea d p b = inArea p (extend d b)

overlap :: Box -> Box -> Bool
overlap (Box l1 h1) (Box l2 h2) = 
    l1 `xy` \lx1 ly1 ->
    l2 `xy` \lx2 ly2 ->
    h1 `xy` \hx1 hy1 ->
    h2 `xy` \hx2 hy2 ->
    (lx2 <= hx1 && ly2 <= hy1) || (lx1 <= hx2 && ly1 <= hy2)

data Translation = Translation {trZoom, trX, drY :: Double}

class TwoD a where
  transform :: Translation -> a -> a

instance TwoD Coord where
  transform (Translation zz dx dy) (Coord x y z t) = Coord (x * zz + dx) (y * zz + dy) (z * zz) t

instance TwoD Box where
  transform tr (Box p1 p2) = Box (transform tr p1) (transform tr p2)

instance (TwoD a, TwoD b) => TwoD (a,b) where
  transform tr (a,b) = (transform tr a, transform tr b)

instance TwoD Selection where
  transform tr (Selection b a) = Selection (transform tr b) (transform tr a)
instance TwoD a => TwoD [a] where
  transform tr = map (transform tr)

instance TwoD a => TwoD (Boxed a) where
  transform tr (Boxed b a) = Boxed (transform tr b) (transform tr a)

data Box = Box Coord Coord
newtype Curve = Curve [Coord]
  deriving (HasBox, TwoD)
newtype ClosedCurve = Closed [Coord]
  deriving (HasBox, TwoD)
newtype Stroke = Stroke (Boxed Curve)
  deriving (HasBox, TwoD, Area)
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
diffQuadr :: Int -> Int -> Int
diffQuadr x y | z < negate 2 = z + 4
              | z > 2 = z - 4
              | otherwise = z
  where z = x - y

rot :: [a] -> [a]
rot (x:xs) = xs ++ [x]

instance Area ClosedCurve where
  inArea _ (Closed []) = False
  inArea p (Closed strk) = odd winding
    where qs = map (quadrant . (\p' -> p' - p)) strk
          dqs = zipWith diffQuadr qs (rot qs)
          winding = sum dqs `div` 4

strokeInArea :: Area a => Stroke -> a -> Bool
(Stroke (Boxed _ (Curve s1))) `strokeInArea` s2 = all (`inArea` s2) s1

instance Area Curve where
  inArea _ _ = False
  nearArea d p (Curve ps) = any (nearArea d p) ps

lassoPartitionStrokes :: Boxed ClosedCurve -> [Stroke] -> ([Stroke],[Stroke])
lassoPartitionStrokes strk = partition (`strokeInArea` strk)

instance Area Coord where
  inArea p1 p2 = p1 == p2
  nearArea d p1 p2 = dx*dx + dy*dy < d*d
    where Coord dx dy _ _ = p2 - p1

partitionStrokesNear :: Double -> Coord -> [Stroke] -> ([Stroke],[Stroke])
partitionStrokesNear d2 p strks = partition (nearArea d2 p) strks


----
-- Serialisation

instance ToJSON Coord where
  toJSON (Coord x y z t) = object ["x" .= x, "y" .= y, "z" .= z, "t" .= t]

instance FromJSON Coord where
  parseJSON (Object v) = Coord <$> v.: "x" <*> v.: "y" <*> v.: "z" <*> v.: "t"
  parseJSON _ = fail "Coord object expected"

instance FromJSON Curve where
    parseJSON (Object a) = Curve <$> a.: "points"
    parseJSON _ = empty

instance ToJSON Curve where
   toJSON (Curve a) = object ["points" .=  a]

instance FromJSON Stroke where
    parseJSON a = Stroke <$> parseJSON a

instance ToJSON Stroke where
   toJSON (Stroke c) = toJSON c

instance ToJSON a => ToJSON (Boxed a) where
  toJSON (Boxed _ a) = toJSON a

instance (FromJSON a, HasBox a) => FromJSON (Boxed a) where
  parseJSON x = box <$> parseJSON x
