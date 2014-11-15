{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}
module NoteData where
import Prelude ()
import WNPrelude
import Control.Applicative
import Data.Aeson
import Data.Monoid
import Data.HashMap.Strict as H (union)
import Data.List
import Data.Word
import qualified Data.Vector as V

data Coord = Coord { coordX :: !Double
                   , coordY :: !Double
                   , coordZ :: !Double
                   , coordT :: !Word32
                   }
           deriving (Show,Eq,Ord)

mkColor r g b = Color (r/256) (g/256) (b/256) 1
data Color = Color !Double !Double !Double !Double
blackColor = Color 0 0 0 1
redColor = mkColor 220 50 47
blueColor = mkColor 38 139 210
greenColor = mkColor 65 133 153
highlightColor = Color 1 1 0 0.5


data PenOptions =
  PenOptions
  {_penWidth :: Double
  ,_penColor :: Color
  }
defaultPen = PenOptions 1 blackColor
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
instance HasBox Selection where
  boundingBox (Selection bbox _) = bbox
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

instance HasBox a => HasBox (V.Vector a) where
  boundingBox = boundingBox . V.toList

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

instance TwoD a => TwoD (V.Vector a) where
  transform tr = fmap (transform tr)

instance TwoD a => TwoD (Boxed a) where
  transform tr (Boxed b a) = Boxed (transform tr b) (transform tr a)

data Box = Box Coord Coord
newtype Curve = Curve (V.Vector Coord)
  deriving (HasBox, TwoD)
newtype ClosedCurve = Closed (V.Vector Coord)
  deriving (HasBox, TwoD)
data Stroke = Stroke PenOptions (Boxed Curve)
instance HasBox Stroke where
  boundingBox (Stroke _ x) = boundingBox x
instance TwoD Stroke where
  transform tr (Stroke opts x) = Stroke opts (transform tr x)
instance Area Stroke where
  inArea p (Stroke _ x) = inArea p x
  nearArea d p (Stroke _ x) = nearArea d p x
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

vrot xs = V.tail xs <>  V.singleton (V.head xs)

instance Area ClosedCurve where
  inArea p (Closed v) =
    if V.null v
       then False
       else odd winding
    where qs = fmap (quadrant . (\p' -> p' - p)) v
          dqs = V.zipWith diffQuadr qs (vrot qs)
          winding = V.sum dqs `div` 4

strokeInArea :: Area a => Stroke -> a -> Bool
(Stroke _ (Boxed _ (Curve s1))) `strokeInArea` s2 = V.all (`inArea` s2) s1

instance Area Curve where
  inArea _ _ = False
  nearArea d p (Curve ps) = V.any (nearArea d p) ps

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


instance FromJSON Stroke where
    parseJSON a = Stroke <$> parseJSON a <*> parseJSON a

instance FromJSON PenOptions where
  parseJSON (Object v) = PenOptions <$> ((v .:? "width") .!= 1) <*> ((v .:? "color") .!= blackColor)

instance ToJSON PenOptions where
  toJSON (PenOptions w c) = object ["width" .= w,"color" .= c]

instance FromJSON Color where
  parseJSON (Object v) = Color <$> v.:"r" <*> v.:"g" <*> v.:"b" <*> v.:"a"

instance ToJSON Color where
  toJSON (Color r g b a) = object ["r" .= r, "g" .= g, "b" .= b, "a" .= a]

instance ToJSON Stroke where
  toJSON (Stroke opts curve) = Object (H.union opts' curve')
     where Object opts' = toJSON opts
           Object curve' = toJSON curve

instance ToJSON Curve where
  toJSON (Curve c) = object ["points" .= c]
  
instance ToJSON a => ToJSON (Boxed a) where
  toJSON (Boxed _ a) = toJSON a

instance (FromJSON a, HasBox a) => FromJSON (Boxed a) where
  parseJSON x = box <$> parseJSON x
