{-# LANGUAGE FlexibleContexts, DeriveFunctor, DeriveFoldable, OverloadedStrings, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, NamedFieldPuns, RecordWildCards, RankNTypes, MultiParamTypeClasses #-}
module NoteData where
import Prelude ()
import WNPrelude
import Data.Aeson
import Data.Monoid
import Data.HashMap.Strict as H (union)
import Data.List
import Data.Word
import qualified Data.Vector as V

data Sample' a = Sample {sampleCoord :: !a
                        ,sampleZ :: !Double -- pressure ∈ [0..1]
                        ,sampleT :: !Word32} deriving (Show,Eq,Ord,Functor)
type Sample = Sample' Coord
data Coord = Coord { coordX :: !Double
                   , coordY :: !Double
                   } deriving (Show,Eq,Ord)

data Finger' a = Finger {fingerStart :: a,
                         fingerCurrent :: a} deriving Functor
type Finger = Finger' Coord

blackColor :: Color
blackColor = Color 0 0 0 1

data Color = Color {colorRed, colorGreen, colorBlue, colorAlpha :: !Double}

boxWidth (Box l h) = coordX (h-l)
boxHeight (Box l h) = coordY (h-l)
mkPenWidth x = Box zero zero {coordX = x}
type PenOptions = PenOptions' Coord
data PenOptions' a =
  PenOptions
  {_penShape :: Interval a -- this is a difference of coordinates, so that when a stroke is scaled, the pen shape is scaled too.
  ,_penColor :: Color
  ,_penSensitivity :: Double -- in range [0,1]
  } deriving Functor


instance Additive Coord where
  Coord x1 y1 + Coord x2 y2 = Coord (x1+x2) (y1+y2)
  zero = Coord 0 0
instance AbelianAdditive Coord
instance Group Coord where
  Coord x1 y1 - Coord x2 y2 = Coord (x1-x2) (y1-y2)
instance Module Double Coord where
  f *^ (Coord x y) = Coord (f*x) (f*y)

xy :: forall t. Coord -> (Double -> Double -> t) -> t
xy (Coord x y) f  = f x y

data Selection' a = Selection {selectionBox :: Interval a, selectionStrokes::[Stroke' a]}
  deriving Functor
type Selection = Selection' Coord
instance Area Selection where
  inArea p (Selection b _) = inArea p b
  nearArea d p (Selection b _) = nearArea d p b

emptySelection :: Selection
emptySelection = Selection (Box zero zero) []
isEmptySelection :: forall t. Selection' t -> Bool
isEmptySelection (Selection _ xs) = null xs
instance HasBox Selection where
  boundingBox (Selection bbox _) = bbox
------------------
-- Boxes

data Boxed' f a = Boxed (Interval a) (f a) deriving Functor
type Boxed f = Boxed' f Coord

box :: HasBox (f Coord) => f Coord -> Boxed' f Coord
box x = Boxed (boundingBox x) x

extend :: Double -> Box -> Box
extend d (Box p1 p2) = Box (p1-x)(p2+x)
  where x = Coord d d

class HasBox a where
  boundingBox :: a -> Box

instance HasBox (Boxed' x Coord) where
  boundingBox (Boxed b _) = b

instance HasBox Box where
  boundingBox = id

instance HasBox a => HasBox [a] where
  boundingBox = foldl' (\/) top . map boundingBox

instance HasBox a => HasBox (V.Vector a) where
  boundingBox = boundingBox . toList

instance HasBox Coord where
  boundingBox c = Box c c

instance HasBox a => HasBox (Sample' a) where
  boundingBox (Sample c _ _) = boundingBox c

class Area a where
  inArea :: Coord -> a -> Bool
  nearArea :: Double -> Coord -> a -> Bool

instance Area (a Coord) => Area (Boxed a) where
  inArea p (Boxed b a) = inArea p b && inArea p a
  nearArea d p (Boxed b a) = nearArea d p b && nearArea d p a

instance Area Box where
  inArea (Coord x y) (Box p1 p2) =
    p1 `xy` \x1 y1 -> p2 `xy` \x2 y2 -> x1 <= x && x <= x2 && y1 <= y && y <= y2
  nearArea d p b = inArea p (extend d b)

data Dilation = Dilation {_trZoom :: Double, _trDelta :: Coord}

apply :: Dilation -> Coord -> Coord
apply (Dilation zz d) c = zz *^ c + d

instance Additive Dilation where
  (+) (Dilation z1 d1) (Dilation z2 d2) = Dilation (z1*z2) (d1 + z1*^d2)
  zero = Dilation 1 zero
instance Group Dilation where
  negate (Dilation z d) = Dilation f (negate (f*^d))
    where f = 1/z


data Interval a = Box {intervalLo :: !a, intervalHi :: !a} deriving (Functor,Show)
type Box = Interval Coord
newtype Curve' a = Curve {fromCurve :: V.Vector (Sample' a)}
  deriving (Functor,HasBox)
type Curve = Curve' Coord
newtype ClosedCurve' a = Closed (V.Vector a)
  deriving (Functor,HasBox)
type ClosedCurve = ClosedCurve' Coord
data Stroke' a = Stroke (PenOptions' a) (Boxed' Curve' a)
  deriving Functor
type Stroke = Stroke' Coord
instance HasBox Stroke where
  boundingBox (Stroke pen x) = extend (boxWidth (_penShape pen)) bbox
    where bbox = boundingBox x
instance Area Stroke where
  inArea p (Stroke _ x) = inArea p x
  nearArea d p (Stroke _ x) = nearArea d p x
type NoteData = [Stroke]

-- | Quadrand where the coord lies
quadrant :: Coord -> Int
quadrant (Coord x y) = if x > 0
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
    if null v
       then False
       else odd winding
    where qs = fmap (quadrant . (\p' -> p' - p)) v
          dqs = V.zipWith diffQuadr qs (vrot qs)
          winding = sum dqs `div` 4

strokeInArea :: Area a => Stroke -> a -> Bool
(Stroke _ (Boxed _ (Curve s1))) `strokeInArea` s2 = all ((`inArea` s2) . sampleCoord) s1

instance Area Curve where
  inArea _ _ = False
  nearArea d p (Curve ps) = any (nearArea d p . sampleCoord) ps

lassoPartitionStrokes :: Boxed ClosedCurve' -> [Stroke] -> ([Stroke],[Stroke])
lassoPartitionStrokes strk = partition (`strokeInArea` strk)

instance Area Coord where
  inArea p1 p2 = p1 == p2
  nearArea d p1 p2 = dx*dx + dy*dy < d*d
    where Coord dx dy = p2 - p1

partitionStrokesNear :: Double -> Coord -> [Stroke] -> ([Stroke],[Stroke])
partitionStrokesNear d2 p strks = partition (nearArea d2 p) strks

class Lattice a where
  (\/),(/\) :: a -> a -> a
  bottom,top :: a

infty :: Double
infty = 1/0
instance Lattice Coord where
  (Coord x1 y1) \/ (Coord x2 y2) = Coord (max x1 x2) (max y1 y2)
  c1 /\ c2 = negate ((negate c1) \/ (negate c2))
  bottom = Coord infty infty
  top = Coord (-infty) (-infty)
instance Lattice a => Lattice (Interval a) where
  (Box l1 h1) \/ (Box l2 h2) = Box (l1 /\ l2) (h1 \/ h2)
  (Box l1 h1) /\ (Box l2 h2) = Box (l1 \/ l2) (h1 /\ h2)
  bottom = Box top bottom
  top = Box bottom top

nilBox (Box (Coord x0 y0) (Coord x1 y1)) = (x0 >= x1) || (y0 >= y1)

overlapBox :: Box -> Box -> Bool
overlapBox b1 b2 = not $ nilBox $ b1 /\ b2

----
-- Serialisation

instance ToJSON Sample where
  toJSON (Sample (Coord x y) z t) = object ["x" .= x, "y" .= y, "z" .= z, "t" .= t]

instance FromJSON Sample where
  parseJSON = withObject "Coord" $ \v -> Sample <$> (Coord <$> v.: "x" <*> v.: "y") <*> v.: "z" <*> v.: "t"

instance FromJSON Curve where
    parseJSON = withObject "Curve" $ \a -> Curve <$> a.: "points"


instance FromJSON Stroke where
    parseJSON a = Stroke <$> parseJSON a <*> parseJSON a

instance FromJSON PenOptions where
  parseJSON = withObject "PenOptions" $ \v ->
    PenOptions <$> (mkPenWidth <$> ((v .:? "width") .!= 1))
               <*> ((v .:? "color") .!= blackColor)
               <*> ((v .:? "sensitivity") .!= 1)

instance ToJSON PenOptions where
  toJSON (PenOptions w c s) = object ["width" .= boxWidth w,"color" .= c, "sensitivity" .= s]

instance FromJSON Color where
  parseJSON = withObject "color" $ \v -> Color <$> v.:"r" <*> v.:"g" <*> v.:"b" <*> v.:"a"

instance ToJSON Color where
  toJSON (Color r g b a) = object ["r" .= r, "g" .= g, "b" .= b, "a" .= a]

instance ToJSON Stroke where
  toJSON (Stroke opts curve) = Object (H.union opts' curve')
     where Object opts' = toJSON opts
           Object curve' = toJSON curve

instance ToJSON Curve where
  toJSON (Curve c) = object ["points" .= c]

instance ToJSON (f a) => ToJSON (Boxed' f a) where
  toJSON (Boxed _ a) = toJSON a

instance (FromJSON (f Coord), HasBox (f Coord)) => FromJSON (Boxed f) where
  parseJSON x = box <$> parseJSON x
