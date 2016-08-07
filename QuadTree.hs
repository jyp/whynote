{-#LANGUAGE RecordWildCards, LambdaCase, TemplateHaskell, ScopedTypeVariables, DeriveFunctor,DeriveFoldable #-}

module QuadTree where
import Prelude ()
import WNPrelude
import Data.Ratio
import Test.QuickCheck
import Data.Monoid
import Data.Word

infty :: Double
infty = 1/0

class Lattice a where
  (\/),(/\) :: a -> a -> a
  bottom,top :: a

data Coord = Coord { coordX :: !Double
                   , coordY :: !Double
                   , coordZ :: !Double -- pressure (> 0). Note that scaling gives pressures > 1.
                   , coordT :: !Word32
                   }
           deriving (Show,Eq,Ord)

instance Additive Coord where
  Coord x1 y1 z1 t1 + Coord x2 y2 z2 t2 = Coord (x1+x2)(y1+y2)(z1+z2) (t1+t2)
  zero = Coord 0 0 0 0
instance AbelianAdditive Coord
instance Group Coord where
  Coord x1 y1 z1 t1 - Coord x2 y2 z2 t2 = Coord (x1-x2)(y1-y2)(z1-z2) (t1-t2)
instance Lattice Coord where
  (Coord x1 y1 z1 t1) \/ (Coord x2 y2 z2 t2) = Coord (max x1 x2) (max y1 y2)(max z1 z2)(max t1 t2)
  c1 /\ c2 = negate ((negate c1) \/ (negate c2))
  bottom = Coord infty infty infty maxBound
  top = Coord (-infty) (-infty) (-infty) minBound

instance Lattice a => Lattice (Interval a) where
  (Box l1 h1) \/ (Box l2 h2) = Box (l1 /\ l2) (h1 \/ h2)
  (Box l1 h1) /\ (Box l2 h2) = Box (l1 \/ l2) (h1 /\ h2)
  bottom = Box top bottom
  top = Box bottom top



data Interval a = Box {intervalLo :: !a, intervalHi :: !a} deriving (Functor,Foldable)
instance Applicative Interval where
  pure x = Box x x
  Box f g <*> Box x y = Box (f x) (g y)
type Box = Interval Coord

data Boxed' f a = Boxed (Interval a) (f a) deriving Functor
type Boxed f = Boxed' f Coord

class HasBox a where
  boundingBox :: a -> Box


data Q a = Empty | Quad [a] (Q a) (Q a) (Q a) (Q a) deriving (Functor,Foldable)

data Quadrant = NW | NE | SE | SW deriving (Show,Eq)
instance Arbitrary Quadrant where arbitrary = elements quadrants

quadrants :: [Quadrant]
quadrants = [NW , NE , SE , SW]

type QuadTree a = (Extent,Q a)

type Number = Rational
data Extent = Extent {extentX :: Number, extentY :: Number, extentSize :: Number} deriving (Show,Eq)
extentBox e = Box (Coord (fromRational (extentX e)) (fromRational (extentY e)) 0 0)
                  (Coord (fromRational (extentX' e)) (fromRational (extentY' e)) 0 0)

instance Arbitrary Extent where
  arbitrary = do
    n :: Integer <- arbitrary
    x :: Integer <- arbitrary
    y :: Integer <- arbitrary
    let extentSize = 2^^n
        extentX = fromIntegral x*extentSize
        extentY = fromIntegral y*extentSize
    return Extent{..}

extentX' Extent {..} = extentX + extentSize
extentY' Extent {..} = extentY + extentSize

floorBy :: Number -> Number -> Number
floorBy by n = fromIntegral (floor (n / by) :: Integer) * by

divBy :: Number -> Number -> Bool
divBy by n = numerator x `mod` denominator x == 0
  where x = n / by

prop_div_floor :: Positive Number -> Number -> Bool
prop_div_floor (Positive b) n = divBy b (floorBy b n)

-- | sub extent for a given quadrant
subExtent :: Extent -> Quadrant -> Extent
subExtent Extent {extentSize=sz,..} = \case
  NW -> Extent {..}
  NE -> Extent {extentX = extentX + extentSize,..}
  SW -> Extent {extentY = extentY + extentSize,..}
  SE -> Extent {extentX = extentX + extentSize,extentY = extentY + extentSize,..}
  where extentSize = sz /2

superExtent :: Extent -> (Quadrant,Extent)
superExtent Extent {extentX = x, extentY = y, extentSize = sz} = (quadrant, Extent{..})
  where extentX = floorBy extentSize x
        extentY = floorBy extentSize y
        left = divBy extentSize x
        top = divBy extentSize y
        quadrant | left,top = NW
                 | left     = SW
                 | top      = NE
                 | otherwise = SE
        extentSize = sz*2

prop_sub_super e = let (q,e1) = superExtent e in subExtent e1 q == e
prop_super_sub q e = superExtent (subExtent e q) == (q,e)


contains :: Box -> Box -> Bool
b1 `contains` b2 = (b1 /\ b2) `eqBox` b2
eqBox b1 b2 = all zeroCoord ((-) <$> b2 <*> b1)
zeroCoord (Coord x y _ _) = x == 0 && y == 0

-- | Insert in a quad tree of the given extent (does not create 'bigger' quadtree).
insert' :: (a,Box) -> Extent -> Q a -> Q a
insert' (a,ex) ex0 q = case moveToChild of
  (quadrant,subEx):_ -> update (insert' (a,ex) subEx) quadrant q
  [] -> case q of
    Empty -> Quad [a] Empty Empty Empty Empty
    Quad as q1 q2 q3 q4 -> Quad (a:as) q1 q2 q3 q4
  where moveToChild = [(quadrant,subEx) | quadrant <- quadrants, let subEx = subExtent ex0 quadrant, extentBox subEx `contains` ex]

insert :: HasBox a => a -> QuadTree a -> QuadTree a
insert x (ex0,q) | extentBox ex0 `contains` ext = (ex0,insert' (x,ext) ex0 q)
                 | otherwise = insert x (ex1,update (\_ -> q) quadrant Empty)
  where (quadrant,ex1) = superExtent ex0
        ext = boundingBox x

qfilter :: HasBox a => Box -> QuadTree a -> QuadTree a
qfilter ext qt = (fst qt,qfilter' ext qt)

overlapBox :: Box -> Box -> Bool
overlapBox b1 b2 = not $ nilBox $ b1 /\ b2
  where nilBox (Box (Coord x0 y0 _ _) (Coord x1 y1 _ _)) = (x0 >= x1) || (y0 >= y1)

qfilter' :: HasBox a => Box -> QuadTree a -> Q a
qfilter' ext (ex0,Empty) = Empty
qfilter' ext (ex0,q)
  | ext `contains` extentBox ex0 = q
  | overlapBox ext (extentBox ex0) = update' (filter (overlapBox ext . boundingBox)) {- FIXME: filter! -} f q
  | otherwise = Empty
     where f quadrant q' = qfilter' ext (subExtent ex0 quadrant,q')

subQ q Empty = Empty
subQ NW (Quad _ q _ _ _) = q
subQ NE (Quad _ _ q _ _) = q
subQ SW (Quad _ _ _ q _) = q
subQ SE (Quad _ _ _ _ q) = q
update f q Empty = update f q (Quad [] Empty Empty Empty Empty)
update f q (Quad xs q1 q2 q3 q4) = case q of
  NW -> quad xs (f q1) q2 q3 q4
  NE -> quad xs q1 (f q2) q3 q4
  SW -> quad xs q1 q2 (f q3) q4
  SE -> quad xs q1 q2 q3 (f q4)

update' h f Empty = update' h f (Quad [] Empty Empty Empty Empty)
update' h f (Quad xs q1 q2 q3 q4) = quad (h xs) (f NW q1) (f NE q2) (f SW q3) (f SE q4)

quad [] Empty Empty Empty Empty = Empty
quad as a b c d = Quad as a b c d



empty :: Extent -> QuadTree a
empty ex = (ex,Empty)


return []
runTests :: IO Bool
runTests = $quickCheckAll
