{-#LANGUAGE RecordWildCards, LambdaCase, TemplateHaskell, ScopedTypeVariables, DeriveFunctor,DeriveFoldable,FlexibleContexts, GADTs, TypeSynonymInstances, FlexibleInstances #-}

module QuadTree where
import Prelude ()
import WNPrelude
import Data.Ratio
import Test.QuickCheck
import Data.Word
import Debug.Trace

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



data Interval a = Box {intervalLo :: !a, intervalHi :: !a} deriving (Functor,Foldable,Show)

instance Applicative Interval where
  pure x = Box x x
  Box f g <*> Box x y = Box (f x) (g y)
type Box = Interval Coord

data Boxed' f a = Boxed {boxedBox :: Interval a, boxedContents :: f a} deriving (Functor,Show)
instance Eq (f a) => Eq (Boxed' f a) where
  (Boxed _ x) == (Boxed _ y) = x == y
type Boxed f = Boxed' f Coord
instance (Lattice a,Arbitrary a, Arbitrary (f a)) => Arbitrary (Boxed' f a) where
  arbitrary = Boxed <$> arbitrary <*> arbitrary

class HasBox a where
  boundingBox :: a -> Box
instance HasBox Box where
  boundingBox = id
instance HasBox (Boxed' x Coord) where
  boundingBox (Boxed b _) = b

instance Arbitrary Coord where
  arbitrary = Coord <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Lattice a,Arbitrary a) => Arbitrary (Interval a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Box (x /\ y) (x \/ y))

data Q a = Empty | Quad [a] (Q a) (Q a) (Q a) (Q a) deriving (Functor,Foldable,Show)
instance (HasBox a,Arbitrary a) => Arbitrary (QuadTree a) where
  arbitrary = do
    ex <- arbitrary
    (elems::[a]) <- arbitrary
    return $ foldr insert (QT ex Empty) elems

data Quadrant = NW | NE | SE | SW deriving (Show,Eq)
instance Arbitrary Quadrant where arbitrary = elements quadrants
quadrants :: [Quadrant]
quadrants = [NW , NE , SE , SW]

data QuadTree a = QT {qtExtent :: Extent, qtTree :: Q a} deriving (Show,Functor,Foldable)

type Number = Rational
data Extent = Extent {extentX :: Number, extentY :: Number, extentSize :: Number} deriving (Show,Eq)

extentBox :: Extent -> Box
extentBox e = Box (Coord (fromRational (extentX e)) (fromRational (extentY e)) 0 0)
                  (Coord (fromRational (extentX' e)) (fromRational (extentY' e)) 0 0)

instance Arbitrary Extent where
  arbitrary = do
    n :: Integer <- arbitrary
    x :: Integer <- arbitrary
    y :: Integer <- arbitrary
    let extentSize = 2^^n
        sz2 = extentSize / 2
        extentX = fromIntegral x*extentSize - sz2
        extentY = fromIntegral y*extentSize - sz2
    return Extent{..}


extentX' Extent {..} = extentX + extentSize
extentY' Extent {..} = extentY + extentSize

{-roundBy :: Number -> Number -> Number
roundBy by n = fromIntegral (round (n / by) :: Integer) * by

divBy :: Number -> Number -> Bool
divBy by n = numerator x `mod` denominator x == 0
  where x = n / by

prop_div_round :: Positive Number -> Number -> Bool
prop_div_round (Positive b) n = divBy b (roundBy b n)
-}
-- | sub extent for a given quadrant
subExtent :: Extent -> Quadrant -> Extent
subExtent Extent {extentSize=sz,..} = \case
  NW -> Extent {..}
  NE -> Extent {extentX = extentX + extentSize,..}
  SW -> Extent {extentY = extentY + extentSize,..}
  SE -> Extent {extentX = extentX + extentSize,extentY = extentY + extentSize,..}
  where extentSize = sz /2

superExtent :: Box -> Extent -> (Quadrant,Extent)
superExtent
  (Box (Coord lx ly _ _) _)
  Extent {extentX = x, extentY = y, extentSize = sz} = (quadrant, Extent{..})
  where extentX = if left then x else x-sz
        extentY = if top  then y else y-sz
        left = lx >= fromRational x
        top = ly >= fromRational y
        quadrant | left,top = NW
                 | left     = SW
                 | top      = NE
                 | otherwise = SE
        extentSize = sz*2

prop_sub_super b e = let (q,e1) = superExtent b e in subExtent e1 q == e

superExtents :: Box -> Extent -> [Extent]
superExtents b ex = ex:superExtents b (snd (superExtent b ex))

prop_supers :: Box -> Extent -> Bool
prop_supers box ex = any (\e -> extentBox e `contains` box) (superExtents box ex)

contains :: Box -> Box -> Bool
b1 `contains` b2 = (b1 /\ b2) `eqBox` b2
eqBox b1 b2 = all zeroCoord ((-) <$> b2 <*> b1)
zeroCoord (Coord x y _ _) = x == 0 && y == 0

prop_meet_contains :: Box -> Box -> Bool
prop_meet_contains b1 b2 = b1 `contains `(b1 /\ b2)

prop_join_contains :: Box -> Box -> Bool
prop_join_contains b1 b2 = (b1 \/ b2) `contains ` b1

overlapBox :: Box -> Box -> Bool
overlapBox b1 b2 = not $ emptyBox $ b1 /\ b2

emptyBox (Box (Coord x0 y0 _ _) (Coord x1 y1 _ _)) = (x0 >= x1) || (y0 >= y1)

-- | Insert in a quad tree of the given extent (does not create 'bigger' quadtree).
insert' :: (a,Box) -> Extent -> Q a -> Q a
insert' (a,bx) ex0 q = case moveToChild of
  (quadrant,subEx):_ -> update (insert' (a,bx) subEx) quadrant q
  [] -> case q of
    Empty -> Quad [a] Empty Empty Empty Empty
    Quad as q1 q2 q3 q4 -> Quad (a:as) q1 q2 q3 q4
  where moveToChild = [(quadrant,subEx) | quadrant <- quadrants, let subEx = subExtent ex0 quadrant, extentBox subEx `contains` bx]

insert :: HasBox a => a -> QuadTree a -> QuadTree a
insert x (QT ex0 q) | emptyBox b = (QT ex0 q)
                    | extentBox ex0 `contains` b = (QT ex0 (insert' (x,b) ex0 q))
                    | otherwise = insert x (QT ex1 (update (\_ -> q) quadrant Empty))
  where (quadrant,ex1) = superExtent b ex0
        b = boundingBox x

prop_insert_contains :: Boxed (K Int) -> QuadTree (Boxed (K Int)) -> Property
prop_insert_contains a@(Boxed b _) qt = not (emptyBox b) ==> property (a `elem` insert a qt)

qfilter :: HasBox a => Box -> QuadTree a -> QuadTree a
qfilter bx qt@(QT ex _) = QT ex (qfilter' bx qt)

qfilter' :: HasBox a => Box -> QuadTree a -> Q a
qfilter' _ (QT _ Empty) = Empty
qfilter' ext (QT ex0 q)
  | ext `contains` extentBox ex0 = q
  | overlapBox ext (extentBox ex0) = update' (filter (overlapBox ext . boundingBox)) {- FIXME: filter! -} f q
  | otherwise = Empty
     where f quadrant q' = qfilter' ext (QT (subExtent ex0 quadrant) q')

newtype K x a = K x deriving (Eq,Show)
instance Arbitrary x => Arbitrary (K x a) where
  arbitrary = K <$> arbitrary

prop_qfilter :: Box -> QuadTree (Boxed (K Int)) -> Bool
prop_qfilter b qt = toList (qfilter b qt) == filter (overlapBox b . boundingBox) (toList qt)

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
empty ex = QT ex Empty


return []
runTests :: IO Bool
runTests = $quickCheckAll
