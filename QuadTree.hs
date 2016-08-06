{-#LANGUAGE RecordWildCards, LambdaCase, TemplateHaskell, ScopedTypeVariables #-}

module QuadTree where
import Data.Ratio
import Test.QuickCheck
import Data.Monoid
data Q a = Empty | Quad [a] (Q a) (Q a) (Q a) (Q a)

data Quadrant = NW | NE | SE | SW deriving (Show,Eq)
instance Arbitrary Quadrant where arbitrary = elements quadrants

quadrants :: [Quadrant]
quadrants = [NW , NE , SE , SW]

type QuadTree a = (Extent,Q a)

type Number = Rational
data Extent = Extent {extentX :: Number, extentY :: Number, extentSize :: Number} deriving (Show,Eq)
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

contains :: Extent -> Extent -> Bool
Extent x0 y0 sz0 `contains` Extent x1 y1 sz1 = x1 >= x0 && y1 >= y0 && x1+sz1 <= x0+sz0 && y1+sz1 <= y0+sz0

-- | Insert in a quad tree of the given extent (does not create 'bigger' quadtree).
insert' :: (a,Extent) -> Extent -> Q a -> Q a
insert' (a,ex) ex0 q = case moveToChild of
  (quadrant,subEx):_ -> update (insert' (a,ex) subEx) quadrant q
  [] -> case q of
    Empty -> Quad [a] Empty Empty Empty Empty
    Quad as q1 q2 q3 q4 -> Quad (a:as) q1 q2 q3 q4
  where moveToChild = [(quadrant,subEx) | quadrant <- quadrants, let subEx = subExtent ex0 quadrant, subEx `contains` ex]

insert :: (a,Extent) -> QuadTree a -> QuadTree a
insert (x,ext) (ex0,q) | ex0 `contains` ext = (ext,insert' (x,ext) ex0 q)
                       | otherwise = insert (x,ext) (ex1,update (\_ -> q) quadrant Empty)
  where (quadrant,ex1) = superExtent ex0

overlaps :: Extent -> Extent -> Bool
overlaps e1 e2 = max (extentX e1) (extentX e2) < min (extentX' e1) (extentX' e2) &&
                 max (extentY e1) (extentY e2) < min (extentY' e1) (extentY' e2)

qfilter :: Extent -> QuadTree a -> QuadTree a
qfilter ext qt = (ext,qfilter' ext qt)

qfilter' :: Extent -> QuadTree a -> Q a
qfilter' ext (ex0,Empty) = Empty
qfilter' ext (ex0,q)
  | ext `contains` ex0 = q
  | overlaps ext ex0 = update' id {- FIXME: filter! -} f q
  | otherwise = Empty
     where f quadrant q' = qfilter' ext (subExtent ex0 quadrant,q')

instance Foldable Q where
  foldMap f Empty = mempty
  foldMap f (Quad as qa qb qc qd) = foldMap f as <> foldMap f qa <>foldMap f qb <>foldMap f qc <> foldMap f qd

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


prop_c1 :: Extent -> Extent -> Property
prop_c1 a b = a `contains` b ==> a `overlaps` b

empty :: Extent -> QuadTree a
empty ex = (ex,Empty)


return []
runTests :: IO Bool
runTests = $quickCheckAll
