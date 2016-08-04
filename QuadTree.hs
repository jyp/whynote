{-#LANGUAGE RecordWildCards, LambdaCase #-}

module QuadTree where
import Data.Ratio

data Q a = Empty | Quad [a] (Q a) (Q a) (Q a) (Q a)

data Quadrant = NW | NE | SE | SW

quadrants :: [Quadrant]
quadrants = [NW , NE , SE , SW]

type QuadTree a = Maybe (Extent,Q a)

type Number = Rational
data Extent = Extent {extentX :: Number, extentY :: Number, extentSize :: Number}


floorBy :: Number -> Number -> Number
floorBy by n = fromIntegral (floor (n / by) :: Integer) * by

divBy :: Number -> Number -> Bool
divBy by n = numerator x `mod` denominator x == 0
  where x = n / by

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

insert :: (a,Extent) -> (Extent, Q a) -> (Extent, Q a)
insert (x,ext) (ex0,q) | ex0 `contains` ext = (ext,insert' (x,ext) ex0 q)
                       | otherwise = insert (x,ext) (ex1,update (\_ -> q) quadrant Empty)
  where (quadrant,ex1) = superExtent ex0

overlaps :: Extent -> Extent -> Bool
overlaps = _

qfilter :: Extent -> (Extent, Q a) -> [a]
qfilter ext (ex0,Empty) = []
qfilter ext (ex0,q@(Quad as _ _ _ _)) | overlaps ext ex0
  = as ++ concat [qfilter ext (subExtent ex0 quadrant , subQ quadrant q ) | quadrant <- quadrants]
  | otherwise = []

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

quad [] Empty Empty Empty Empty = Empty
quad as a b c d = Quad as a b c d
