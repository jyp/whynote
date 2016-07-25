{-#LANGUAGE RecordWildCards, LambdaCase #-}

module QuadTree where


data Q a = Empty | Quad [a] (Q a) (Q a) (Q a) (Q a)

data Quadrant = NW | NE | SE | SW

quadrants = [NW , NE , SE , SW]

type QuadTree a = Maybe (Extent,Q a)

type Number = Rational
data Extent = Extent {extentX :: Number, extentY :: Number, extentSize :: Number}

-- | Ceiling of the log (exact).
ceilLog :: Rational -> Int
ceilLog r | r > 2 = ceilLog (r/2) + 1ua
          | r <= 1 = ceilLog (r*2) - 1
          | otherwise = 0

-- floorBy :: Number -> Number -> Number
-- floorBy by n = floor (n / by) * by

-- roundExtent :: Extent -> Extent
-- roundExtent Extent{..} = Extent {extentX = floorBy sz extentX,
--                                  extentY = floorBy sz extentY,
--                                  extentSize = sz}
--   where sz = 2 ^^ (loga extentSize)

subExtent Extent {extentSize=sz,..} = \case
  NW -> Extent {..}
  NE -> Extent {extentX = extentX + extentSize,..}
  SW -> Extent {extentY = extentY + extentSize,..}
  SE -> Extent {extentX = extentX + extentSize,extentY = extentY + extentSize,..}
  where extentSize = sz /2

insert :: (a,Extent) -> Extent -> Q a -> Q a
insert (a,ex) ex0 q = case moveToChild of
  (quadrant,subEx):_ -> update quadrant q (insert (a,ex) subEx)
  [] -> case q of
    Empty -> Quad [(a,ex)] Empty Empty Empty Empty
  where moveToChild = [(quadrant,subEx) | quadrant <- quadrants, let subEx = subExtent ex0 quadrant, subEx `contains` ex]


update q Empty f = update (Quad []) Empty Empty Empty Empty
update q (Quad xs q1 q2 q3 q4) = case q of
  NW -> Quad xs (f q1) q2 q3 q4
  NE -> Quad xs (f q1) q2 q3 q4
  SW -> Quad xs (f q1) q2 q3 q4
  SE -> Quad xs (f q1) q2 q3 q4
