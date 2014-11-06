module NoteData where

import Data.Word
data Coord = Coord { coordX :: Double
                   , coordY :: Double
                   , coordZ :: Double
                   , coordT :: Word32
                   }
           deriving (Show,Eq,Ord)

instance Num Coord where
  negate (Coord x y z t) = Coord (negate x)(negate y)(negate z)(negate t)
  Coord x0 y0 z0 t0 + Coord x1 y1 z1 t1 = Coord (x0+x1)(y0+y1)(z0+z1)(t0+t1)

type Stroke = [Coord]
type NoteData = [Stroke]

emptyNoteData = []

quadrant :: Coord -> Int
quadrant (Coord x y _ _) = if x > 0
                              then if y > 0 then 0 else 3
                              else if y > 0 then 1 else 2
diffQuadr x y | z < negate 2 = z + 4
              | z > 2 = z - 4
              | otherwise = z
  where z = x - y

rot (x:xs) = xs ++ [x]

pointInside :: Coord -> Stroke -> Bool
pointInside _ [] = False
pointInside p strk = odd winding
  where qs = map (quadrant . (\p' -> p' - p)) strk
        dqs = zipWith diffQuadr qs (rot qs)
        winding = sum dqs `div` 4

strokeInside :: Stroke -> Stroke -> Bool
s1 `strokeInside` s2 = all (`pointInside` s2) s1

strokesOutside :: Stroke -> [Stroke] -> [Stroke]
strokesOutside strk = filter (not . (`strokeInside` strk))
