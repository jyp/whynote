module CurveApprox where

import Data.Word
import Data.Matrix as Matrix
import Data.Vector as V
import NoteData
import Debug.Trace
import Control.Monad
import Data.Monoid
import Data.Function

bezierCharacteristic = Matrix.fromList 4 4
       [-1 ,3 ,-3 ,1
       , 3 ,-6 , 3 ,0
       ,-3 ,3 , 0 ,0
       , 1 ,0 , 0 ,0]

Right bezierCharacteristicInv = inverse bezierCharacteristic

qMatrix :: Vector Double -> Matrix Double
qMatrix xs = matrix (V.length xs) 4 $ \(i,j) -> (xs V.! (i-1)) ^^ (4-j)

evalCurve :: Matrix Double -> Vector Double -> Matrix Double
evalCurve c ts = q `multStd2` bezierCharacteristic `multStd2` c
   where q = qMatrix ts

approxCoeffs :: Vector Double -> Vector Double -> Matrix Double
approxCoeffs ts xs = c
  where q = qMatrix ts -- 4xn
        qt = transpose q -- nx4
        qtq = qt `multStd2` q -- 4x4
        x = colVector xs -- 1xn
        c = case Matrix.inverse qtq of
          Left err -> error err
          Right sol -> bezierCharacteristicInv `multStd` sol `multStd` (qt `multStd2` x)


approxControlPoints curve = (result,maxError)
  where
  ts = fmap (\c -> fromIntegral (coordT c - t0w) / dt) curve
  dt = fromIntegral (coordT (V.last curve) - t0w)
  [ex,ey,ez] = fmap (getMatrixAsVector . flip evalCurve ts) cs -- evaluated coordinates
  errorFct (Coord x y z _) ax ay az = sqrt (sq (x-ax) + sq (y-ay))
  maxError = V.maximum (V.zipWith4 errorFct curve ex ey ez)
  Coord _ _ _ t0w = V.head curve
  [[x0,x1,x2,x3],
   [y0,y1,y2,y3],
   [z0,z1,z2,z3]] = fmap Matrix.toList cs
  cs = [approxCoeffs ts (fmap proj curve) | proj <- [coordX,coordY,coordZ]]
  sq x = x*x
  result = [Coord x0 y0 z0 0,
            Coord x1 y1 z1 1,
            Coord x2 y2 z2 2,
            Coord x3 y3 z3 3]

approxControlPoints' curve = (result,errData)
  where
  normT t = fromIntegral (t - t0) / dt
  ts = fmap (normT . coordT) curve
  dt = fromIntegral (coordT (V.last curve) - t0)
  [ex,ey,ez] = fmap (getMatrixAsVector . flip evalCurve ts) cs' -- evaluated coordinates
  errorFct w@(Coord x y z t) ax ay az i = (i,normT t,w,sqrt (sq (x-ax) + sq (y-ay)))
  errData = V.maximumBy (compare `on` \(_,_,_,e) -> e) (V.zipWith5 errorFct curve ex ey ez (V.fromList [0..V.length curve-1]))
  Coord x0 y0 z0 t0 = V.head curve
  Coord x3 y3 z3 t3 = V.last curve
  [[_x0,x1,x2,_x3],
   [_y0,y1,y2,_y3],
   [_z0,z1,z2,_z3]] = fmap Matrix.toList cs
  cs = [approxCoeffs ts (fmap proj curve) | proj <- [coordX,coordY,coordZ]]
  cs' = fmap (Matrix.fromList 4 1) [[x0,x1,x2,x3],[y0,y1,y2,y3],[z0,z1,z2,z3]]
  sq x = x*x
  result = [Coord x0 y0 z0 0,
            Coord x1 y1 z1 1,
            Coord x2 y2 z2 2,
            Coord x3 y3 z3 3]

exampleCurve = V.fromList $ Prelude.reverse
  [Coord {coordX = 170.485717773, coordY = 287.41064453125, coordZ = 0.91943359375, coordT = 43228638}
  ,Coord {coordX = 169.655395507, coordY = 287.54864501953, coordZ = 0.84912109375, coordT = 43228630}
  ,Coord {coordX = 169.240234375, coordY = 287.41064453125, coordZ = 0.69384765625, coordT = 43228626}
  ,Coord {coordX = 169.240234375, coordY = 286.85870361328, coordZ = 0.54443359375, coordT = 43228618}]

type Spline = (Coord,Tree,Coord)
data Tree = Node {controlPoints :: (Coord,Coord), -- two middle control points for the bezier curve
                  recurse :: Maybe (Double, -- parameter of the bezier where we make the most error
                                    Coord, -- point where he curve splits (exact)
                                    Tree,Tree)}
curveApproxes curve = Node (c1,c2) (if (Prelude.length curve > 4 && err > eps) then Just recur else Nothing)
  where ([_,c1,c2,_],(i,maxErrT,w,err)) = approxControlPoints' curve
        recur = (maxErrT,w,curveApproxes (l <> V.singleton (V.head r)),curveApproxes r)
        (l,r) = V.splitAt i curve
        eps = 0.1

{-approxCurve :: Curve -> Cairo.Render ()
approxCurve (Curve c) = when (V.length c >= 4) $ do
  setLineWidth 0.2
  Cairo.setSourceRGBA 1 0.2 0.2 1
  moveTo x0 y0
  curveTo x1 y1 x2 y2 x3 y3
  stroke
  where [Coord x0 y0 z0 0,
         Coord x1 y1 z1 1,
         Coord x2 y2 z2 2,
         Coord x3 y3 z3 3] = fst (approxControlPoints' c)
-}
