module CurveApprox where

import Data.Word
import Data.Matrix as Matrix
import Data.Vector as V
import NoteData
import Debug.Trace

bezierCharacteristic = Matrix.fromList 4 4
       [-1 ,3 ,-3 ,1
       , 3 ,-6 , 3 ,0
       ,-3 ,3 , 0 ,0
       , 1 ,0 , 0 ,0]

Right bezierCharacteristicInv = inverse bezierCharacteristic


qMatrix :: Vector Double -> Matrix Double
qMatrix xs = matrix (V.length xs) 4 $ \(i,j) -> (xs V.! (i-1)) ^^ (4-j)


qmMatrix ts = (qMatrix ts) `multStd2` bezierCharacteristic

coeffs' :: [Double] -> [Double] -> Matrix Double
coeffs' ts xs = coeffsOrig (V.fromList ts) (V.fromList xs)

coeffs :: Vector Double -> Vector Double -> Matrix Double
coeffs ts xs = c
  where qm = q `multStd2` m -- 4xn
        q = qMatrix ts -- 4xn
        m = bezierCharacteristic -- 4x4
        qmt = transpose qm -- nx4
        qmtqm = qmt `multStd2` qm
        x = colVector xs
        c = case Matrix.inverse qmtqm of
          Left err -> error err
          Right sol -> (sol `multStd2` qmt) `multStd2` x

evalCurve :: Matrix Double -> Vector Double -> Matrix Double
evalCurve c ts = q `multStd2` bezierCharacteristic `multStd2` c
   where q = qMatrix ts

coeffsOrig :: Vector Double -> Vector Double -> Matrix Double
coeffsOrig ts xs = c
  where q = qMatrix ts -- 4xn
        qt = transpose q -- nx4
        qtq = qt `multStd2` q -- 4x4
        x = colVector xs -- 1xn
        c = case Matrix.inverse qtq of
          Left err -> error err
          Right sol -> bezierCharacteristicInv `multStd` sol `multStd` (qt `multStd2` x)

-- curveControlPoints :: Vector Coord -> [Coord]
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
  cs = [coeffsOrig ts (fmap proj curve) | proj <- [coordX,coordY,coordZ]]
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
