module Render where

import qualified Graphics.Rendering.Cairo as Cairo
import Event
import           Control.Monad 


drawStroke :: [PointerCoord] -> Cairo.Render ()
drawStroke c = do
  Cairo.setSourceRGBA 0 0 0 1
  Cairo.setFillRule Cairo.FillRuleWinding
  drawVWStrokeCurve c
  Cairo.fill

drawVWStrokeCurve :: [PointerCoord] -> Cairo.Render ()
drawVWStrokeCurve [] = return ()
drawVWStrokeCurve [_] = return ()
drawVWStrokeCurve (phead@(PointerCoord xo yo _z0 _t0) : xs) = do
    Cairo.moveTo xo yo
    let (plast:rxs) = reverse xs
    foldM_ forward phead xs
    foldM_ forward plast rxs
  where turn (x,y) = (negate y,x)
        norm (x,y) = sqrt (x*x + y*y)
        (x1,y1) .-. (x0,y0) = (x1-x0 , y1-y0)
        (x1,y1) .+. (x0,y0) = (x1+x0 , y1+y0)
        z *. (x0,y0) = (z * x0 , z * y0)
        zFactor = 0.5 -- to be tuned
        forward pc0@(PointerCoord x0 y0 z0 t0) pc1@(PointerCoord x y z t) = do
          let p0 = (x0,y0)
              p1 = (x,y)
              dp = p1 .-. p0
              dist = norm dp
          if dist < 0.01 -- otherwise normalisation can diverge
             then return pc0
             else do
               -- shift the current position perpendicularly to the
               -- direction of movement, by an amount proportional to
               -- the pressure (z).
               let shift = turn $ ((1/dist) * z * zFactor) *. dp
               Prelude.uncurry Cairo.lineTo $ shift .+. p1
               return pc1
