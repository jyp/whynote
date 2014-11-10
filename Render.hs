module Render where

import qualified Prelude as P
import WNPrelude
import Graphics.Rendering.Cairo as Cairo
import Event
-- import Control.Monad 
import NoteData
import qualified Data.Vector as V
import Data.Traversable
import Data.Foldable

drawEv :: Event -> Render ()
drawEv ev@Event{eventCoord = Coord x y z t} = do
    setSourceRGB 0 0 0
    setLineWidth 2
    moveTo x y
    arc x y (2 * z) 0 3
    stroke

drawLasso :: ClosedCurve -> Cairo.Render ()
drawLasso (Closed c)
  | V.null c = return ()
  | otherwise = do
    let p0 = V.head c
        ps = V.tail c
    Cairo.setSourceRGBA 0 0 0 0.2
    Cairo.setFillRule Cairo.FillRuleEvenOdd
    setLineWidth 5
    xy p0 moveTo
    forM_ ps $ \p -> xy p lineTo
    Cairo.fill

drawStroke :: Stroke -> Cairo.Render ()
drawStroke (Stroke (Boxed _ c)) = do
  Cairo.setSourceRGBA 0 0 0 1
  Cairo.setFillRule Cairo.FillRuleWinding
  strokePath c
  Cairo.fill

drawStrokeSelected :: Stroke -> Cairo.Render ()
drawStrokeSelected (Stroke (Boxed _ c)) = do
  strokePath c
  setLineWidth 3
  -- Cairo.setSourceRGBA 0.5 0.5 0.5 1
  -- strokePreserve
  Cairo.setSourceRGBA 1 1 1 1
  Cairo.setFillRule Cairo.FillRuleWinding
  Cairo.fill  

strokePath :: Curve -> Cairo.Render ()
strokePath (Curve c)
  | V.length c <= 1 = return ()
  | otherwise = do
    let phead@(Coord xo yo _z0 _t0) = V.head c
        xs = V.tail c
    Cairo.moveTo xo yo
    let plast = V.head rxs0
        rxs = V.tail rxs0
        rxs0 = V.reverse xs
    V.foldM_ forward phead xs
    V.foldM_ forward plast rxs
  where turn (x,y) = (negate y,x)
        norm (x,y) = sqrt (x*x + y*y)
        (x1,y1) .-. (x0,y0) = (x1-x0 , y1-y0)
        (x1,y1) .+. (x0,y0) = (x1+x0 , y1+y0)
        z *. (x0,y0) = (z * x0 , z * y0)
        zFactor = 1 -- to be tuned
        forward pc0@(Coord x0 y0 z0 t0) pc1@(Coord x y z t) = do
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
               P.uncurry Cairo.lineTo $ shift .+. p1
               return pc1

renderNoteData :: NoteData -> Render ()
renderNoteData cs = do
  -- TODO: filter (overlap box . boundingBox)
  forM_ cs drawStroke

boxRectangle :: Box -> Render ()
boxRectangle (Box lo hi) =
  lo `xy` \lx ly ->
  (hi-lo) `xy` 
  rectangle lx ly

renderSelection :: Selection -> Render ()
renderSelection (Selection bbox cs) = do
  save
  setDash [5,5] 0
  setLineWidth 1
  boxRectangle $ bbox
  strokePreserve
  setSourceRGBA 0 0 1 0.5
  fill
  restore
  forM_ cs drawStrokeSelected

