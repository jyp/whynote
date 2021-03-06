{-# LANGUAGE RecordWildCards,ViewPatterns,DeriveFunctor,TypeSynonymInstances,FlexibleContexts,FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module Render where

import qualified Prelude as P
import WNPrelude
import Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk as G
import Control.Monad (when)
import NoteData
import qualified Data.Vector as V
import Data.Traversable
import Data.Foldable
import Data.List (findIndex)

type GuiElement = GuiElement' Coord
data GuiElement' a = Lass (ClosedCurve' a)
                   | Fing (Finger' a)
                   | Strk (Stroke' a)
                   | Menu {menuAngle::Double,menuCursor::a,menuCenter::a,menuOptions::[String]}
                   deriving Functor

instance HasBox GuiElement where
  boundingBox (Fing f) = extend 84 . boundingBox . fingerCurrent $ f
  boundingBox (Strk s) = boundingBox s
  boundingBox (Lass s) = boundingBox s
  boundingBox (Menu _ _ c _) = extend menuOuterCircle (boundingBox c)

renderGui :: GuiElement' Coord -> Render ()
renderGui (Fing (Finger {fingerCurrent = Coord x y})) = saveEx $ do
  Cairo.setSourceRGBA 0 0 0 0.7
  setLineWidth 3
  moveTo x y
  arc x y 80 0 (2*pi)
  Cairo.stroke
renderGui (Menu a p c opts) = renderMenu True a p c opts >> return ()
renderGui (Strk s) = drawStroke s
renderGui (Lass (Closed c))
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


setSourceColor :: Color -> Render ()
setSourceColor (Color r g b a) = Cairo.setSourceRGBA r g b a

drawStroke :: Stroke -> Cairo.Render ()
drawStroke (Stroke pen (Boxed _ c)) = do
  setSourceColor (_penColor pen)
  Cairo.setFillRule Cairo.FillRuleWinding
  strokePath pen c
  Cairo.fill
  -- approxCurve c

drawStrokeSelected :: Stroke -> Cairo.Render ()
drawStrokeSelected (Stroke opts (Boxed _ c)) = do

  strokePath opts c
  -- setLineWidth 2
  -- Cairo.setSourceRGBA 0.5 0.5 0.5 1
  -- strokePreserve
  Cairo.setSourceRGBA 1 1 1 1
  Cairo.setFillRule Cairo.FillRuleWinding
  Cairo.fill

strokePath :: PenOptions -> Curve -> Cairo.Render ()
strokePath (PenOptions {..}) (Curve c)
  | V.length c <= 1 = return ()
  | otherwise = do
    let phead@(Sample (Coord xo yo) _ _) = V.head c
        xs = V.tail c
    Cairo.moveTo xo yo
    let plast = V.head rxs0
        rxs = V.tail rxs0
        rxs0 = V.reverse xs
    V.foldM_ forward phead xs
    V.foldM_ forward plast rxs
  where turn (x,y) = (negate y,x)
        norm (x,y) = sqrt (x*x + y*y)
        penWidth = boxWidth _penShape
        sentitiveWidth = penWidth * _penSensitivity
        constWidth = penWidth * (1 - _penSensitivity)
        (x1,y1) .-. (x0,y0) = (x1-x0 , y1-y0)
        (x1,y1) .+. (x0,y0) = (x1+x0 , y1+y0)
        z *. (x0,y0) = (z * x0 , z * y0)
        forward pc0@(Sample (Coord x0 y0) z0 _) pc1@(Sample (Coord x y) z _) = do
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
               let shift = turn $ ((1/dist) * (z * sentitiveWidth + constWidth)) *. dp
               P.uncurry Cairo.lineTo $ shift .+. p1
               return pc1

clipNoteData :: NoteData -> Render NoteData
clipNoteData cs = saveEx $ do
  Just r@(G.Rectangle (t->x) (t->y) (t->w) (t->h)) <- G.getClipRectangle
  let box = Box (Coord x y ) (Coord (x+w) (y+h))
  return (filter (overlapBox box . boundingBox) cs)
  where       t = fromIntegral

renderNoteData n = mapM_ drawStroke =<< clipNoteData n

boxRectangle :: Box -> Render ()
boxRectangle (Box lo hi) =
  lo `xy` \lx ly ->
  (hi-lo) `xy`
  rectangle lx ly

type Point = (Double,Double)

menuRootRadius = 40
menuInnerCircle = 50 -- FIXME: rename to Radius
menuOuterCircle = 150

renderNode  :: Bool -> String -> Double -> Coord -> Render ()
renderNode active t rad (Coord cx cy) = saveEx $ do
  Cairo.translate cx cy
  moveTo rad 0
  arc 0 0 rad 0 (2*pi)
  if active then  setSourceRGBA 0.5 0.5 1 1 else setSourceRGBA 1 1 1 1
  fillPreserve
  setFontSize 15
  TextExtents {textExtentsWidth = w, textExtentsHeight = h} <- textExtents t
  moveTo (-w/2) (h/2)
  setSourceRGBA 0 0 0 1 >> showText t >> stroke

renderMenuRoot :: String -> Coord -> Render ()
renderMenuRoot t = renderNode False t menuRootRadius


coordToPt :: Coord -> (Double,Double)
coordToPt (Coord x y) = (x,y)

-- TODO: use a record for the options.
-- | Render a menu, and return the selected option.
-- @renderMenu doRender (x,y) (cx,cy) txts@
--  - a0: the direction of the menu (center), in radians
--  - doRender: render the menu, or just return the selected option?
--  - (x,y): the position of the cursor (determines which option is selected)
--  - (cx,cy): center of the dial
--  - txts: options to render (by text)
renderMenu :: Bool -> Double -> Coord -> Coord -> [String] -> Render (Maybe Int)
renderMenu doRender a0 p c txts = renderDial doRender a0 (coordToPt p) (coordToPt c) 5 menuInnerCircle menuOuterCircle 12 txts

-- | Save excursion
saveEx :: Render a -> Render a
saveEx p = save *> p <* restore

-- TODO: use a record for these options.
-- | Render a menu dial, and return the selected option.
-- @renderDial doRender a0 (x,y) (cx,cy) dx inner outer n txts@
--  - doRender: render the menu, or just return the selected option?
--  - a0: the direction of the menu (center), in radians
--  - (x,y): the position of the cursor (determines which option is selected)
--  - (cx,cy): center of the dial
--  - dx: distance between the options
--  - inner,outer : inner and outer radiuses
--  - n: nominal number of options (determines the size and placement of the options)
--  - txts: options to render (by text)
renderDial :: Bool -> Double -> Point -> Point -> Double -> Double -> Double -> Int -> [String] -> Render (Maybe Int)
renderDial doRender theta (x,y) (cx,cy) dx inner outer n txts = saveEx $ do
  let angles :: [Double]
      angles = map (+shift) [0,-2*pi/(fromIntegral n).. -2*pi]
      shift = theta + pi*fromIntegral (length txts)/(fromIntegral n)
      daIn = dx/inner
      daOut = dx/outer
  Cairo.translate cx cy
  actives <- forM (zip3 txts angles (rot angles)) $ \(t,a0,a1) -> do
     newPath
     setSourceRGBA 0 0 0 1
     arcNegative 0 0 inner (a0-daIn) (a1+daIn)
     arc 0 0 outer (a1+daOut) (a0-daOut)
     closePath
     active <- inFill (x-cx) (y-cy)
     when doRender $ do
       setFillRule FillRuleWinding
       if active
         then setSourceRGBA 0.5 0.5 1 1
         else setSourceRGBA 1 1 1 1
       fillPreserve
       setSourceRGBA 0 0 0 1
       stroke
     saveEx $ do
       Cairo.rotate ((a0+a1)/2)
       moveTo (inner+5) 5
       setSourceRGBA 0 0 0 1
       setFontSize 15
       when doRender $ showText t
     return active
  return $ findIndex id actives

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
  mapM_ drawStrokeSelected =<< clipNoteData cs
