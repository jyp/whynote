{-# OPTIONS -Wall #-}
{-# LANGUAGE ViewPatterns #-}
import Prelude ()
import WNPrelude
import Graphics.Rendering.Cairo
import GtkProcess
import System.Environment
import Render
import NoteData

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input,output,read -> w, read -> h] -> do
      noteData <- loadState input
      let bbox@(Box (Coord x0 y0 _ _) _) = boundingBox noteData
          factor = max (boxWidth bbox / w) (boxHeight bbox / h)
          tr = negate (Translation factor x0 y0)
      withSVGSurface output w h $ \surface ->
        renderWith surface $ renderNoteData ((apply tr <$>) <$> noteData)
    _ -> putStrLn "usage: y02svg <inputfile.y0> <outputfile.svg> <width> <height>"
