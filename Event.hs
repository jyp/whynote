{-# LANGUAGE DeriveFunctor #-}
module Event (module Event, Word32) where

import NoteData
import Data.Word

-- |
data Source = Core | Stylus | Eraser | MultiTouch | Touch | Timeout
                 deriving (Show,Eq,Ord)

data EventType = Press | Motion | Release
               | Begin | Update | End | Cancel
           deriving (Show,Eq)

type Event = Event' Coord
data Event' a = Event
             {
               eventSource :: Source,
               eventButton :: Int,
               eventModifiers :: Word32,
               eventType :: EventType,
               eventSample :: Sample' a
             } deriving (Show,Eq,Functor)
eventPressure = sampleZ . eventSample
eventCoord = sampleCoord . eventSample
eventTime :: Event -> Word32
eventTime = sampleT . eventSample
