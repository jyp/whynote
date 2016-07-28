module Event (module Event, Word32) where

import NoteData
import Data.Word

-- |
data Source = Core | Stylus | Eraser | MultiTouch | Touch | Timeout
                 deriving (Show,Eq,Ord)

data EventType = Press | Motion | Release
               | Begin | Update | End | Cancel
           deriving (Show,Eq)

data Event = Event
             {
               eventSource :: Source,
               eventButton :: Int,
               eventModifiers :: Word32,
               eventType :: EventType,
               eventCoord :: Coord
             }
           deriving (Show,Eq)

eventTime :: Event -> Word32
eventTime = coordT . eventCoord
