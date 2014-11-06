module Event where

import NoteData
import Data.Word

-- | 
data Source = Core | Stylus | Eraser | Touch
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
