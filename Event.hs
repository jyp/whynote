module Event where

import Data.Word

-- | 
data Source = Core | Stylus | Eraser | Touch
                 deriving (Show,Eq,Ord)

data PointerCoord = PointerCoord { pointerX :: Double
                                 , pointerY :: Double
                                 , pointerZ :: Double
                                 , pointerT :: Word32
                                 }
                  deriving (Show,Eq,Ord)

data EventType = Press | Motion | Release
           deriving (Show,Eq)

data Event = Event
             {
               eventSource :: Source,
               eventButton :: Int,
               eventModifiers :: Word32,
               eventType :: EventType,
               eventCoord :: PointerCoord
             }
           deriving (Show,Eq)
