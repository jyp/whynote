{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Device 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

#include <gtk/gtk.h>
#include "template-hsc-gtk2hs.h"

module Device where

import Control.Applicative 
import Control.Monad.Reader
import qualified Config
import Data.Int
import Foreign.C
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Graphics.UI.Gtk
import Unsafe.Coerce
--
import Graphics.UI.Gtk.Abstract.Widget
-- | 
data PointerType = Core | Stylus | Eraser | Touch 
                 deriving (Show,Eq,Ord)

-- |
data PenButton = PenButton1 | PenButton2 | PenButton3 | EraserButton | TouchButton | Unknown
               deriving (Show,Eq,Ord)

-- | 

data DeviceList = DeviceList { dev_core       :: CInt
                             , dev_stylus     :: CInt
                             , dev_eraser     :: CInt
                             , dev_touch      :: CInt
                             } 
                deriving Show 
                  
-- | 

data PointerCoord = PointerCoord { pointerType :: PointerType 
                                 , pointerX :: Double 
                                 , pointerY :: Double 
                                 , pointerZ :: Double
                                 } 
                  | NoPointerCoord
                  deriving (Show,Eq,Ord)

-- | 
foreign import ccall "c_initdevice.h initdevice" c_initdevice
  ::
     Ptr Widget -- ^ widget
  -> Ptr CInt -- ^ core 
  -> Ptr CInt -- ^ stylus
  -> Ptr CInt -- ^ eraser
  -> Ptr CInt -- ^ touch 
  -> CString  -- ^ core 
  -> CString  -- ^ stylus
  -> CString  -- ^ eraser
  -> CString  -- ^ touch 
  -> IO ()


foreign import ccall "gdk_event_get_source_device" gdk_event_get_source_device
  :: Ptr t -> Device

-- | 
initDevice :: Widget -> Config.WNConfig -> IO DeviceList  
initDevice widget (Config.WNConfig{..}) =
 withForeignPtr (unsafeCoerce widget :: ForeignPtr Widget) $ \w ->
  with 0 $ \pcore ->
    with 0 $ \pstylus ->
      with 0 $ \peraser ->
        with 0 $ \ptouch -> do
          pcorename <- newCString core
          pstylusname <- newCString stylus
          perasername <- newCString eraser
          ptouchname <- newCString touch 

          c_initdevice w pcore pstylus peraser ptouch pcorename pstylusname perasername ptouchname

          core_val <- peek pcore
          stylus_val <- peek pstylus
          eraser_val <- peek peraser
          touch_val <- peek ptouch
          return $ DeviceList core_val stylus_val eraser_val touch_val

class HasCoords t

instance HasCoords EButton
instance HasCoords EMotion


-- |
getPointer :: DeviceList -> Ptr t -> IO (PenButton,PointerCoord)
getPointer devlst ptr = do
    let dev = gdk_event_get_source_device ptr
    (_ty,btn,x,y,axf) <- getInfo ptr
    pcoord <- coord x y dev axf 
    let rbtn = case pointerType pcoord of 
          Eraser -> EraserButton
          Touch  -> TouchButton
          _ -> case btn of
                 1 -> PenButton1
                 2 -> PenButton2 
                 3 -> PenButton3
                 _ -> Unknown
    return (rbtn, pcoord)
  where
    getInfo :: Ptr t -> IO (Int32, Int32, Double, Double, Ptr CDouble)
    getInfo ptr = do 
      (ty :: #{gtk2hs_type GdkEventType}) <- peek (castPtr ptr)
      if ty `elem` [ #{const GDK_BUTTON_PRESS}
                   , #{const GDK_2BUTTON_PRESS}
                   , #{const GDK_3BUTTON_PRESS}
                   , #{const GDK_BUTTON_RELEASE}] 
        then do 
          (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventButton, x} ptr 
          (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventButton, y} ptr
          (btn :: #{gtk2hs_type gint}) <- #{peek GdkEventButton, button} ptr
          axis <- #{peek GdkEventButton, axes} ptr
          return (ty,btn,realToFrac x,realToFrac y,axis)
        else if ty `elem` [ #{const GDK_MOTION_NOTIFY} ] 
        then do
          (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventMotion, x} ptr
          (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventMotion, y} ptr
          axis <- #{peek GdkEventButton, axes} ptr
          return (ty,0,realToFrac x, realToFrac y,axis)
        else error ("eventCoordinates: none for event type "++show ty)
    coord :: Double ->  Double -> Device -> Ptr CDouble -> IO PointerCoord
    coord x y device ptrax
          | device == dev_stylus devlst = do 
            (wacomx :: Double) <- peekByteOff ptrax 0
            (wacomy :: Double) <- peekByteOff ptrax 8
            (wacomz :: Double) <- peekByteOff ptrax 16
            return $ (PointerCoord Stylus wacomx wacomy wacomz)
          | device == dev_eraser devlst = do 
            (wacomx :: Double) <- peekByteOff ptrax 0
            (wacomy :: Double) <- peekByteOff ptrax 8
            (wacomz :: Double) <- peekByteOff ptrax 16 
            return $ (PointerCoord Eraser wacomx wacomy wacomz)
            -- Touch may be provided by touch screen -- not wacom device -- so no pressure here.
          | device == dev_touch devlst =
             return $ PointerCoord Touch x y 1.0
          --   (touchx :: Double) <- peekByteOff ptrax 0
          --   (touchy :: Double) <- peekByteOff ptrax 8
          --   (touchz :: Double) <- peekByteOff ptrax 16 
          --   -- (touchw :: Double) <- peekByteOff ptrax 24
          --   return $ (PointerCoord Touch touchx touchy touchz)            
          | otherwise = return $ PointerCoord Core x y 1.0
type Device = CInt

{-
-- | 
    
wacomCoordConvert :: WidgetClass self => self 
                     -> (Double,Double) 
                     -> IO (Double,Double)
wacomCoordConvert canvas (x,y)= do 
  win <- widgetGetDrawWindow canvas
  (x0,y0) <- drawWindowGetOrigin win
  screen <- widgetGetScreen canvas
  (ws,hs) <- (,) <$> screenGetWidth screen <*> screenGetHeight screen
  return (fromIntegral ws*x-fromIntegral x0,fromIntegral hs*y-fromIntegral y0)
  
-- | 
  
wacomPConvert ::  WidgetClass self => self 
                  -> PointerCoord 
                  -> IO (Double,Double)
wacomPConvert canvas pcoord = do 
 let (px,py) = (,) <$> pointerX <*> pointerY $ pcoord  
 case pointerType pcoord of 
   Core -> return (px,py)
   _ -> do 
     wacomCoordConvert canvas (px,py)

-}
