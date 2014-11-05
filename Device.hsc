{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Device 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
-- Copyright   : (c) 2014 JP Bernardy
--

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
import Graphics.UI.Gtk as Gtk
import Unsafe.Coerce
import Data.Word
import Event
--
import Graphics.UI.Gtk.Abstract.Widget

data DeviceList = DeviceList { dev_stylus     :: CInt
                             , dev_eraser     :: CInt
                             , dev_touch      :: CInt
                             } 
                deriving Show 

-- | 
foreign import ccall "c_initdevice.h initdevice" c_initdevice
  ::
     Ptr Widget -- ^ widget
  -> Ptr CInt -- ^ stylus
  -> Ptr CInt -- ^ eraser
  -> Ptr CInt -- ^ touch 
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
    with 0 $ \pstylus ->
      with 0 $ \peraser ->
        with 0 $ \ptouch -> do
          pstylusname <- newCString stylus
          perasername <- newCString eraser
          ptouchname <- newCString touch

          c_initdevice w pstylus peraser ptouch pstylusname perasername ptouchname

          stylus_val <- peek pstylus
          eraser_val <- peek peraser
          touch_val <- peek ptouch
          return $ DeviceList stylus_val eraser_val touch_val

class HasCoords t

instance HasCoords EButton
instance HasCoords EMotion


-- |
getPointer :: DeviceList -> Ptr t -> IO Event
getPointer devlst ptr = do
    let dev = gdk_event_get_source_device ptr
    (_ty,btn,modifiers,x,y,axf,typ,time) <- getInfo ptr
    (source,pcoord) <- coord x y dev axf
    return $ Event source (fromIntegral btn) modifiers typ pcoord {pointerT = time}
  where
    getInfo :: Ptr t -> IO (Int32, Int32, Word32, Double, Double, Ptr CDouble,EventType,TimeStamp)
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
          mods <- #{peek GdkEventButton, state} ptr
          time <- #{peek GdkEventButton, time} ptr
          return (ty,btn,mods,realToFrac x,realToFrac y,axis,
                  if ty == #{const GDK_BUTTON_RELEASE} then Event.Release else Press,
                  time)
        else if ty `elem` [ #{const GDK_MOTION_NOTIFY} ]
        then do
          (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventMotion, x} ptr
          (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventMotion, y} ptr
          axis <- #{peek GdkEventMotion, axes} ptr
          mods <- #{peek GdkEventButton, state} ptr
          time <- #{peek GdkEventMotion, time} ptr
          return (ty,0,mods,realToFrac x, realToFrac y,axis,Motion,time)
        else error ("eventCoordinates: none for event type "++show ty)
    coord :: Double ->  Double -> Device -> Ptr CDouble -> IO (Source,PointerCoord)
    coord x y device ptrax
          | device == dev_stylus devlst = do
            (wacomx :: Double) <- peekByteOff ptrax 0
            (wacomy :: Double) <- peekByteOff ptrax 8
            (wacomz :: Double) <- peekByteOff ptrax 16
            return $ (Stylus,PointerCoord wacomx wacomy wacomz 0)
          | device == dev_eraser devlst = do
            (wacomx :: Double) <- peekByteOff ptrax 0
            (wacomy :: Double) <- peekByteOff ptrax 8
            (wacomz :: Double) <- peekByteOff ptrax 16
            return $ (Eraser,PointerCoord wacomx wacomy wacomz 0)
            -- Touch may be provided by touch screen -- not wacom device -- so no pressure here.
          | device == dev_touch devlst =
             return $ (Touch,PointerCoord x y 1.0 0)
          | otherwise = return $ (Core,PointerCoord x y 1.0 0)
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
