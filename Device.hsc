{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Device
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
-- Copyright   : (c) 2014-2016 JP Bernardy
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
import NoteData
import System.Glib.GError
--
import Graphics.UI.Gtk.Abstract.Widget

type Device = CInt
data DeviceList = DeviceList { dev_stylus     :: Device
                             , dev_eraser     :: Device
                             , dev_touch      :: Device
                             , dev_mtouch     :: Device
                             }
                deriving Show

connect_PTR__BOOL ::
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Ptr a -> IO Bool) ->
  IO (ConnectId obj)
connect_PTR__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> IO Bool
        action _ ptr1 =
          failOnGError $
          user (castPtr ptr1)

eventM :: WidgetClass w => SignalName -> [EventMask] ->
  ConnectAfter -> w -> (EventM t Bool) -> IO (ConnectId w)
eventM name eMask after obj fun = do
  id <- connect_PTR__BOOL name after obj (runReaderT fun)
  widgetAddEvents obj eMask
  return id


touchEvent :: WidgetClass self => Signal self (EventM EAny Bool)
touchEvent = Signal (eventM "touch_event" [TouchMask])

-- |
foreign import ccall "c_initdevice.h initdevice" c_initdevice
  ::
     Ptr Widget -- ^ widget
  -> Ptr Device -- ^ stylus
  -> Ptr Device -- ^ eraser
  -> Ptr Device -- ^ touch
  -> Ptr Device -- ^ mtouch
  -> CString  -- ^ stylus
  -> CString  -- ^ eraser
  -> CString  -- ^ touch
  -> CString  -- ^ mtouch
  -> IO ()


foreign import ccall "gdk_event_get_source_device" gdk_event_get_source_device
  :: Ptr t -> Device

foreign import ccall "gdk_window_set_event_compression" gdk_window_set_event_compression
  :: Ptr (DrawWindow) -> Bool -> IO ()

setEventCompression :: DrawWindow -> Bool -> IO ()
setEventCompression dw x =
  withForeignPtr (unsafeCoerce dw :: ForeignPtr DrawWindow) $ \w ->
    gdk_window_set_event_compression w x

-- |
initDevice :: Widget -> Config.Devices -> IO DeviceList
initDevice widget (Config.Devices{..}) =
  withForeignPtr (unsafeCoerce widget :: ForeignPtr Widget) $ \w ->
    with 0 $ \pstylus ->
      with 0 $ \peraser ->
        with 0 $ \ptouch ->
         with 0 $ \pmtouch -> do
          pstylusname <- newCString stylus
          perasername <- newCString eraser
          ptouchname <- newCString touch
          pmtouchname <- newCString multitouch

          c_initdevice w pstylus peraser ptouch pmtouch pstylusname perasername ptouchname pmtouchname

          stylus_val <- peek pstylus
          eraser_val <- peek peraser
          touch_val <- peek ptouch
          mtouch_val <- peek pmtouch
          return $ DeviceList stylus_val eraser_val touch_val mtouch_val

-- |
getPointer :: DeviceList -> Ptr t -> IO Event
getPointer devlst ptr = do
    let dev = gdk_event_get_source_device ptr
    (_ty,btn,modifiers,x,y,axf,typ,time) <- getInfo ptr
    (source,pcoord) <- coord x y dev axf
    return $ Event source (fromIntegral btn) modifiers typ pcoord {coordT = time}
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
        else if ty == #{const GDK_MOTION_NOTIFY}
        then do
          (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventMotion, x} ptr
          (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventMotion, y} ptr
          axis <- #{peek GdkEventMotion, axes} ptr
          mods <- #{peek GdkEventMotion, state} ptr
          time <- #{peek GdkEventMotion, time} ptr
          return (ty,0,mods,realToFrac x, realToFrac y,axis,Motion,time)
        else if ty >= #{const GDK_TOUCH_BEGIN} && ty <= #{const GDK_TOUCH_CANCEL}
        then do
          let typ = case ty of
                #{const GDK_TOUCH_BEGIN} -> Begin
                #{const GDK_TOUCH_UPDATE} -> Update
                #{const GDK_TOUCH_END} -> End
                #{const GDK_TOUCH_CANCEL} -> Cancel
          (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventTouch, x} ptr
          (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventTouch, y} ptr
          axis <- #{peek GdkEventTouch, axes} ptr
          mods <- #{peek GdkEventTouch, state} ptr
          time <- #{peek GdkEventTouch, time} ptr
          sequ <- #{peek GdkEventTouch, sequence} ptr
          return (ty,sequ,mods,realToFrac x, realToFrac y,axis,typ,time)
        else error ("eventCoordinates: none for event type "++show ty)
    coord :: Double ->  Double -> Device -> Ptr CDouble -> IO (Source,Coord)
    coord x y device ptrax
          | device == dev_stylus devlst = do
            (wacomx :: Double) <- peekByteOff ptrax 0
            (wacomy :: Double) <- peekByteOff ptrax 8
            (wacomz :: Double) <- peekByteOff ptrax 16
            return $ (Stylus,Coord wacomx wacomy wacomz 0)
          | device == dev_eraser devlst = do
            (wacomx :: Double) <- peekByteOff ptrax 0
            (wacomy :: Double) <- peekByteOff ptrax 8
            (wacomz :: Double) <- peekByteOff ptrax 16
            return $ (Eraser,Coord wacomx wacomy wacomz 0)
            -- Touch may be provided by touch screen -- not wacom device -- so no pressure here.
          | device == dev_touch devlst =
             return $ (Touch,Coord x y 1.0 0)
          | device == dev_mtouch devlst =
             return $ (MultiTouch,Coord x y 1.0 0)
          | otherwise = return $ (Core,Coord x y 1.0 0)
