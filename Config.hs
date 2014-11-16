{-# LANGUAGE OverloadedStrings #-}
module Config (loadConfig, WNConfig(..),Devices(..),defaultPen,configuredPens) where

import Data.Configurator.Types
import Data.Configurator as Conf
import Control.Applicative
import Data.String
import NoteData

data WNConfig = WNConfig Devices
data Devices = Devices {touch,multitouch,stylus,eraser :: String}

lkString :: Config -> String -> IO String
lkString cfg s = lookupDefault (fromString s) cfg (fromString s)

loadConfig :: IO WNConfig
loadConfig = do
  cfg <- load [Required "$(HOME)/.whynote"]
  
  WNConfig <$> (Devices
           <$> lkString cfg "device.touch"
           <*> lkString cfg "device.multitouch"
           <*> lkString cfg "device.stylus"
           <*> lkString cfg "device.eraser")

mkColor r g b = Color (r/256) (g/256) (b/256) 1
redColor = mkColor 220 50 47
blueColor = mkColor 38 139 210
greenColor = mkColor 65 133 153
highlightColor = Color 1 1 0 0.5

configuredPens = [("default", defaultPen),
                  ("hilighter",PenOptions 20 highlightColor 0.2)]
