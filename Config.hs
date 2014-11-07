{-# LANGUAGE OverloadedStrings #-}
module Config (loadConfig, WNConfig(..)) where

import Data.Configurator.Types
import Data.Configurator as Conf
import Control.Applicative
import Data.String

data WNConfig = WNConfig {touch,multitouch,stylus,eraser :: String}

lkString :: Config -> String -> IO String
lkString cfg s = lookupDefault (fromString s) cfg (fromString s)

loadConfig :: IO WNConfig
loadConfig = do
  cfg <- load [Required "$(HOME)/.whynote"]
  
  WNConfig <$> lkString cfg "device.touch"
           <*> lkString cfg "device.multitouch"
           <*> lkString cfg "device.stylus"
           <*> lkString cfg "device.eraser"
