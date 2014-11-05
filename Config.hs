{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Configurator.Types
import Data.Configurator
import Control.Applicative

data WNConfig = WNConfig {touch,stylus,eraser :: String}


loadConfig :: IO WNConfig
loadConfig = do
  cfg <- load [Required "$(HOME)/.whynote"]
  
  WNConfig <$> require  cfg "device.touch"
           <*> require  cfg "device.stylus"
           <*> require  cfg "device.eraser"
