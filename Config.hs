module Config where

data WNConfig = WNConfig {core,touch,stylus,eraser :: String}

wnConfig = WNConfig
                {core = "SynPS/2 Synaptics TouchPad"
                ,touch = "ELAN Touchscreen"
                ,stylus = "Wacom ISDv4 EC Pen stylus"
                ,eraser = "Wacom ISDv4 EC Pen eraser"
                }
