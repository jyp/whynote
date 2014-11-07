{-# LANGUAGE OverloadedStrings #-}
module File where

import NoteData
import Data.Aeson
import Control.Applicative
import Control.Monad

data NoteFile = NoteFile [Coord]

instance FromJSON NoteFile where
    parseJSON (Object a) = NoteFile <$> a.: "strokes"
    parseJSON _ = empty

instance ToJSON NoteFile where
  toJSON (NoteFile s)= object ["strokes" .= s]

instance FromJSON Stroke where
    parseJSON (Stroke a) = Stroke <$> a.: "points"
    parseJSON _ = empty

instance ToJSON Stroke where
   toJSON (Stroke a) = object ["points" .=  a]
