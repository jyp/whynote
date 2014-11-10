{-# LANGUAGE OverloadedStrings #-}
module File where
import qualified Data.ByteString.Lazy as B
import NoteData
import Data.Aeson
import Control.Applicative

data NoteFile = NoteFile NoteData

instance FromJSON NoteFile where
    parseJSON (Object a) = NoteFile <$> a.: "strokes"
    parseJSON _ = empty

instance ToJSON NoteFile where
  toJSON (NoteFile s)= object ["strokes" .= s]

readNote :: FilePath -> IO NoteFile
readNote f = do
  d <- eitherDecode <$> B.readFile f
  case d of
    Left err -> error err
    Right r -> return r

writeNote :: FilePath -> NoteFile -> IO ()
writeNote f d = do
  B.writeFile f $ encode d
