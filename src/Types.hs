{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Text (Text, pack)

exampleFile :: FilePath
exampleFile = "big-example-file"

totalLines :: Int
totalLines = 1000000

data HeaderLine = HL
  deriving (Data, Eq, Read, Show, Typeable)

data DataLine = DL
  { quote :: ByteString
  , price :: Int
  } deriving (Data, Eq, Read, Show, Typeable)

newtype FinalLine = FL
  { unFinalLine :: Int
  } deriving (Data, Eq, Read, Show, Typeable)

data Line
  = HeaderLine HeaderLine
  | DataLine DataLine
  | FinalLine FinalLine
  deriving (Data, Eq, Read, Show, Typeable)

data FullFile = FullFile
  { fullFileHeaderLine :: HeaderLine
  , fullFileDataLine :: [DataLine]
  , fullFileFinalLine :: FinalLine
  } deriving (Data, Eq, Read, Show, Typeable)

tshow :: Show a => a -> Text
tshow = pack . show
