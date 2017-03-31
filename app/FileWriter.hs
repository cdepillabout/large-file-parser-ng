{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit (Producer, ($$), sinkFile, yield, yieldMany)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)

import Types (exampleFile, totalLines, tshow)


fileProducer :: Monad m => Producer m ByteString
fileProducer = do
  yield "00 header\n"
  yieldMany dataLines
  yield $ "99 final " <> encodeUtf8 (tshow totalLines) <> "\n"
  where
    dataLines :: [ByteString]
    dataLines =
      fmap
        (\n -> "01 data " <> encodeUtf8 (tshow n) <> "\n")
        [1 .. totalLines]

fileWriter
  :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
  => m ()
fileWriter =
  runResourceT $ fileProducer $$ sinkFile exampleFile

main :: IO ()
main = fileWriter
