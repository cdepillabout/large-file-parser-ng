{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Conduit (Sink, (=$=), ($$), sourceFile)
import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Attoparsec.ByteString.Char8
       (Parser, decimal, endOfLine, isAlpha_ascii, skipSpace, string,
        takeWhile1)
import Data.Conduit.Attoparsec (conduitParser)
import qualified Data.Conduit.Combinators as ConduitCombinators
import Data.Data (Data)
import Data.Functor (($>))
import Data.Typeable (Typeable)

import Types
       (DataLine(..), FinalLine(..), HeaderLine(..), Line(..),
        exampleFile)

parserLine :: Parser Line
parserLine =
  (fmap HeaderLine parserHeaderLine <|>
  fmap DataLine parserDataLine <|>
  fmap FinalLine parserFinalLine) <* endOfLine

parserHeaderLine :: Parser HeaderLine
parserHeaderLine = string "00 header" $> HL

parserDataLine :: Parser DataLine
parserDataLine = do
  void $ string "01 "
  quote <- takeWhile1 isAlpha_ascii
  skipSpace
  price <- decimal
  pure $ DL {..}

parserFinalLine :: Parser FinalLine
parserFinalLine = string "99 final " *> fmap FL decimal

data StreamResult
  = Beginning
  | Cont Int
  | Succeed
  | ErrLengthNotEqual
  | ErrFileIsBad
  deriving (Data, Eq, Read, Show, Typeable)

sink
  :: forall m x.
     MonadIO m
  => Sink (x, Line) m StreamResult
sink = ConduitCombinators.foldl f Beginning
  where
    f :: StreamResult -> (x, Line) -> StreamResult
    f Beginning      (_, HeaderLine HL)           = Cont 0
    f (Cont lineNum) (_, DataLine{})              = Cont (lineNum + 1)
    f (Cont lineNum) (_, FinalLine (FL finalLen)) =
      if lineNum /= finalLen
        then ErrLengthNotEqual
        else Succeed
    f _              _                            = ErrFileIsBad

streamingFileParser
  :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
  => m ()
streamingFileParser = do
  streamRes <-
    runResourceT $ sourceFile exampleFile =$= conduitParser parserLine $$ sink
  liftIO $ print streamRes

main :: IO ()
main = streamingFileParser
