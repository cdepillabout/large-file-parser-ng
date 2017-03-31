{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor (($>))
import Data.Monoid ((<>))
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import System.IO (readFile)
import Text.Parsec
       (ParsecT, char, digit, letter, many1, newline, runParserT, string)
import Text.Read (readMaybe)

import Types
       (DataLine(..), FinalLine(..), HeaderLine(..), exampleFile)

type Parser = ParsecT String () IO

-- manyLength :: forall s u m a. ParsecT s u m a -> ParsecT s u m Int
-- manyLength p = go 0
--   where
--     go :: Int -> ParsecT s u m Int
--     go !i =
--       ((p *> pure True) <|> pure False) >>=
--         \success -> if success then go (i+1) else pure i

manyLength :: forall s u m a. ParsecT s u m a -> ParsecT s u m Int
manyLength p = go 0
  where
    go :: Int -> ParsecT s u m Int
    go !i =
      ((p *> pure True) <|> pure False) >>=
        (\success -> if success then go (i+1) else pure i)

parserHeaderLine :: forall u m. Monad m => ParsecT String u m HeaderLine
parserHeaderLine = string "00 header" $> HL

parserDataLine :: forall u m. MonadIO m => ParsecT String u m DataLine
parserDataLine = do
  void $ string "01 "
  quote <- encodeUtf8 . pack <$> many1 letter
  void $ char ' '
  price <- parserInt
  -- TODO: Print out the current data line if it was successfully parsed here.
  pure $ DL {..}

parserFinalLine :: forall u m. Monad m => ParsecT String u m FinalLine
parserFinalLine = string "99 final " *> fmap FL parserInt

parserInt :: forall u m. Monad m => ParsecT String u m Int
parserInt = do
  maybeInt <- readMaybe <$> many1 digit
  maybe (fail "Cannot read integer") pure maybeInt

parserFile :: Parser ()
parserFile = do
  void $ parserHeaderLine <* newline
  len <- manyLength (parserDataLine <* newline)
  (FL finalLen) <- parserFinalLine
  when (len /= finalLen) $
    fail "length of the data lines do not equal final line length"

smartFileParser :: IO ()
smartFileParser = do
  file <- readFile exampleFile
  eitherRes <- runParserT parserFile () exampleFile file
  case eitherRes of
    Left parserErr -> putStrLn $ "Got parser error: " <> show parserErr
    Right _ -> putStrLn "Successfully parsed."

main :: IO ()
main = smartFileParser
