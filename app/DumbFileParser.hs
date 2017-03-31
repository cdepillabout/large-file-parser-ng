{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (void)
import Data.Functor (($>))
import Data.Monoid ((<>))
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Text.Parsec ((<|>), char, digit, letter, many1, newline, string)
import Text.Parsec.ByteString (Parser, parseFromFile)
import Text.Read (readMaybe)

import Types
       (DataLine(..), FinalLine(..), FullFile(..), HeaderLine(..),
        exampleFile)

manyLength :: forall a . Parser a -> Parser (Int, [a])
manyLength p = go 0 []
  where
    go :: Int -> [a] -> Parser (Int, [a])
    go i as = (p >>= \a -> go (i + 1) (a:as)) <|> pure (i, as)

parserFile :: Parser FullFile
parserFile = do
  headerLine <- parserHeaderLine <* newline
  (len, dataLines) <- manyLength (parserDataLine <* newline)
  (FL finalLen) <- parserFinalLine
  if len /= finalLen
    then fail "length of the data lines do not equal final line length"
    else
      pure $
        FullFile
          { fullFileHeaderLine = headerLine
          , fullFileDataLine = dataLines
          , fullFileFinalLine = FL finalLen
          }

parserHeaderLine :: Parser HeaderLine
parserHeaderLine = string "00 header" $> HL

parserDataLine :: Parser DataLine
parserDataLine = do
  void $ string "01 "
  quote <- encodeUtf8 . pack <$> many1 letter
  void $ char ' '
  price <- parserInt
  pure $ DL {..}

parserFinalLine :: Parser FinalLine
parserFinalLine = string "99 final " *> fmap FL parserInt

parserInt :: Parser Int
parserInt = do
  maybeInt <- readMaybe <$> many1 digit
  maybe (fail "Cannot read integer") pure maybeInt

dumbFileParser :: IO ()
dumbFileParser = do
  eitherRes <- parseFromFile parserFile exampleFile
  case eitherRes of
    Left parserErr -> putStrLn $ "Got parser error: " <> show parserErr
    Right _ -> putStrLn "Successfully parsed."

main :: IO ()
main = dumbFileParser
