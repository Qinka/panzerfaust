{-# LANGUAGE FlexibleContexts #-}

module Main where

import Text.Parsec
import Data.Char
import Control.Monad


parsingHex :: Stream s m Char
           => ParsecT s u m Int
parsingHex = do
  skipMany $ oneOf " \t"
  string "0x"
  read . ("0x" ++) <$> count 2 hexDigit

dropAfterNewLine x =
  if '\n' `elem` x
    then reverse . dropWhile (/='\n') $ reverse x
    else x

parsingCANFrame :: Stream s m Char
                => ParsecT s String m ()
parsingCANFrame = void $ do
  -- parsing CAN ID
  -- Should be 0x700
  skipMany $ oneOf  " \t"
  skipMany $ noneOf " \t"
  skipMany $ oneOf  " \t"
  -- parsing hex number
  str <- dropAfterNewLine . map chr <$> count 8 parsingHex
  -- add to state
  modifyState $ \str' -> str' ++ str
  skipMany (noneOf "\n") <* char '\n'

skipLine :: Stream s m Char
         => ParsecT s String m ()
skipLine = skipMany (noneOf "\n") <* char '\n'

parsingToString :: Stream s m Char
                => ParsecT s String m String
parsingToString = do
  many $ try parsingCANFrame <|> skipLine
  getState

main :: IO ()
main = do
  text <- getContents
  case runParser parsingToString "" "stdin" text of
    Left e  -> print e
    Right s -> putStrLn s
