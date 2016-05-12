{-# LANGUAGE OverloadedStrings #-}


-- | Module responsible for parsing phrase type expansions.


module NLP.Walenty.Expand
( Expansion (..)
, readExpansions
, parseExpansions
) where


import           Control.Monad (void)

import           Data.Text     (Text)
import qualified Data.Text     as T
-- import qualified Data.Text.IO  as T

-- import           Data.Attoparsec.Text (Parser, string, (<?>))
-- import qualified Data.Attoparsec.Text as A
import           Text.Parsec   (string, (<?>))
import qualified Text.Parsec   as A


-- | Phrase type expansion.
data Expansion = Expansion
  { from :: Text
    -- ^ The phrase type to expand
  , to   :: [Text]
    -- ^ To what `from` can be expanded (TODO: parse the
    -- corresponding certitute info)
  } deriving (Show, Eq, Ord)


-- | Parser type.
type Parser = A.Parsec String ()


-- | Read expansion file.
readExpansions :: FilePath -> IO [Expansion]
readExpansions path = parseExpansions path <$> readFile path


-- | Parse expansion file.
parseExpansions :: FilePath -> String -> [Expansion]
parseExpansions path =
  check . A.parse (expansionsP <* endOfInput) path
  where
    check (Right x) = x
    check (Left e) = error (show e)


expansionsP :: Parser [Expansion]
expansionsP = do
  x  <- expansionP
  xs <- A.option [] expansionsP
  return (x:xs)
  <?> "expansionsP"


expansionP :: Parser Expansion
expansionP = do
  typ <- takeTill (=='-')
  void $ string "-->"
  void $ A.endOfLine
  eto <- toP
  return $ Expansion typ eto
  <?> "expansionP"


toP :: Parser [Text]
toP = do
  void $ string "    "
  x <- takeTill (=='\t')
  A.skipMany1 A.space
  void $ certP
  void $ A.endOfLine
  xs <- A.option [] toP
  return (x:xs)
  <?> "toP"


certP :: Parser Text
certP = do
  void $ A.char '['
  cert <- takeTill (==']')
  void $ A.char ']'
  return cert
  <?> "certP"


-------------------------------------------------------------
-- Parsing compatibility layer
-------------------------------------------------------------


-- | End of input parser.
endOfInput :: Parser ()
endOfInput = A.eof


-- | Take till parser.
takeTill :: (Char -> Bool) -> Parser Text
takeTill p = T.pack <$> A.manyTill A.anyChar (A.lookAhead $ A.satisfy p)
