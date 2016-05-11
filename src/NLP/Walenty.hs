{-# LANGUAGE OverloadedStrings #-}


-- | The module provides functions for parsing /Walenty/.


module NLP.Walenty
( Verb (..)
, CertLevel (..)
, Aspect (..)
, Frame

, readWalenty
, parseWalenty
) where


import           Control.Applicative  ((<|>))
import qualified Data.Char            as C
import           Data.Foldable        (asum)
import           Data.Maybe           (mapMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as L
import qualified Data.Text.Lazy.IO    as L

import           Data.Attoparsec.Text (Parser, string)
import qualified Data.Attoparsec.Text as A



-------------------------------------------------------------
-- Types
-------------------------------------------------------------


-- | A verbal lexical entry from /Walenty/.
data Verb =
  Verb
  { base      :: Text
                 -- ^ Base form of the verb
  , certitude :: CertLevel
                 -- ^ Level of certitude of the entry
  , field3    :: Text
                 -- ^ Unknown
  , field4    :: Text
                 -- ^ Unknown
  , aspect    :: Aspect
                 -- ^ Aspect of the verb
  , frame     :: Frame
                 -- ^ Valency frame of the verb
  } deriving (Show, Eq, Ord)


-- | A certitude level attached to a lexical entry.
data CertLevel
  = Sure
    -- ^ corresponding to /pewny/ in /Walenty/
  | Dubious
    -- ^ /wątpliwy/
  | Bad
    -- ^ /zły/
  | Archaic
    -- ^ /archaiczny/
  | Colloquial
    -- ^ /potoczny/
  | Vulgar
    -- ^ /wulgarny/
  deriving (Show, Eq, Ord)


-- | An aspect of the verb.
data Aspect
  = Perfective
  | Imperfective
  | UnknownAspect
  deriving (Show, Eq, Ord)


-- | A valency frame of the verb.
type Frame = Text


-- | A comment line.
type Comment = Text


-------------------------------------------------------------
-- Parser
-------------------------------------------------------------


-- | Read Walenty file (verb entries only).
readWalenty :: FilePath -> IO [Either Verb Comment]
readWalenty path = parseWalenty <$> L.readFile path


-- | Parse Walenty file (verb entries only).
parseWalenty :: L.Text -> [Either Verb Comment]
parseWalenty
  = map (takeRight . parseLine . L.toStrict)
  . filter (not . L.null)
  . map (L.dropAround $ \c -> C.isSpace c || not (C.isPrint c))
  . L.lines
  where
    parseLine = A.parseOnly $ lineP <* A.endOfInput
    takeRight (Right x) = x
    takeRight (Left e) = error e


-- | A line parser (either a verb entry or a comment).
lineP :: Parser (Either Verb Comment)
lineP
  =  Right <$> commentP
 <|> Left  <$> verbP


commentP :: Parser Comment
commentP = A.char '%' *> (T.strip <$> A.takeText)
-- commentP = A.takeText


-- | A parser for verb lexical entries.
verbP :: Parser Verb
verbP = Verb
  <$> (fieldP <* breakP)
--   <*> (pure Sure)
  <*> (certP <* breakP)
--   <*> (pure "field3")
  <*> (fieldP <* breakP)
--   <*> (pure "field4")
  <*> (fieldP <* breakP)
--   <*> (pure Perfective)
  <*> (aspectP <* breakP)
  <*> frameP


breakP :: Parser ()
breakP = A.char ':' *> A.skipSpace


-- | Read the entire field, whatever's inside.
fieldP :: Parser Text
fieldP = A.takeTill (==':')


certP :: Parser CertLevel
certP = asum
  [ Sure <$ string "pewny"
  , Dubious <$ string "wątpliwy"
  , Bad <$ string "zły"
  , Archaic <$ string "archaiczny"
  , Colloquial <$ string "potoczny"
  , Vulgar <$ string "wulgarny" ]


aspectP :: Parser Aspect
aspectP = asum
  [ Perfective <$ string "perf"
  , Imperfective <$ string "imperf"
  , UnknownAspect <$ string "_" ]


frameP :: Parser Text
frameP = A.takeText
