{-# LANGUAGE OverloadedStrings #-}


-- | The module provides functions for parsing /Walenty/.


module NLP.Walenty
( Verb (..)
, CertLevel (..)
, Aspect (..)
, Frame
, Item
, Function
, Argument

, readWalenty
, parseWalenty
) where


import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import qualified Data.Char            as C
import           Data.Foldable        (asum)
import           Data.Maybe           (mapMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as L
import qualified Data.Text.Lazy.IO    as L

import           Data.Attoparsec.Text (Parser, string, (<?>))
import qualified Data.Attoparsec.Text as A


-------------------------------------------------------------
-- Types
-------------------------------------------------------------


-- | A verbal lexical entry from /Walenty/.
data Verb = Verb
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


-- | A comment line.
type Comment = Text


-- | A valency frame of the verb.
type Frame = [Item]


-- | An item of the frame.
data Item = Item
  { function :: Maybe Function
  , control  :: Maybe Control
  , argsAlt  :: [Either Lex Argument]
    -- ^ A list of alternative arguments which can
    -- occur on the corresponding frame position
  }
  deriving (Show, Eq, Ord)


-- | Function of the corresponding argument.
data Function
  = Subject
  | Object
  deriving (Show, Eq, Ord)


-- | From /Walenty/ documentation: /control relations are involved e.g.
-- in establishing the source of agreement for adjectival arguments.
data Control
  = Controller
  | Controllee
  deriving (Show, Eq, Ord)


-- | An argument of the frame.
data Arg
  = NP
    { gcase :: Case
      -- ^ Grammatical case
    }
    -- ^ Noun phrase
  | PrepNP
    { prep  :: Text
      -- ^ Preposition
    , gcase :: Case
      -- ^ Grammatical case
    }
    -- ^ Prepositional phrase
  | Other Text
    -- ^ All the other cases, provisionally
  deriving (Show, Eq, Ord)


-- | A lexicalized argument of the frame.
data Lex = Lex
  { lexArg :: Argument
    -- ^ The underlying, lexicalized argument
  , lexNum :: Number
    -- ^ The grammatical number
  }


-- | Grammatical case.  Note that /vocative/ case does not
-- actually occur in /Walenty/, thus it is not represented
-- in the data structure below.
data Case
  = Nominative
  | Genitive
  | Dative
  | Accusative
  | Instrumental
  | Locative
  -- -- | Vocative
  | Structural
    -- ^ Structural case, i.e, the case which depends on
    -- the grammatical function?
  deriving (Show, Eq, Ord)


data Number
  = Singular
  | Plural
  deriving (Show, Eq, Ord)


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
  <?> "lineP"


commentP :: Parser Comment
commentP = A.char '%' *> (T.strip <$> A.takeText)
  <?> "commentP"
-- commentP = A.takeText


-- | A parser for verb lexical entries.
verbP :: Parser Verb
verbP = Verb
  <$> (fieldP  <* breakP)
  <*> (certP   <* breakP)
  <*> (fieldP  <* breakP)
  <*> (fieldP  <* breakP)
  <*> (aspectP <* breakP)
  <*> frameP
  <?> "verbP"


breakP :: Parser ()
breakP = A.char ':' *> A.skipSpace
  <?> "breakP"


-- | Read the entire field, whatever's inside.
fieldP :: Parser Text
fieldP = A.takeTill (==':')
  <?> "fieldP"


certP :: Parser CertLevel
certP = A.choice
  [ Sure <$ string "pewny"
  , Dubious <$ string "wątpliwy"
  , Bad <$ string "zły"
  , Archaic <$ string "archaiczny"
  , Colloquial <$ string "potoczny"
  , Vulgar <$ string "wulgarny" ]
  <?> "certP"


aspectP :: Parser Aspect
aspectP = A.choice
  [ Perfective <$ string "perf"
  , Imperfective <$ string "imperf"
  , UnknownAspect <$ string "_" ]
  <?> "aspectP"


frameP :: Parser Frame
frameP = itemP `A.sepBy1'` A.char '+'
  <?> "frameP"


itemP :: Parser Item
itemP = Item
  <$> A.option Nothing (Just <$> functionP)
  <*> A.option Nothing (Just <$> (comma *> controlP))
  <*> argumentsP
  <?> "itemP"
  where
    comma = void $ A.option ' ' (A.char ',')


functionP :: Parser Function
functionP = A.choice
  [ Subject <$ string "subj"
  , Object <$ string "obj" ]
  <?> "functionP"


controlP :: Parser Control
controlP = A.choice
  [ Controller <$ string "controller"
  , Controllee <$ string "controllee" ]
  <?> "controlP"


argumentsP :: Parser [Argument]
argumentsP = do
  A.char '{'
  args <- argumentP `A.sepBy1'` A.char ';'
  A.char '}'
  return args
  <?> "argumentsP"


argumentP :: Parser Argument
argumentP = A.choice
  [npP, prepNpP, otherP]
  <?> "argumentP"


otherP :: Parser Argument
otherP = Other <$> A.takeTill (`elem` [';', '}'])


npP :: Parser Argument
npP = NP <$> do
  string "np"
  A.char '(' *> caseP <* A.char ')'


prepNpP :: Parser Argument
prepNpP = do
  string "prepnp"
  A.char '('
  p <- A.takeWhile1 C.isLetter
  A.char ','
  c <- caseP
  A.char ')'
  return $ PrepNP p c


caseP :: Parser Case
caseP = A.choice
  [ Nominative <$ string "nom"
  , Genitive <$ string "gen"
  , Dative <$ string "dat"
  , Accusative <$ string "acc"
  , Instrumental <$ string "inst"
  , Locative <$ string "loc"
  , Structural <$ string "str" ]
  <?> "caseP"
