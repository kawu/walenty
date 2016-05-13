{-# LANGUAGE OverloadedStrings #-}


-- | The module provides functions for parsing /Walenty/.


module NLP.Walenty
( Verb (..)
, CertLevel (..)
, Aspect (..)
, Frame
, Argument (..)
, Function (..)
, Phrase (..)

, readWalenty
, parseWalenty
) where


import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import qualified Data.Char            as C
import qualified Data.Map.Strict      as M
-- import           Data.Foldable        (asum)
-- import           Data.Maybe           (mapMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as L
import qualified Data.Text.Lazy.IO    as L

import           Data.Attoparsec.Text (Parser, string, (<?>))
import qualified Data.Attoparsec.Text as A

import qualified NLP.Walenty.Expand   as E


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
type Frame = [Argument]


-- | An item of the frame.
data Argument = Argument
  { function  :: Maybe Function
  , control   :: Maybe Control
  , phraseAlt :: [Phrase]
    -- ^ A list of phrase types which can
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


-- | An phrase type occuring on a specific position of the frame.
data Phrase
  = NP
    { caseG       :: Case
      -- ^ Grammatical case
    , number      :: Maybe Number
      -- ^ Number (if specified)
    , lexicalHead :: [Text]
      -- ^ Lexical (semantic) head (if specified)
    , dependents  :: Attribute
      -- ^ Dependents (if specified)
    }
    -- ^ Noun phrase
  | PrepNP
    { preposition :: Text
      -- ^ Preposition
    , caseG       :: Case
      -- ^ Grammatical case
    , number      :: Maybe Number
      -- ^ Number (if specified)
    , lexicalHead :: [Text]
      -- ^ Lexical head (if specified)
    , dependents  :: Attribute
      -- ^ Dependents (if specified)
    }
    -- ^ Prepositional phrase
  | CP
    { complementizer :: Text
      -- ^ complementizer type (e.g., 'że', 'żeby', 'int')
      -- TODO: to be represented with a type?
    , negation       :: Maybe Negation
      -- ^ Number (if specified)
    , lexicalHead    :: [Text]
      -- ^ Lexical head (if specified)
    , cpUnknown      :: Maybe Text
      -- ^ "się"?
    , dependents     :: Attribute
      -- ^ Dependents (if specified)
    }
    -- ^ Bare complementiser clause
  | NCP
    { complementizer :: Text
      -- ^ complementizer type (e.g., 'że', 'żeby', 'int')
    , caseG          :: Case
      -- ^ Grammatical case
    , negation       :: Maybe Negation
      -- ^ Number (if specified)
    , lexicalHead    :: [Text]
      -- ^ Lexical head (if specified)
    , cpUnknown      :: Maybe Text
      -- ^ "się"?
    , dependents     :: Attribute
      -- ^ Dependents (if specified)
    }
    -- ^ Complementiser clauses with a correlative pronoun
  | PrepNCP
    { preposition    :: Text
      -- ^ Preposition
    , complementizer :: Text
      -- ^ complementizer type (e.g., 'że', 'żeby', 'int')
    , caseG          :: Case
      -- ^ Grammatical case
    , negation       :: Maybe Negation
      -- ^ Number (if specified)
    , lexicalHead    :: [Text]
      -- ^ Lexical head (if specified)
    , cpUnknown      :: Maybe Text
      -- ^ "się"?
    , dependents     :: Attribute
      -- ^ Dependents (if specified)
    }
    -- ^ Prepositional phrase involving a complementiser clause
    -- with a correlative pronoun
  | AdjP
    { caseG       :: Case
      -- ^ Grammatical case
    , adjNumber   :: Maybe (Agree Number)
      -- ^ Number (if specified)
    , gender      :: Maybe (Agree Gender)
      -- ^ Gender (if specified)
    , degree      :: Maybe Degree
      -- ^ Degree (if specified)
    , lexicalHead :: [Text]
      -- ^ Lexical head (if specified)
    , dependents  :: Attribute
      -- ^ Dependents (if specified)
    }
    -- ^ Adjectival phrase
  | PrepNumP
    { preposition   :: Text
      -- ^ Preposition
    , caseG         :: Case
      -- ^ Grammatical case
    , lexicalNumber :: [Text]
      -- ^ Number (if specified)
    , lexicalHead   :: [Text]
      -- ^ Lexical head (if specified)
    , dependents    :: Attribute
      -- ^ Dependents (if specified)
    }
    -- ^ Prepositional numeral(?) phrase
  | PrepAdjP
    { preposition :: Text
      -- ^ Preposition
    , caseG       :: Case
      -- ^ Grammatical case
    , adjNumber   :: Maybe (Agree Number)
      -- ^ Number (if specified)
    , gender      :: Maybe (Agree Gender)
      -- ^ Gender (if specified)
    , degree      :: Maybe Degree
      -- ^ Degree (if specified)
    , lexicalHead :: [Text]
      -- ^ Lexical head (if specified)
    , dependents  :: Attribute
      -- ^ Dependents (if specified)
    }
    -- ^ Prepositional adjectival phrase(?)
  | Other Text
    -- ^ All the other cases, provisionally
  deriving (Show, Eq, Ord)


data Agree a
  = Agree
  | Value a
  deriving (Show, Eq, Ord)


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
  | Partitive
  | Agreement
  | PostPrep
  | Predicative
  deriving (Show, Eq, Ord)


-- | Grammatical number.
data Number
  = Singular
  | Plural
  deriving (Show, Eq, Ord)


-- | Grammatical gender.
data Gender
  = M1
  | M2
  | M3
  | F
  | N
  deriving (Show, Eq, Ord)


-- | Grammatical degree.
data Degree
  = Pos
  | Com
  | Sup
  deriving (Show, Eq, Ord)


-- | Negation.
data Negation
  = Neg
  | Aff
  deriving (Show, Eq, Ord)


-- | Attribute is used to specify dependents of the given phrase.
--
-- TODO: I've checked that dependents can be specified for their
-- function; can they be specified for the control as well?
-- (I didn't find any example).
data Attribute
  = NAtr
    -- ^ No dependents allowed
  | Atr Frame
    -- ^ Optional dependent arguments; if none is specified,
    -- any argument is allowed.
  | Atr1 Frame
    -- ^ Like `Atr`, but at most one dependent
  | RAtr Frame
    -- ^ Required attribute
  | RAtr1 Frame
    -- ^ Like `RAtr`, but at most one dependent
  deriving (Show, Eq, Ord)


-------------------------------------------------------------
-- Parser
-------------------------------------------------------------


-- | Read Walenty file (verb entries only).
readWalenty
  :: FilePath -- ^ Expand file
  -> FilePath -- ^ Walenty proper
  -> IO [Either Verb Comment]
readWalenty expPath walPath = do
  expMap <- E.readExpansions expPath
  parseWalenty expMap <$> L.readFile walPath


-- | Parse Walenty file (verb entries only).
parseWalenty :: E.ExpansionMap -> L.Text -> [Either Verb Comment]
parseWalenty expMap
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


    frameP :: Parser Frame
    frameP = argumentP `A.sepBy1'` A.char '+'
      <?> "frameP"


    argumentP :: Parser Argument
    argumentP = Argument
      <$> A.option Nothing (Just <$> functionP)
      <*> A.option Nothing (Just <$> (spaceComma *> controlP))
      <*> phrasesP
      <?> "argumentP"
      where
        spaceComma = void $ A.option ' ' (A.char ',')


    phrasesP :: Parser [Phrase]
    phrasesP = concat <$>
      let p = phraseP `A.sepBy1'` A.char ';'
      in  between '{' '}' p
      <?> "phrasesP"


    phraseP :: Parser [Phrase]
    phraseP = (fmap (:[]) . A.choice)
      [ npP, prepNpP, cpP, ncpP, prepNcP
      , adjpP, prepNumpP, prepAdjpP ]
      <|> otherP
      <?> "phraseP"


    npP :: Parser Phrase
    npP = regNpP <|> lexNpP
      <?> "npP"
      where
        regNpP = string "np" *> do
          cas <- between '(' ')' caseP
          return $ NP cas Nothing [] (Atr [])
          <?> "regNpP"
        lexNpP = string "lex" *> do
          between '(' ')' $ string "np" *> do
            cas <- between '(' ')' caseP
            num <- comma *> maybe_ numberP
            lks <- comma *> lexicalHeadsP
            atr <- comma *> attributeP
            return $ NP cas num lks atr
          <?> "lexNpP"


    prepNpP :: Parser Phrase
    prepNpP = plain <|> lexicalized
      <?> "prepNpP"
      where
        plain = string "prepnp" *> do
          between '(' ')' $ do
            -- prp <- A.takeWhile1 C.isLetter
            prp <- prepP
            cas <- A.char ',' *> caseP
            return $ PrepNP prp cas Nothing [] (Atr [])
          <?> "plain PrepNP"
        lexicalized = string "lex" *> do
          between '(' ')' $ string "prepnp" *> do
            (prp, cas) <- between '(' ')' $ do
              -- prp <- A.takeWhile1 C.isLetter
              prp <- prepP
              cas <- comma *> caseP
              return (prp, cas)
            num <- comma *> maybe_ numberP
            lks <- comma *> lexicalHeadsP
            atr <- comma *> attributeP
            return $ PrepNP prp cas num lks atr
          <?> "lexicalized PrepNP"


    cpP :: Parser Phrase
    cpP = plain <|> lexicalized
      <?> "CP"
      where
        plain = string "cp" *> do
          between '(' ')' $ do
            cmp <- compP
            return $ CP cmp Nothing [] Nothing (Atr [])
          <?> "plain CP"
        lexicalized = string "lex" *> do
          between '(' ')' $ string "cp" *> do
            cmp <- between '(' ')' compP
            neg <- comma *> maybe_ negationP
            lks <- comma *> lexicalHeadsP
            unk <- comma *> A.takeTill (==',')
            atr <- comma *> attributeP
            return $ CP cmp neg lks (Just unk) atr
          <?> "lexicalized CP"


    ncpP :: Parser Phrase
    ncpP = plain <|> lexicalized
      <?> "NCP"
      where
        plain = string "ncp" *> do
          between '(' ')' $ do
            cas <- caseP
            cmp <- comma *> compP
            return $ NCP cmp cas Nothing [] Nothing (Atr [])
          <?> "plain NCP"
        lexicalized = string "lex" *> do
          between '(' ')' $ string "ncp" *> do
            (cas, cmp) <- between '(' ')' $ do
              cas <- caseP
              cmp <- comma *> compP
              return (cas, cmp)
            neg <- comma *> maybe_ negationP
            lks <- comma *> lexicalHeadsP
            unk <- comma *> A.takeTill (==',')
            atr <- comma *> attributeP
            return $ NCP cmp cas neg lks (Just unk) atr
          <?> "lexicalized NCP"


    prepNcP :: Parser Phrase
    prepNcP = plain <|> lexicalized
      <?> "PrepNCP"
      where
        plain = string "prepncp" *> do
          between '(' ')' $ do
            -- prp <- A.takeWhile1 C.isLetter
            prp <- prepP
            cas <- comma *> caseP
            cmp <- comma *> compP
            return $ PrepNCP prp cmp cas Nothing [] Nothing (Atr [])
          <?> "plain PrepNCP"
        lexicalized = string "lex" *> do
          between '(' ')' $ do
            pcp <- plain
            neg <- comma *> maybe_ negationP
            lks <- comma *> lexicalHeadsP
            unk <- comma *> A.takeTill (==',')
            atr <- comma *> attributeP
            return pcp
              { negation = neg
              , lexicalHead = lks
              , cpUnknown = Just unk
              , dependents = atr }
          <?> "lexicalized PrepNCP"


    adjpP :: Parser Phrase
    adjpP = plain <|> lexicalized
      <?> "AdjP"
      where
        plain = string "adjp" *> do
          between '(' ')' $ do
            cas <- caseP
            return $ AdjP cas Nothing Nothing Nothing [] (Atr [])
          <?> "plain AdjP"
        lexicalized = string "lex" *> do
          between '(' ')' $ do
            adj <- plain
            num <- comma *> agreeP numberP
            gen <- comma *> agreeP genderP
            deg <- comma *> degreeP
            lks <- comma *> lexicalHeadsP
            atr <- comma *> attributeP
            return adj
              { adjNumber = Just num
              , gender = Just gen
              , degree = Just deg
              , lexicalHead = lks
              , dependents = atr }
          <?> "lexicalized AdjP"


    prepAdjpP :: Parser Phrase
    prepAdjpP = plain <|> lexicalized
      <?> "PrepAdjP"
      where
        plain = string "prepadjp" *> do
          between '(' ')' $ do
            prp <- prepP
            cas <- comma *> caseP
            return $ PrepAdjP prp cas Nothing Nothing Nothing [] (Atr [])
          <?> "plain PrepAdjP"
        lexicalized = string "lex" *> do
          between '(' ')' $ do
            pap <- plain
            num <- comma *> agreeP numberP
            gen <- comma *> agreeP genderP
            deg <- comma *> degreeP
            lks <- comma *> lexicalHeadsP
            atr <- comma *> attributeP
            return pap
              { adjNumber = Just num
              , gender = Just gen
              , degree = Just deg
              , lexicalHead = lks
              , dependents = atr }
          <?> "lexicalized PrepAdjP"


    prepNumpP :: Parser Phrase
    prepNumpP = plain <|> lexicalized
      <?> "prepNumP"
      where
        plain = string "prepnump" *> do
          between '(' ')' $ do
            prp <- prepP
            cas <- comma *> caseP
            return $ PrepNumP prp cas [] [] (Atr [])
          <?> "plain PrepNumP"
        lexicalized = string "lex" *> do
          between '(' ')' $ do
            pnp <- plain
            num <- comma *> lexicalHeadsP
            lks <- comma *> lexicalHeadsP
            atr <- comma *> attributeP
            return $ pnp
              { lexicalNumber = num
              , lexicalHead = lks
              , dependents = atr }
          <?> "lexicalized PrepNumP"


    attributeP :: Parser Attribute
    attributeP =
      A.choice [natrP, atr1P, atrP, ratr1P, ratrP]
      <?> "attributeP"
      where
        natrP  = NAtr <$ A.string "natr"
        atrP   = someAtrP "atr" Atr
        atr1P  = someAtrP "atr1" Atr1
        ratrP  = someAtrP "ratr" RAtr
        ratr1P = someAtrP "ratr1" RAtr1
        someAtrP atrName mkAttr =
          A.string atrName *> do
            fmap mkAttr
              . A.option []
              $ between '(' ')' frameP
          <?> T.unpack atrName


    -- | A parser which should handle any kind of phrase
    -- (but it doesn't really).
    otherP :: Parser [Phrase]
    otherP = do
      x <- A.takeTill (`elem` [';', '}'])
      return $ case M.lookup x expMap of
        Nothing -> [Other x]
        Just xs -> concatMap parsePhrase xs
      <?> "otherP"


    -- | Parse an embedded phrase, `error` if not possible.
    parsePhrase :: Text -> [Phrase]
    parsePhrase = takeRight . A.parseOnly (phraseP <* A.endOfInput)



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


caseP :: Parser Case
caseP = A.choice
  [ Nominative <$ string "nom"
  , Genitive <$ string "gen"
  , Dative <$ string "dat"
  , Accusative <$ string "acc"
  , Instrumental <$ string "inst"
  , Locative <$ string "loc"
  , Structural <$ string "str"
  , Partitive <$ string "part"
  , Agreement <$ string "agr"
  , PostPrep <$ string "postp"
  , Predicative <$ string "pred" ]
  <?> "caseP"



numberP :: Parser Number
numberP = A.choice
  [ Singular <$ string "sg"
  , Plural <$ string "pl" ]
  <?> "numberP"


genderP :: Parser Gender
genderP = A.choice
  [ M1 <$ string "m1"
  , M2 <$ string "m2"
  , M3 <$ string "m3"
  , F <$ string "f"
  , N <$ string "n" ]
  <?> "genderP"


degreeP :: Parser Degree
degreeP = A.choice
  [ Pos <$ string "pos"
  , Com <$ string "com"
  , Sup <$ string "sup" ]
  <?> "degreeP"


negationP :: Parser Negation
negationP = A.choice
  [ Neg <$ string "neg"
  , Aff <$ string "aff" ]
  <?> "numberP"


-- | A parser for lexical (semantic) heads in lexical specifications.
lexicalHeadsP :: Parser [Text]
lexicalHeadsP =
  oneP <|> xorP <?> "lexicalHeadsP"
  where
    oneP = (:[]) <$> lexicalHeadP
    xorP = string "XOR" *> do
      between '(' ')' $ do
        lexicalHeadP `A.sepBy1'` A.char ','


-- | A parser for lexical (semantic) heads in lexical specifications.
lexicalHeadP :: Parser Text
lexicalHeadP = between '\'' '\'' $
  A.takeTill (=='\'')


-- | Complementizer (type?)
compP :: Parser Text
compP = A.takeTill (==')')
  <?> "compP"


-- | Preposition (type?)
prepP :: Parser Text
prepP = A.takeTill (`elem` [',', ')'])
  <?> "prepP"


agreeP :: Parser a -> Parser (Agree a)
agreeP p = A.choice
  [ Agree <$ string "agr"
  , Value <$> p ]
  <?> "agreeP"


-------------------------------------------------------------
-- Utils
-------------------------------------------------------------


-- | `between c1 c2 p` parses with `p` between characters `c1` and `c2`.
between :: Char -> Char -> Parser a -> Parser a
between c1 c2 p = A.char c1 *> p <* A.char c2


-- | Comma parser, i.e., `A.char ','`.
comma :: Parser ()
comma = void $ A.char ','


-- | A parser which interprets the '_' character as `Nothing`,
-- and otherwise uses the given parser to parser input.
maybe_ :: Parser a -> Parser (Maybe a)
maybe_ p = A.choice
  [ Nothing <$ A.char '_'
  , Just <$> p ]
  <?> "maybe_"
