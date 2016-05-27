{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module NLP.Walenty.Parser
( readWalenty
, parseWalenty
) where


import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import qualified Data.Char            as C
import qualified Data.Map.Strict      as M
-- import qualified Data.Map.Strict      as M
-- import           Data.Foldable        (asum)
import           Data.Maybe           (maybeToList)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as L
import qualified Data.Text.Lazy.IO    as L

import           Data.Attoparsec.Text (Parser, string, (<?>))
import qualified Data.Attoparsec.Text as A


import qualified NLP.Walenty.Expand   as E
import           NLP.Walenty.Types


-------------------------------------------------------------
-- Parser
-------------------------------------------------------------


-- | A comment line.
type Comment = Text


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
  <*> A.option [] (maybeComma *> (controlP `A.sepBy1'` A.char ','))
  <*> phrasesP
  <?> "argumentP"
  where
    maybeComma = void $ A.option ' ' (A.char ',')


phrasesP :: Parser [Phrase]
phrasesP = -- concat <$>
  let p = phraseP `A.sepBy1'` A.char ';'
  in  between '{' '}' p
  <?> "phrasesP"


phraseP :: Parser Phrase
phraseP = A.choice
  [ Standard <$> stdPhraseP
  , Special <$> specPhraseP ]
  <?> "phraseP"


stdPhraseP :: Parser StdPhrase
stdPhraseP = A.choice
  [ npP, prepNpP, cpP, ncpP, prepNcP, prepGerP, adjpP, numpP, prepNumpP
  , prepAdjpP, infpP, comparpP, pactpP, paspP, prepPaspP, qublicP, advpP ]
  <?> "stdPhraseP"


specPhraseP :: Parser SpecPhrase
specPhraseP = A.choice
  [ orP, reflP, eP, nonchP , distrpP
  , comPrepNP, xpP, posspP , fixedP ]
  <?> "stdPhraseP"


npP :: Parser StdPhrase
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
        num <- comma *> maybe_ (agreeP numberP)
        lks <- comma *> lexicalHeadsP
        atr <- comma *> attributeP
        return $ NP cas num lks atr
      <?> "lexNpP"


prepNpP :: Parser StdPhrase
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
        num <- comma *> maybe_ (agreeP numberP)
        lks <- comma *> lexicalHeadsP
        atr <- comma *> attributeP
        return $ PrepNP prp cas num lks atr
      <?> "lexicalized PrepNP"


cpP :: Parser StdPhrase
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


ncpP :: Parser StdPhrase
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


prepNcP :: Parser StdPhrase
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


prepGerP :: Parser StdPhrase
prepGerP = plain <|> lexicalized
  <?> "PrepGerP"
  where
    plain = string "prepgerp" *> do
      between '(' ')' $ do
        prp <- prepP
        cas <- comma *> caseP
        return $ PrepGerP prp cas Nothing Nothing [] Nothing (Atr [])
      <?> "plain PrepGerP"
    lexicalized = string "lex" *> do
      between '(' ')' $ do
        ger <- plain
        num <- comma *> maybe_ numberP
        neg <- comma *> maybe_ negationP
        lks <- comma *> lexicalHeadsP
        unk <- comma *> A.takeTill (==',')
        atr <- comma *> attributeP
        return ger
          { number = num
          , negation = neg
          , lexicalHead = lks
          , cpUnknown = Just unk
          , dependents = atr }
      <?> "lexicalized PrepGerP"


adjpP :: Parser StdPhrase
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
        num <- comma *> maybe_ (agreeP numberP)
        gen <- comma *> agreeP genderP
        deg <- comma *> maybe_ degreeP
        lks <- comma *> lexicalHeadsP
        atr <- comma *> attributeP
        return adj
          { agrNumber = num
          , gender = Just gen
          , degree = deg
          , lexicalHead = lks
          , dependents = atr }
      <?> "lexicalized AdjP"


pactpP :: Parser StdPhrase
pactpP = plain <|> lexicalized
  <?> "PactP"
  where
    plain = string "pactp" *> do
      between '(' ')' $ do
        cas <- caseP
        return $ PactP cas Nothing Nothing Nothing [] Nothing (Atr [])
      <?> "plain PactP"
    lexicalized = string "lex" *> do
      between '(' ')' $ do
        pac <- plain
        num <- comma *> agreeP numberP
        gen <- comma *> agreeP genderP
        neg <- comma *> maybe_ negationP
        lks <- comma *> lexicalHeadsP
        unk <- comma *> A.takeTill (==',')
        atr <- comma *> attributeP
        return pac
          { agrNumber = Just num
          , gender = Just gen
          , negation = neg
          , lexicalHead = lks
          , cpUnknown = Just unk
          , dependents = atr }
      <?> "lexicalized PactP"


prepAdjpP :: Parser StdPhrase
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
          { agrNumber = Just num
          , gender = Just gen
          , degree = Just deg
          , lexicalHead = lks
          , dependents = atr }
      <?> "lexicalized PrepAdjP"


numpP :: Parser StdPhrase
numpP = plain <|> lexicalized
  <?> "numP"
  where
    plain = string "nump" *> do
      between '(' ')' $ do
        cas <- caseP
        return $ NumP cas [] [] (Atr [])
      <?> "plain NumP"
    lexicalized = string "lex" *> do
      between '(' ')' $ do
        nup <- plain
        num <- comma *> lexicalHeadsP
        lks <- comma *> lexicalHeadsP
        atr <- comma *> attributeP
        return $ nup
          { lexicalNumber = num
          , lexicalHead = lks
          , dependents = atr }
      <?> "lexicalized NumP"


prepNumpP :: Parser StdPhrase
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


infpP :: Parser StdPhrase
infpP = plain <|> lexicalized
  <?> "InfP"
  where
    plain = string "infp" *> do
      between '(' ')' $ do
        asp <- maybe_ aspectP
        return $ InfP asp Nothing [] False (Atr [])
      <?> "plain InfP"
    lexicalized = string "lex" *> do
      between '(' ')' $ do
        inf <- plain
        neg <- comma *> maybe_ negationP
        lks <- comma *> lexicalHeadsP
        rfl <- comma *> sieP
        atr <- comma *> attributeP
        return $ inf
          { negation = neg
          , lexicalHead = lks
          , reflexive = rfl
          , dependents = atr }
      <?> "lexicalized InfP"


comparpP :: Parser StdPhrase
comparpP = plain <|> lexicalized
  <?> "ComparP"
  where
    plain = string "compar" *> do
      between '(' ')' $ do
        conj <- A.takeTill (`elem` [',', ')'])
        return $ ComparP [conj] []
      <?> "plain ComparP"
    lexicalized = string "lex" *> do
      between '(' ')' $ do
        comp <- plain <* comma
        args <- phraseP `A.sepBy1'` A.char '+'
        -- return $ comp {comparFrame = concat args}
        return $ comp {comparFrame = args}
      <?> "lexicalized ComparP"


qublicP :: Parser StdPhrase
qublicP = plain <|> lexicalized
  <?> "QubP"
  where
    plain = string "qub" *> do
      return $ QubP [] (Atr [])
      <?> "plain QubP"
    lexicalized = string "lex" *> do
      between '(' ')' $ do
        qub <- plain
        lks <- comma *> lexicalHeadsP
        atr <- comma *> attributeP
        return $ qub {lexicalHead = lks, dependents = atr}
      <?> "lexicalized QubP"


paspP :: Parser StdPhrase
paspP = plain <|> lexicalized
  <?> "PasP"
  where
    plain = string "ppasp" *> do
      between '(' ')' $ do
        cas <- caseP
        return $ PasP cas Nothing Nothing Nothing [] (Atr [])
      <?> "plain PasP"
    lexicalized = string "lex" *> do
      between '(' ')' $ do
        pas <- plain
        num <- comma *> agreeP numberP
        gen <- comma *> agreeP genderP
        neg <- comma *> maybe_ negationP
        lks <- comma *> lexicalHeadsP
        atr <- comma *> attributeP
        return pas
          { agrNumber = Just num
          , gender = Just gen
          , negation = neg
          , lexicalHead = lks
          , dependents = atr }
      <?> "lexicalized PasP"


prepPaspP :: Parser StdPhrase
prepPaspP = plain <|> lexicalized
  <?> "PrepPasP"
  where
    plain = string "prepppasp" *> do
      between '(' ')' $ do
        prp <- prepP
        cas <- comma *> caseP
        return $ PrepPasP prp cas Nothing Nothing Nothing [] (Atr [])
      <?> "plain PrepPasP"
    lexicalized = string "lex" *> do
      between '(' ')' $ do
        ppp <- plain
        num <- comma *> agreeP numberP
        gen <- comma *> agreeP genderP
        neg <- comma *> maybe_ negationP
        lks <- comma *> lexicalHeadsP
        atr <- comma *> attributeP
        return ppp
          { agrNumber = Just num
          , gender = Just gen
          , negation = neg
          , lexicalHead = lks
          , dependents = atr }
      <?> "lexicalized PrepPasP"


fixedP :: Parser SpecPhrase
fixedP = string "fixed" *> do
  between '(' ')' $ do
    typ <- phraseP
    comma
    lks <- lexicalHeadP
    return Fixed
      { fixedTyp = typ
      , fixedLex = lks }
  <?> "fixedP"


orP, reflP, eP, nonchP, distrpP, posspP :: Parser SpecPhrase
orP    = Or   <$ string "or"
reflP  = Refl <$ string "refl"
eP     = E    <$ string "E"
nonchP = Nonch <$ string "nonch"
distrpP = DistrP <$ string "distrp"
posspP = PossP <$ string "possp"


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
  [ Controller2 <$ string "controller2"
  , Controllee2 <$ string "controllee2"
  , Controller <$ string "controller"
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
    xorP = (string "XOR" <|> string "OR") *> do
      between '(' ')' $ do
        lexicalHeadP `A.sepBy1'`
          (A.char ',' <|> A.char ';')


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


-- | Relexive marker or nothing?
sieP :: Parser Bool
sieP = do
  x <- A.takeTill (==',')
  return $ case x of
    "się" -> True
    _ -> False


-- | An attribute which can be potentially assigned the agreement value.
agreeP :: Parser a -> Parser (Agree a)
agreeP p = A.choice
  [ Agree <$ string "agr"
  , Value <$> p ]
  <?> "agreeP"


-- exppP :: Parser SpecPhrase
-- exppP = A.choice
--   [comPrepNP, xpP, advpP, posspP]


comPrepNP :: Parser SpecPhrase
comPrepNP = string "comprepnp" *> do
  between '(' ')' $ do
    prep <- A.takeTill (==')')
    return $ ComPrepNP prep


xpP :: Parser SpecPhrase
xpP = plain <|> lexicalized
  <?> "XP"
  where
    plain = string "xp" *> do
      between '(' ')' $ do
        cat <- A.takeWhile1 C.isLetter
        val <- A.option Nothing
          (Just <$> between '[' ']' stdPhraseP)
        return $ XP cat val
      <?> "plain XP"
    lexicalized = string "lex" *> do
      between '(' ')' $ do
        xp  <- plain
        case xpVal xp of
          Just ap@AdvP{..} -> do
            deg <- comma *> maybe_ degreeP
            lks <- comma *> lexicalHeadsP
            atr <- comma *> attributeP
            let ap' = ap
                  { degree = deg
                  , lexicalHead = lks
                  , dependents = atr }
            return xp {xpVal = Just ap'}
          Just pp@PrepNP{..} -> do
            num <- comma *> maybe_ (agreeP numberP)
            lks <- comma *> lexicalHeadsP
            atr <- comma *> attributeP
            let pp' = pp
                  { agrNumber = num
                  , lexicalHead = lks
                  , dependents = atr }
            return xp {xpVal = Just pp'}
          Just np@NP{..} -> do
            num <- comma *> maybe_ (agreeP numberP)
            lks <- comma *> lexicalHeadsP
            atr <- comma *> attributeP
            let np' = np
                  { agrNumber = num
                  , lexicalHead = lks
                  , dependents = atr }
            return xp {xpVal = Just np'}
          Just cp@ComparP{..} -> do
            args <- comma *> phraseP `A.sepBy1'` A.char '+'
            let cp' = cp {comparFrame = args}
            return xp {xpVal = Just cp'}
          Just pnp@PrepNumP{..} -> do
            num <- comma *> lexicalHeadsP
            lks <- comma *> lexicalHeadsP
            atr <- comma *> attributeP
            let pnp' = pnp
                  { lexicalNumber = num
                  , lexicalHead = lks
                  , dependents = atr }
            return xp {xpVal = Just pnp'}
          Just pap@PrepAdjP{..} -> do
            num <- comma *> agreeP numberP
            gen <- comma *> agreeP genderP
            deg <- comma *> degreeP
            lks <- comma *> lexicalHeadsP
            atr <- comma *> attributeP
            let pap' = pap
                  { agrNumber = Just num
                  , gender = Just gen
                  , degree = Just deg
                  , lexicalHead = lks
                  , dependents = atr }
            return xp {xpVal = Just pap'}
          Just cp@CP{..} -> do
            neg <- comma *> maybe_ negationP
            lks <- comma *> lexicalHeadsP
            unk <- comma *> A.takeTill (==',')
            atr <- comma *> attributeP
            let cp' = cp
                  { negation = neg
                  , lexicalHead = lks
                  , cpUnknown = Just unk
                  , dependents = atr }
            return xp {xpVal = Just cp'}
          Just pg@PrepGerP{..} -> do
            num <- comma *> maybe_ numberP
            neg <- comma *> maybe_ negationP
            lks <- comma *> lexicalHeadsP
            unk <- comma *> A.takeTill (==',')
            atr <- comma *> attributeP
            let pg' = pg
                  { number = num
                  , negation = neg
                  , lexicalHead = lks
                  , cpUnknown = Just unk
                  , dependents = atr }
            return xp {xpVal = Just pg'}
          _ -> error "xpP: the unthinkable happened"
      <?> "lexicalized XP"
    -- xpApP xp ap@AdvP{..} =


advpP :: Parser StdPhrase
advpP = plain <|> lexicalized
  <?> "AdvP"
  where
    plain = string "advp" *> do
      between '(' ')' $ do
        cat <- A.takeWhile1 C.isLetter
        return $ AdvP cat Nothing [] (Atr [])
      <?> "plain AdvP"
    lexicalized = string "lex" *> do
      between '(' ')' $ do
        adv <- plain
        deg <- comma *> maybe_ degreeP
        lks <- comma *> lexicalHeadsP
        atr <- comma *> attributeP
        return adv
          { degree = deg
          , lexicalHead = lks
          , dependents = atr }
      <?> "lexicalized AdvP"


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


takeRight :: Either [Char] t -> t
takeRight (Right x) = x
takeRight (Left e) = error e
