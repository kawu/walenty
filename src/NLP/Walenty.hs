{-# LANGUAGE DeriveFunctor #-}


-- | The module provides functions for parsing /Walenty/.


module NLP.Walenty
( readWalenty
, parseWalenty
, readExpMap
, expandVerb
) where


import           Control.Arrow        (left)

import qualified Data.Attoparsec.Text as A
import qualified Data.Char            as C
import qualified Data.Map.Strict      as M
import           Data.Maybe           (maybeToList)
import           Data.Text            (Text)
import qualified Data.Text.Lazy       as L
import qualified Data.Text.Lazy.IO    as L

import qualified NLP.Walenty.Expand   as E
import           NLP.Walenty.Parser
import           NLP.Walenty.Types


-------------------------------------------------------------
-- Top-level functions
-------------------------------------------------------------


-- | Read Walenty file (verb entries only).
readWalenty
  :: FilePath -- ^ Walenty proper
  -> IO [Either Verb Comment]
readWalenty walPath = do
  parseWalenty <$> L.readFile walPath


-- | Parse Walenty file (verb entries only).
parseWalenty :: L.Text -> [Either Verb Comment]
parseWalenty
  = map (takeRight . parseLine . L.toStrict)
  . filter (not . L.null)
  . map (L.dropAround $ \c -> C.isSpace c || not (C.isPrint c))
  . L.lines
  where
    parseLine = A.parseOnly $ lineP <* A.endOfInput


-- | Parse definitions of the expansions.
readExpMap
  :: FilePath -- ^ Expansion file
  -> IO ExpMap
readExpMap = fmap resolveExpMap . E.readExpansions


-- -- | Read Walenty file (verb entries only).
-- readWalenty
--   :: FilePath -- ^ Expansion file
--   -> FilePath -- ^ Walenty proper
--   -> IO [Either Verb Comment]
-- readWalenty expPath walPath = do
--   expMap <- E.readExpansions expPath
--   parseWalenty expMap <$> L.readFile walPath
-- 
-- 
-- -- | Parse Walenty file (verb entries only).
-- parseWalenty :: E.ExpansionMap -> L.Text -> [Either Verb Comment]
-- parseWalenty expMap0
--   = map (left $ expandVerb expMap)
--   . map (takeRight . parseLine . L.toStrict)
--   . filter (not . L.null)
--   . map (L.dropAround $ \c -> C.isSpace c || not (C.isPrint c))
--   . L.lines
--   where
--     parseLine = A.parseOnly $ lineP <* A.endOfInput
--     expMap = resolveExpMap expMap0


-------------------------------------------------------------
-- Expansion map processing
-------------------------------------------------------------


-- -- | Relation -- alternative or conjunction -- over the phrases.
-- data Rel a
--   = Alt {_unRel :: [a]}
--   -- ^ Alternative (equivalent to phrase types separated
--   -- by ';' in the core part of the dictionary)
--   | Con {_unRel :: [a]}
--   -- ^ Conjunction (equivalent to phrase types separated by '+'
--   -- in the core part of the dictionary); i.e., different elements
--   -- of the list have all to be realized.
--   deriving (Show, Functor)
-- 
-- 
-- -- | Flatten an expression over phrases.
-- -- If the root is `Con`, all elements as well, and respectively,
-- -- if the root is `Alt`, all elements as well.
-- flatten :: Rel (Rel a) -> Rel a
-- flatten (Alt xs) = (Alt . concat) (takeAlt <$> xs)
--   where takeAlt (Alt x) = x
--         takeAlt (Con _) = error "flatten.takeAlt: error"
-- flatten (Con xs) = (Con . concat) (takeCon <$> xs)
--   where takeCon (Con x) = x
--         takeCon (Alt _) = error "flatten.takeCon: error"


-- | Expansion value.  Either a word form or an alternative
-- of conjunctions of frames.
type ExpVal = Either [Text] [Phrase]


-- | Expansion map.
type ExpMap = M.Map Phrase ExpVal


-- | Resolve the expansion map, i.e.,
resolveExpMap :: E.ExpansionMap -> ExpMap
resolveExpMap =
  M.fromList . map resolvePair . M.toList
  where
    resolvePair (from, to) =
      let x = parseOne from
      in (x, case x of
             Standard AdvP{} -> Left to
             _ -> Right . map parseOne $ to
         )
    parseOne  = takeRight . A.parseOnly (phraseP <* A.endOfInput)


-- | Recursively expand the verb using the expansion map.
expandVerb :: ExpMap -> Verb -> Verb
expandVerb m v = v {frame = expandFrame m (frame v)}


-- | Recursively expand the phrase using the expansion map.
expandFrame :: ExpMap -> Frame -> Frame
expandFrame m = map (expandArg m)


-- | One argument can expand to several arguments!
-- We copy the function/control of the source argument
-- to all resulting arguments in such a case.
expandArg :: ExpMap -> Argument -> Argument
expandArg expMap arg =
  let go = concatMap (expandPhrase expMap)
  in arg {phraseAlt = go (phraseAlt arg)}


-- | Recursively expand the phrase using the expansion map.
expandPhrase :: ExpMap -> Phrase -> [Phrase]
expandPhrase expMap v = fmap (expandInside expMap) $
  case M.lookup v expMap of
    Nothing -> [v]
    Just (Left xs) -> maybeToList $ lexicalize xs v
    Just (Right rp) -> concatMap (expandPhrase expMap) rp


-- | Assign lexical head(s) to the phrase.
lexicalize :: [Text] -> Phrase -> Maybe Phrase
lexicalize xs p = case p of
  -- Based on the assumption that standard phrases
  -- have a potential for lexical specifications
  Standard s -> (Just . Standard) s {lexicalHead = xs}
  _ -> Nothing


-- | Expand any embedded phrases of the given phrase.
expandInside :: ExpMap -> Phrase -> Phrase
expandInside m p = case p of
  Standard s -> Standard $ expandInsideStd m s
  Special s  -> Special $ expandInsideSpec m s


-- | Expand any embedded phrases of the given standard phrase.
expandInsideStd :: ExpMap -> StdPhrase -> StdPhrase
expandInsideStd expMap p = case p of
  ComparP{} -> p
    {comparFrame =
        map (expandInside expMap) (comparFrame p)}
  _ -> p
    {dependents = expandDeps (dependents p)}
  where
    expandDeps NAtr = NAtr
    expandDeps (Atr f) = Atr (expFrame f)
    expandDeps (Atr1 f) = Atr1 (expFrame f)
    expandDeps (RAtr f) = RAtr (expFrame f)
    expandDeps (RAtr1 f) = RAtr1 (expFrame f)
    expFrame = expandFrame expMap


-- | Expand any embedded phrases of the given special phrase.
expandInsideSpec :: ExpMap -> SpecPhrase -> SpecPhrase
expandInsideSpec expMap s = case s of
  XP{} -> s {xpVal = fmap (expandInside expMap) (xpVal s)}
  -- -- Note that we don't expand fixed phrases inside
  -- Fixed{} -> s {fixedTyp = expandPhrase expMap (fixedTyp s)}
  _ -> s


-------------------------------------------------------------
-- Utils
-------------------------------------------------------------


takeRight :: Either [Char] t -> t
takeRight (Right x) = x
takeRight (Left e) = error e
