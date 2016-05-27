

-- | The module provides functions for parsing /Walenty/.


module NLP.Walenty
( readWalenty
, parseWalenty
) where


import           NLP.Walenty.Types
import           NLP.Walenty.Parser


-------------------------------------------------------------
-- Expansion map processing
-------------------------------------------------------------


-- -- | Relation -- alternative or conjunction -- over the phrases.
-- data Rel a
--   = Alt {unRel :: [a]}
--   | Con {unRel :: [a]}
--   deriving (Show, Functor)
-- 
-- 
-- -- -- | Alternative of elements.
-- -- newtype Alt a = Alt {unAlt :: [a]}
-- --   deriving (Show, Functor)
-- --
-- --
-- -- -- | Conjunction of elements.
-- -- newtype Con a = Con {unCon :: [a]}
-- --   deriving (Show, Functor)
-- 
-- 
-- -- | Expansion value.  Either a word form or an alternative
-- -- of conjunctions of frames.
-- type ExpVal = Either [Text] (Rel Phrase)
-- 
-- 
-- -- | Expansion map.
-- type ExpMap = M.Map Phrase ExpVal
-- 
-- 
-- -- -- | Expansion map which maps
-- -- data ExpMap = ExpMap
-- --   { phraseExpansions :: M.Map (ExpPhrase ()) [[Phrase]]
-- --   -- ^ Each `ExpPhrase` can be expanded to a collection of frames,
-- --   -- each frame consisting itself of a list of required phrases.
-- --   , pseudoExpansions :: M.Map (ExpPhrase ()) [Text]
-- --   -- ^ Phrase type expansions which lead to lexical data.
-- --   -- Not clear how to handle them yet.
-- --   , attrSubtypes     :: M.Map Text [Text]
-- --   -- ^ Attribute subtypes expansions.
-- --   } deriving (Show, Eq, Ord)
-- 
-- 
-- -- | Resolve the expansion map, i.e.,
-- resolveExpMap :: E.ExpansionMap -> ExpMap
-- resolveExpMap =
--   M.fromList . map resolvePair . M.toList
--   where
--     resolvePair (from, to) =
--       let x = parseOne from
--       in (x, case x of
--              AdvP{} -> Left to
--              _ -> Right $ case to of
--                      [toElem] -> parseGen toElem
--                      _ -> Alt . map parseOne $ to
--          )
--     parseOne = takeRight . A.parseOnly (phraseP <* A.endOfInput)
--     parseGen x = case A.parseOnly (phrasePlusP <* A.endOfInput) x of
--       Right y -> Con y
--       Left _  -> Alt [parseOne x]
--     phrasePlusP =
--       phraseP `A.sepBy1'` (A.skipSpace *> A.char ';' <* A.skipSpace)
--       <?> "phrasePlusP"
-- 
-- 
-- -- expandVerb :: ExpMap -> Verb -> Verb
-- -- expandVerb expMap v =
-- --   v {frame = expandFrame (frame v)}
-- --   where
-- --     expandFrame = map expandArg
-- --     expandArg a =
-- --       let ..?
-- --       in a {phraseAlt = map (expandPhrase expMap) (phraseAlt a)}
-- 
-- 
-- -- | Recursively expand the phrase using the expansion map.
-- expandPhrase :: ExpMap -> Phrase -> Rel Phrase
-- expandPhrase expMap v = fmap (expandInside expMap) $
--   case M.lookup v expMap of
--     Nothing -> Alt [v]
--     Just (Left xs) -> Alt . maybeToList $ lexicalize xs v
--     Just (Right vss) -> vss
-- 
-- 
-- lexicalize :: [Text] -> Phrase -> Maybe Phrase
-- lexicalize xs p = undefined
-- -- lexicalize xs p = case p of
-- --   NP{} -> Just (doit p)
-- --   PrepNP{} -> Just (doit p)
-- --   CP{} -> Just (doit p)
-- --   NCP{} -> Just (doit p)
-- --   PrepNCP{} -> Just (doit p)
-- --   PrepGerP{} -> Just (doit p)
-- --   Fixed{} -> Just (doit p)
-- --   _ -> Nothing
-- --   where doit q = q {lexicalHead = xs}
-- 
-- 
-- expandInside :: ExpMap -> Phrase -> Phrase
-- expandInside expMap p = undefined
