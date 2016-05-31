module NLP.Walenty.Types
( Verb (..)
, CertLevel (..)
, Frame
, Argument (..)
, Function (..)
, Phrase (..)
, StdPhrase (..)
, SpecPhrase (..)
, Case (..)
, Aspect (..)
, Gender (..)
, Number (..)
, Negation (..)
, Degree (..)
, Control (..)
, Attribute (..)
, Agree (..)
) where


import           Data.Text (Text)


-------------------------------------------------------------
-- Types
-------------------------------------------------------------


-- | A verbal lexical entry from /Walenty/.
data Verb = Verb
  { base          :: Text
    -- ^ Base form of the verb
  , reflexiveV    :: Bool
    -- ^ "się"?
  , certitude     :: CertLevel
    -- ^ Level of certitude of the entry
  , negativity    :: Maybe Negation
    -- ^ Negativity
  , predicativity :: Bool
    -- ^ Predicativity
  , aspect        :: Maybe Aspect
    -- ^ Aspect of the verb
  , frame         :: Frame
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
  -- -- | UnknownAspect
  deriving (Show, Eq, Ord)


-- | A valency frame of the verb.
type Frame = [Argument]


-- | An item of the frame.
data Argument = Argument
  { function  :: Maybe Function
  , control   :: [Control]
    -- ^ An argument can have assigned many different control values
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
  | Controller2
  | Controllee2
  deriving (Show, Eq, Ord)


data Phrase
  = Standard StdPhrase
  | Special SpecPhrase
  deriving (Show, Eq, Ord)


-- | A standard syntactic constituency phrase.
data StdPhrase
  = NP
    { caseG       :: Case
      -- ^ Grammatical case
    , agrNumber   :: Maybe (Agree Number)
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
    , agrNumber   :: Maybe (Agree Number)
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
    , reflexive      :: Bool
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
    , reflexive      :: Bool
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
    , reflexive      :: Bool
      -- ^ "się"?
    , dependents     :: Attribute
      -- ^ Dependents (if specified)
    }
    -- ^ Prepositional phrase involving a complementiser clause
    -- with a correlative pronoun
  | PrepGerP
    { preposition :: Text
      -- ^ Preposition
    , caseG       :: Case
      -- ^ Grammatical case
    , number      :: Maybe Number
      -- ^ Number (if specified)
    , negation    :: Maybe Negation
      -- ^ Number (if specified)
    , lexicalHead :: [Text]
      -- ^ Lexical head (if specified)
    , reflexive   :: Bool
      -- ^ "się"?
    , dependents  :: Attribute
      -- ^ Dependents (if specified)
    }
    -- ^ TODO
  | AdjP
    { caseG       :: Case
      -- ^ Grammatical case
    , agrNumber   :: Maybe (Agree Number)
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
  | PactP
    { caseG       :: Case
      -- ^ Grammatical case
    , agrNumber   :: Maybe (Agree Number)
      -- ^ Number (if specified)
    , gender      :: Maybe (Agree Gender)
      -- ^ Gender (if specified)
    , negation    :: Maybe Negation
      -- ^ Negation (if specified)
    , lexicalHead :: [Text]
      -- ^ Lexical head (if specified)
    , reflexive   :: Bool
      -- ^ "się"?
    , dependents  :: Attribute
      -- ^ Dependents (if specified)
    }
    -- ^ TODO phrase
  | NumP
    { caseG         :: Case
      -- ^ Grammatical case
    , lexicalNumber :: [Text]
      -- ^ Lexical number (if specified)
    , lexicalHead   :: [Text]
      -- ^ Lexical head (if specified)
    , dependents    :: Attribute
      -- ^ Dependents (if specified)
    }
    -- ^ Numeral(?) phrase
  | PrepNumP
    { preposition   :: Text
      -- ^ Preposition
    , caseG         :: Case
      -- ^ Grammatical case
    , lexicalNumber :: [Text]
      -- ^ Lexical number (if specified)
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
    , agrNumber   :: Maybe (Agree Number)
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
    -- ^ TODO
  | InfP
    { infAspect   :: Maybe Aspect
      -- ^ Aspect
    , negation    :: Maybe Negation
      -- ^ Negation (if specified)
    , lexicalHead :: [Text]
      -- ^ Lexical head (if specified)
    , reflexive   :: Bool
      -- ^ "się"?
    , dependents  :: Attribute
      -- ^ Dependents (if specified)
    }
    -- ^ Infinitival phrase
  | PasP
    { caseG       :: Case
      -- ^ Grammatical case
    , agrNumber   :: Maybe (Agree Number)
      -- ^ Number (if specified)
    , gender      :: Maybe (Agree Gender)
      -- ^ Gender (if specified)
    , negation    :: Maybe Negation
      -- ^ Negation (if specified)
    , lexicalHead :: [Text]
      -- ^ Lexical head (if specified)
    , dependents  :: Attribute
      -- ^ Dependents (if specified)
    }
    -- ^ TODO
  | PrepPasP
    { preposition :: Text
      -- ^ Preposition
    , caseG       :: Case
      -- ^ Grammatical case
    , agrNumber   :: Maybe (Agree Number)
      -- ^ Number (if specified)
    , gender      :: Maybe (Agree Gender)
      -- ^ Gender (if specified)
    , negation    :: Maybe Negation
      -- ^ Negation (if specified)
    , lexicalHead :: [Text]
      -- ^ Lexical head (if specified)
    , dependents  :: Attribute
      -- ^ Dependents (if specified)
    }
    -- ^ TODO
  | AdvP
    { advpCat     :: Text
      -- ^ Locative, ablative, adlative, etc. (see the Walenty webpage)
    , degree      :: Maybe Degree
      -- ^ Number (if specified)
    , lexicalHead :: [Text]
      -- ^ Lexical (semantic) head (if specified)
    , dependents  :: Attribute
      -- ^ Dependents (if specified)
    }
    -- ^ Adverbial phrase; such phrases have typically assigned expansions,
    -- even though they constitute standard phrases.
  | QubP
    { lexicalHead :: [Text]
      -- ^ Lexical (semantic) head (if specified)
    , dependents  :: Attribute
      -- ^ Dependents (if specified)
    }
    -- ^ Qublical phrase (:-))
  | ComparP
    { lexicalHead :: [Text]
      -- ^ Comparative conjunction
    , comparFrame :: [Phrase]
      -- ^ A list of arguments, with no functional or control specifications.
      -- Alternatives cannot be represented.  TODO: does it mean that all
      -- have to be present?  TODO: Could we encode them as dependents?
      -- Then all standard phrases would provide `dependents`.
    }
    -- ^ Comparative phrase
  deriving (Show, Eq, Ord)


-- | An phrase type occuring on a specific position of the frame.
data SpecPhrase
  = Or
    -- ^ Oratio recta, i.e. direct speech
  | Refl
    -- ^ Reflexive use marked through the word /się/
  | E
    -- ^ Implicit subject, transmitted subject when marked as controller
  | Nonch
    -- ^ TODO
  | DistrP
    -- ^ TODO
  | PossP
    -- ^ TODO
  | ComPrepNP
    { complexPrep :: Text
      -- ^ E.g. "przy okazji", "u podstaw"
    }
    -- ^ Complex (i.e. multi-word) preposition.  This is not a standard phrase,
    -- but only an expandable phrase, of which expansion should be defined
    -- in a separate expansion file.
  | XP
    { xpCat :: Text
      -- ^ Locative, ablative, adlative, etc. (see the Walenty webpage)
    , xpVal :: Maybe Phrase
      -- ^ The value of the XP category can be specified
      -- at the site of its use
    }
    -- ^ "Adverbial" phrases involving semantic requirements
    -- (expressible through adverbs, prepositional phrases,
    -- or sentential phrases).  Syntactic sugar, also should
    -- be handled by expansions' file.
  | Fixed
    { fixedTyp :: Phrase
      -- ^ Type of the fixed phrase
    , fixedLex :: Text
      -- ^ Lexical form of the fixed phrase
    }
    -- ^ Fixed phrase
  deriving (Show, Eq, Ord)


-- | Allows to construct an attribute which, instead of one of its
-- regular values, can take the agreement value.
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
    -- TODO: therefore, this is not really a frame,
    -- because in a frame all arguments have to be
    -- realized.
  deriving (Show, Eq, Ord)
