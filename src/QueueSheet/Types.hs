------------------------------------------------------------------------------
-- |
-- Module      : QueueSheet.Types
-- Description : queue sheet types
-- Copyright   : Copyright (c) 2020 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module QueueSheet.Types
  ( -- * Name
    Name(..)
    -- * Url
  , Url(..)
    -- * Section
  , Section(..)
  , defaultSection
    -- * Tag
  , Tag(..)
    -- * Date
  , Date(..)
    -- * Item
  , Item(..)
    -- * Queue
  , Queue(..)
    -- * Import
  , Import(..)
    -- * ImportOrQueue
  , ImportOrQueue(..)
    -- * QueuesFile
  , QueuesFile(..)
    -- * QueueSheet
  , QueueSheet(..)
  ) where

-- https://hackage.haskell.org/package/aeson
import qualified Data.Aeson as A
import Data.Aeson (FromJSON(parseJSON), (.:), (.:?), (.!=))
import qualified Data.Aeson.Types as AT

-- https://hackage.haskell.org/package/base
import Control.Applicative ((<|>))
#if !MIN_VERSION_base (4,11,0)
import Data.Monoid ((<>))
#endif

-- https://hackage.haskell.org/package/ginger
import qualified Text.Ginger as Ginger
import Text.Ginger ((~>))

-- https://hackage.haskell.org/package/scientific
import qualified Data.Scientific as Sci

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

------------------------------------------------------------------------------
-- $Name

-- | Name of a queue or queue item
newtype Name = Name Text
  deriving newtype (Eq, Show)

instance FromJSON Name where
  parseJSON = fmap Name . parseToString

instance Ginger.ToGVal m Name where
  toGVal (Name t) = Ginger.toGVal $ escapeTeX t

instance TTC.Render Name where
  render (Name t) = TTC.fromT t

------------------------------------------------------------------------------
-- $Url

-- | URL of queue or queue item
newtype Url = Url Text
  deriving newtype (Eq, Show)

instance FromJSON Url where
  parseJSON = fmap Url . parseToString

instance Ginger.ToGVal m Url where
  toGVal (Url t) = Ginger.toGVal $ escapeTeX t

instance TTC.Render Url where
  render (Url t) = TTC.fromT t

------------------------------------------------------------------------------
-- $Section

-- | Section used to organize queues
newtype Section = Section Text
  deriving newtype (Eq, Show)

instance FromJSON Section where
  parseJSON = fmap Section . parseToString

instance Ginger.ToGVal m Section where
  toGVal (Section t) = Ginger.toGVal $ escapeTeX t

instance TTC.Render Section where
  render (Section t) = TTC.fromT t

-- | The default section is represented as an empty string
defaultSection :: Section
defaultSection = Section ""

------------------------------------------------------------------------------
-- $Tag

-- | Queue tag
data Tag
  = TagPartial
  | TagComplete
  deriving (Eq, Show)

instance FromJSON Tag where
  parseJSON = A.withText "Tag" $ \case
    "partial"  -> return TagPartial
    "complete" -> return TagComplete
    tag        -> fail $ "unknown tag: " ++ T.unpack tag

------------------------------------------------------------------------------
-- $Date

-- | Date of last queue update
newtype Date = Date Text
  deriving newtype (Eq, Show)

instance FromJSON Date where
  parseJSON = fmap Date . parseToString

instance Ginger.ToGVal m Date where
  toGVal (Date t) = Ginger.toGVal $ escapeTeX t

instance TTC.Render Date where
  render (Date t) = TTC.fromT t

------------------------------------------------------------------------------
-- $Item

-- | Queue item
data Item
  = Item
    { itemName :: !Name
    , itemUrl  :: !(Maybe Url)
    }
  deriving (Eq, Show)

instance FromJSON Item where
  parseJSON = \case
    (A.Object o) -> do
      itemName <- o .:  "name"
      itemUrl  <- o .:? "url"
      return Item{..}
    value -> do
      itemName <- Name <$> parseToString value
      let itemUrl = Nothing
      return Item{..}

instance Ginger.ToGVal m Item where
  toGVal Item{..} = Ginger.dict
    [ "name" ~> itemName
    , "url"  ~> itemUrl
    ]

------------------------------------------------------------------------------
-- $Queue

-- | Queue information
data Queue
  = Queue
    { queueName    :: !Name
    , queueUrl     :: !(Maybe Url)
    , queueSection :: !Section
    , queueSplit   :: !Bool
    , queueTags    :: ![Tag]
    , queueDate    :: !(Maybe Date)
    , queueItems   :: !(Maybe (Either Item [Item]))
    }
  deriving (Eq, Show)

instance FromJSON Queue where
  parseJSON = A.withObject "Queue" $ \o -> do
    queueName    <- o .:  "name"
    queueUrl     <- o .:? "url"
    queueSection <- o .:? "section" .!= defaultSection
    queueSplit   <- o .:? "split"   .!= False
    queueTags    <- o .:? "tags"    .!= []
    queueDate    <- o .:? "date"
    mPrevItem    <- o .:? "prev"
    mNextItems   <- o .:? "next"
    let queueItems = case (mPrevItem, mNextItems) of
          (_,         Just items) -> Just $ Right items
          (Just item, Nothing)    -> Just $ Left item
          (Nothing,   Nothing)    -> Nothing
    return Queue{..}

------------------------------------------------------------------------------
-- $Import

-- | Import declaration
data Import
  = Import
    { importPath    :: !FilePath
    , importSection :: !(Maybe Section)
    }
  deriving (Eq, Show)

instance FromJSON Import where
  parseJSON = A.withObject "Import" $ \o -> do
    importPath    <- o .:  "import"
    importSection <- o .:? "section"
    return Import{..}

------------------------------------------------------------------------------
-- $ImportOrQueue

-- | Import declaration or queue information
data ImportOrQueue
  = IQImport !Import
  | IQQueue  !Queue
  deriving (Eq, Show)

instance FromJSON ImportOrQueue where
  parseJSON value =
    (IQImport <$> parseJSON value) <|> (IQQueue <$> parseJSON value)

------------------------------------------------------------------------------
-- $QueuesFile

-- | Queues file
data QueuesFile
  = QueuesFile
    { qfSections       :: ![Section]
    , qfImportOrQueues :: ![ImportOrQueue]
    }
  deriving (Eq, Show)

instance FromJSON QueuesFile where
  parseJSON = \case
    (A.Object o) -> do
      qfSections <- (:) defaultSection <$> (o .:? "sections" .!= [])
      qfImportOrQueues <- o .: "queues"
      return QueuesFile{..}
    a@A.Array{} -> do
      let qfSections = [defaultSection]
      qfImportOrQueues <- parseJSON a
      return QueuesFile{..}
    A.String{} -> fail "unexpected string"
    A.Number{} -> fail "unexpected number"
    A.Bool{}   -> fail "unexpected bool"
    A.Null     -> fail "unexpected null"

------------------------------------------------------------------------------
-- $QueueSheet

-- | Queue sheet
data QueueSheet
  = QueueSheet
    { qsSections :: ![Section]
    , qsQueues   :: ![Queue]
    }
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- $Library

-- | Escape a string for inclusion in a TeX document
escapeTeX :: Text -> Text
escapeTeX = T.foldl go ""
  where
    go :: Text -> Char -> Text
    go acc = \case
      '#'  -> acc <> "\\#"
      '$'  -> acc <> "\\$"
      '%'  -> acc <> "\\%"
      '&'  -> acc <> "\\&"
      '\\' -> acc <> "\\textbackslash{}"
      '^'  -> acc <> "\\textasciicircum{}"
      '_'  -> acc <> "\\_"
      '{'  -> acc <> "\\{"
      '}'  -> acc <> "\\}"
      '~'  -> acc <> "\\textasciitilde{}"
      c    -> acc `T.snoc` c

-- | Parse any scalar value as a string
--
-- Strings, numbers, booleans, and null are parsed as a string.  Empty
-- strings, arrays, and objects result in an error.
parseToString :: A.Value -> AT.Parser Text
parseToString = \case
    (A.String t)
      | T.null t  -> fail "empty string"
      | otherwise -> pure t
    (A.Number n)  -> pure . T.pack . either (show @Double) (show @Integer) $
      Sci.floatingOrInteger n
    (A.Bool b)    -> pure $ if b then "true" else "false"
    A.Null        -> pure "null"
    A.Array{}     -> fail "unexpected array"
    A.Object{}    -> fail "unexpected object"
