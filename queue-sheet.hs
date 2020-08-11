------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : queue sheet utility
-- Copyright   : Copyright (c) 2020 Travis Cardwell
-- License     : MIT
--
-- See the README for details.
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

module Main (main) where

-- https://hackage.haskell.org/package/aeson
import qualified Data.Aeson as A
import Data.Aeson (FromJSON(parseJSON), (.:), (.:?), (.!=))
import qualified Data.Aeson.Types as AT

-- https://hackage.haskell.org/package/base
import Control.Applicative (optional)
import Control.Monad (forM_, unless, when)
import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base (4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Version (showVersion)
import System.Exit (exitFailure)
import qualified System.IO as IO

-- https://hackage.haskell.org/package/directory
import System.Directory
  ( createDirectory, doesPathExist, removeDirectoryRecursive, renameFile
  , withCurrentDirectory
  )

-- https://hackage.haskell.org/package/filepath
import System.FilePath ((</>), replaceExtension)

-- https://hackage.haskell.org/package/ginger
import qualified Text.Ginger as Ginger
import Text.Ginger ((~>))

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA

-- https://hackage.haskell.org/package/process
import qualified System.Process as Proc

-- https://hackage.haskell.org/package/scientific
import qualified Data.Scientific as Sci

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Writer (Writer)

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- https://hackage.haskell.org/package/yaml
import qualified Data.Yaml as Yaml

-- (queue-sheet:cabal)
import qualified Paths_queue_sheet as Project

-- (queue-sheet:executable)
import qualified LibOA

------------------------------------------------------------------------------
-- $Constants

-- | Build directory name
buildDir :: FilePath
buildDir = "queue-sheet-build"

-- | Default template file
defaultTemplate :: FilePath
defaultTemplate = "template.tex"

------------------------------------------------------------------------------
-- $Types

-- | Name of a queue or queue item
newtype Name = Name Text
  deriving newtype Show

instance FromJSON Name where
  parseJSON = fmap Name . parseToString

instance Ginger.ToGVal m Name where
  toGVal (Name t) = Ginger.toGVal $ escapeTeX t

instance TTC.Render Name where
  render (Name t) = TTC.fromT t

-- | URL of queue or queue item
newtype Url = Url Text
  deriving newtype Show

instance FromJSON Url where
  parseJSON = fmap Url . parseToString

instance Ginger.ToGVal m Url where
  toGVal (Url t) = Ginger.toGVal $ escapeTeX t

instance TTC.Render Url where
  render (Url t) = TTC.fromT t

-- | Section used to organize queues
newtype Section = Section Text
  deriving newtype (Eq, Show)

instance FromJSON Section where
  parseJSON = fmap Section . parseToString

instance Ginger.ToGVal m Section where
  toGVal (Section t) = Ginger.toGVal $ escapeTeX t

instance TTC.Render Section where
  render (Section t) = TTC.fromT t

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

-- | Date of last queue update
newtype Date = Date Text
  deriving newtype Show

instance FromJSON Date where
  parseJSON = fmap Date . parseToString

instance Ginger.ToGVal m Date where
  toGVal (Date t) = Ginger.toGVal $ escapeTeX t

instance TTC.Render Date where
  render (Date t) = TTC.fromT t

-- | Queue item
data Item
  = Item
    { itemName :: !Name
    , itemUrl  :: !(Maybe Url)
    }
  deriving Show

instance FromJSON Item where
  parseJSON = \case
    (A.Object o) ->
      Item
        <$> o .:  "name"
        <*> o .:? "url"
    value ->
      Item
        <$> fmap Name (parseToString value)
        <*> pure Nothing

instance Ginger.ToGVal m Item where
  toGVal Item{..} = Ginger.dict
    [ "name" ~> itemName
    , "url"  ~> itemUrl
    ]

--instance TTC.Render Item where
--  render (Item t) = TTC.fromT t

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
  deriving Show

instance FromJSON Queue where
  parseJSON = A.withObject "Queue" $ \o -> do
    queueName    <- o .:  "name"
    queueUrl     <- o .:? "url"
    queueSection <- o .:  "section"
    queueSplit   <- o .:? "split" .!= False
    queueTags    <- o .:? "tags" .!= []
    queueDate    <- o .:? "date"
    mPrevItem    <- o .:? "prev"
    mNextItems   <- o .:? "next"
    let queueItems = case (mPrevItem, mNextItems) of
          (_,         Just items) -> Just $ Right items
          (Just item, Nothing)    -> Just $ Left item
          (Nothing,   Nothing)    -> Nothing
    return Queue{..}

-- | Queues file
data QueuesFile
  = QueuesFile
    { qfSections :: ![Section]
    , qfQueues   :: ![Queue]
    }
  deriving Show

instance FromJSON QueuesFile where
  parseJSON = A.withObject "QueuesFile" $ \o ->
    QueuesFile
      <$> o .: "sections"
      <*> o .: "queues"

------------------------------------------------------------------------------
-- $Library

-- | Display an error and exit the program
errorExit :: String -> IO a
errorExit msg = do
    putStrLn $ "error: " ++ msg
    exitFailure

-- | Parse any scalar value as a string
--
-- Strings, numbers, booleans, and null are parsed as a string.  Arrays and
-- objects result in an error.
parseToString :: A.Value -> AT.Parser Text
parseToString = \case
    (A.String s) -> pure s
    (A.Number n) -> pure . T.pack . either (show @Double) (show @Integer) $
                      Sci.floatingOrInteger n
    (A.Bool b)   -> pure $ if b then "true" else "false"
    A.Null       -> pure "null"
    A.Array{}    -> fail "unexpected array"
    A.Object{}   -> fail "unexpected object"

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

------------------------------------------------------------------------------
-- $Yaml

-- | Load @queues.yaml@
loadQueuesYaml :: FilePath -> IO QueuesFile
loadQueuesYaml path = do
    let yamlError = errorExit
                  . (("error loading " ++ path ++ ": ") ++)
                  . Yaml.prettyPrintParseException
    qf@QueuesFile{..} <- either yamlError pure =<< Yaml.decodeFileEither path
    forM_ qfQueues $ \Queue{..} ->
      unless (queueSection `elem` qfSections) . errorExit . unwords $
        [ "queue", TTC.render queueName
        , "has unknown section", TTC.render queueSection
        ]
    return qf

------------------------------------------------------------------------------
-- $Template

-- | Queue context
data QueueCtx
  = QueueCtx
    { name       :: !Name
    , url        :: !(Maybe Url)
    , isSplit    :: !Bool
    , isPartial  :: !Bool
    , isComplete :: !Bool
    , date       :: !(Maybe Date)
    , prevItem   :: !(Maybe Item)
    , nextItems  :: ![Item]
    }

instance Ginger.ToGVal m QueueCtx where
  toGVal QueueCtx{..} = Ginger.dict
    [ "name"       ~> name
    , "url"        ~> url
    , "isSplit"    ~> isSplit
    , "isPartial"  ~> isPartial
    , "isComplete" ~> isComplete
    , "date"       ~> date
    , "prevItem"   ~> prevItem
    , "nextItems"  ~> nextItems
    ]

-- | Queue context constructor
queueCtx :: Queue -> QueueCtx
queueCtx Queue{..} = QueueCtx
    { name       = queueName
    , url        = queueUrl
    , isSplit    = queueSplit
    , isPartial  = TagPartial `elem` queueTags
    , isComplete = TagComplete `elem` queueTags
    , date       = queueDate
    , prevItem   = either Just (const Nothing) =<< queueItems
    , nextItems  = maybe [] (either (const []) id) queueItems
    }

-- | Section context
newtype SectionCtx = SectionCtx (Section, [QueueCtx])

instance Ginger.ToGVal m SectionCtx where
  toGVal (SectionCtx (section, queues)) = Ginger.dict
    [ "name"   ~> section
    , "queues" ~> queues
    ]

-- | Template context
newtype Context = Context [SectionCtx]
  deriving newtype (Ginger.ToGVal m)

-- | Template context constructor
context :: [Section] -> [Queue] -> Context
context sections queues = Context
    [ SectionCtx
        ( section
        , [ queueCtx queue
          | queue <- queues, queueSection queue == section
          ]
        )
    | section <- sections
    ]

-- | Create a Ginger context from a template context
gingerContext
  :: Context
  -> Ginger.GingerContext Ginger.SourcePos (Writer Text) Text
gingerContext ctx = Ginger.makeContextText $ \case
    "sections" -> Ginger.toGVal ctx
    _          -> Ginger.toGVal (Nothing :: Maybe Text)

-- | Load a Ginger template
loadTemplate :: FilePath -> IO (Ginger.Template Ginger.SourcePos)
loadTemplate path = do
    eet <- Ginger.parseGingerFile' options path
    case eet of
      Right template -> return template
      Left err -> errorExit $ concat
        [ "error loading template: "
        , maybe path show $ Ginger.peSourcePosition err
        , ": "
        , Ginger.peErrorMessage err
        ]
  where
    options :: Ginger.ParserOptions IO
    options = Ginger.ParserOptions
      { poIncludeResolver     = fmap Just . IO.readFile
      , poSourceName          = Nothing
      , poKeepTrailingNewline = False
      , poLStripBlocks        = False
      , poTrimBlocks          = False
      , poDelimiters          = Ginger.Delimiters
          { delimOpenInterpolation  = "<<"
          , delimCloseInterpolation = ">>"
          , delimOpenTag            = "<!"
          , delimCloseTag           = "!>"
          , delimOpenComment        = "<#"
          , delimCloseComment       = "#>"
          }
      }

-- | Render a template using the given context
renderTemplate
  :: FilePath
  -> Ginger.Template Ginger.SourcePos
  -> Context
  -> IO ()
renderTemplate path template ctx =
    TIO.writeFile path $ Ginger.runGinger (gingerContext ctx) template

------------------------------------------------------------------------------
-- $Build

-- | Build the PDF using XeLaTeX
build :: FilePath -> IO ()
build path = Proc.callProcess "xelatex" ["-halt-on-error", path]

------------------------------------------------------------------------------
-- $CLI

-- | Program options
data Options
  = Options
    { optTemplate :: !FilePath
    , optOutput   :: !(Maybe FilePath)
    , optQueues   :: !FilePath
    }
  deriving Show

-- | Parse program options
parseOptions :: IO Options
parseOptions = OA.execParser
    $ OA.info (LibOA.helper <*> LibOA.versioner version <*> options)
    $ mconcat
        [ OA.fullDesc
        , OA.progDesc "queue sheet utility"
        , OA.failureCode 2
        ]
  where
    version :: String
    version = "queue-sheet-haskell " ++ showVersion Project.version

    options :: OA.Parser Options
    options = Options
      <$> templateOption
      <*> optional outputOption
      <*> queuesArgument

    templateOption :: OA.Parser FilePath
    templateOption = OA.strOption $ mconcat
      [ OA.long "template"
      , OA.short 't'
      , OA.metavar "TEMPLATE.tex"
      , OA.value defaultTemplate
      , OA.showDefaultWith id
      , OA.help "template file"
      ]

    outputOption :: OA.Parser FilePath
    outputOption = OA.strOption $ mconcat
      [ OA.long "output"
      , OA.short 'o'
      , OA.metavar "QUEUES.pdf"
      , OA.help "output file"
      ]

    queuesArgument :: OA.Parser FilePath
    queuesArgument = OA.strArgument $ mconcat
      [ OA.metavar "QUEUES.yaml"
      , OA.help "YAML file specifying queue information"
      ]

-- | Main function
main :: IO ()
main = do
    Options{..} <- parseOptions
    QueuesFile{..} <- loadQueuesYaml optQueues
    template <- loadTemplate optTemplate
    exists <- doesPathExist buildDir
    when exists . errorExit $ "directory already exists: " ++ buildDir
    let pdfFile = fromMaybe (replaceExtension optQueues "pdf") optOutput
        texFile = replaceExtension pdfFile "tex"
    createDirectory buildDir
    withCurrentDirectory buildDir $ do
      renderTemplate texFile template $ context qfSections qfQueues
      build texFile
    renameFile (buildDir </> pdfFile) pdfFile
    removeDirectoryRecursive buildDir
