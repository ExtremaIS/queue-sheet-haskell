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

-- https://hackage.haskell.org/package/ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen (Doc)

-- https://hackage.haskell.org/package/base
import Control.Monad (forM_, unless, when)
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

------------------------------------------------------------------------------
-- $Types

-- | Name of a queue
newtype Name = Name Text
  deriving newtype Show

instance FromJSON Name where
  parseJSON = fmap Name . parseToString

instance Ginger.ToGVal m Name where
  toGVal (Name t) = Ginger.toGVal $ escapeTeX t

instance TTC.Render Name where
  render (Name t) = TTC.fromT t

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
newtype Item = Item Text
  deriving newtype Show

instance FromJSON Item where
  parseJSON = fmap Item . parseToString

instance Ginger.ToGVal m Item where
  toGVal (Item t) = Ginger.toGVal $ escapeTeX t

instance TTC.Render Item where
  render (Item t) = TTC.fromT t

-- | Queue information
data Queue
  = Queue
    { queueName    :: !Name
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

-- | Load @sections.yaml@
loadSectionsYaml :: FilePath -> IO [Section]
loadSectionsYaml = loadYamlFile

-- | Load @queues.yaml@
loadQueuesYaml :: FilePath -> [Section] -> IO [Queue]
loadQueuesYaml path sections = do
    queues <- loadYamlFile path
    forM_ queues $ \Queue{..} ->
      unless (queueSection `elem` sections) . errorExit . unwords $
        [ "queue", TTC.render queueName
        , "has unknown section", TTC.render queueSection
        ]
    return queues

-- | Load a YAML file
--
-- If there is an error, the error is displayed and the program is exited.
loadYamlFile :: FromJSON a => FilePath -> IO [a]
loadYamlFile path = do
    eer <- Yaml.decodeFileEither path
    case eer of
      Right result -> return result
      Left err -> errorExit $
        "error loading " ++ path ++ ": " ++ Yaml.prettyPrintParseException err

------------------------------------------------------------------------------
-- $Template

-- | Queue context
data QueueCtx
  = QueueCtx
    { name       :: !Name
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

-- | Display an error and exit the program
errorExit :: String -> IO a
errorExit msg = do
    putStrLn $ "error: " ++ msg
    exitFailure

-- | CLI parser information
pinfo :: OA.ParserInfo FilePath
pinfo
    = OA.info (LibOA.helper <*> LibOA.versioner version <*> queuesYaml)
    $ mconcat
        [ OA.fullDesc
        , OA.progDesc "queue sheet utility"
        , OA.failureCode 2
        , OA.footerDoc $ Just filesHelp
        ]
  where
    version :: String
    version = "queue-sheet-haskell " ++ showVersion Project.version

    queuesYaml :: OA.Parser FilePath
    queuesYaml = OA.strArgument $ mconcat
      [ OA.metavar "QUEUES.yaml"
      , OA.help "YAML file specifying queue information"
      ]

    filesHelp :: Doc
    filesHelp = LibOA.section "Files:" $ LibOA.table
      [ ("sections.yaml", "specify section names and order")
      , ("QUEUES.yaml",   "specify queues information")
      , ("template.tex",  "queue sheet template")
      , ("QUEUES.pdf",    "output queue sheet")
      ]

-- | Main function
main :: IO ()
main = do
    queuesYaml <- OA.execParser pinfo
    sections   <- loadSectionsYaml "sections.yaml"
    queues     <- loadQueuesYaml queuesYaml sections
    template   <- loadTemplate "template.tex"
    exists     <- doesPathExist buildDir
    when exists . errorExit $ "directory already exists: " ++ buildDir
    let queuesTex = replaceExtension queuesYaml "tex"
        queuesPdf = replaceExtension queuesYaml "pdf"
    createDirectory buildDir
    withCurrentDirectory buildDir $ do
      renderTemplate queuesTex template $ context sections queues
      build queuesTex
    renameFile (buildDir </> queuesPdf) queuesPdf
    removeDirectoryRecursive buildDir
