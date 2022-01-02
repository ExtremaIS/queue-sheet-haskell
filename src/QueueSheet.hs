------------------------------------------------------------------------------
-- |
-- Module      : QueueSheet
-- Description : queue sheet metadata
-- Copyright   : Copyright (c) 2020-2022 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module QueueSheet
  ( -- * Constants
    version
  ) where

-- https://hackage.haskell.org/package/base
import Data.Version (showVersion)

-- (queue-sheet:cabal)
import qualified Paths_queue_sheet as Project

------------------------------------------------------------------------------
-- $Constants

-- | Version
version :: String
version = "queue-sheet-haskell " ++ showVersion Project.version
