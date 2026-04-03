{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Verbosum.Types
  ( -- * Source Locations
    SourceSpan(..)
  , sourceSpanContains

    -- * Holes
  , Hole(..)
  , ClearlyBlock(..)
  , EnrichedHole(..)

    -- * GHC Diagnostics
  , GHCDiagnostic(..)
  , DiagnosticSpan(..)
  , Binding(..)

    -- * Configuration
  , Config(..)
  , defaultConfig
  ) where

import Data.Text (Text)

-- | A span of source code with line and column information
data SourceSpan = SourceSpan
  { spanStartLine :: Int
  , spanStartCol :: Int
  , spanEndLine :: Int
  , spanEndCol :: Int
  } deriving (Show, Eq)

-- | Check if a position is within a source span
sourceSpanContains :: SourceSpan -> Int -> Int -> Bool
sourceSpanContains span' line col =
  (line > spanStartLine span' || (line == spanStartLine span' && col >= spanStartCol span'))
  && (line < spanEndLine span' || (line == spanEndLine span' && col <= spanEndCol span'))

-- | A typed hole found in the source code
data Hole = Hole
  { holeName :: Text
  , holeSpan :: SourceSpan
  , holeType :: Maybe Text
  , holeBindings :: [Binding]
  , holeValidFits :: [Text]
  } deriving (Show, Eq)

-- | A CLEARLY directive block with instructions
data ClearlyBlock = ClearlyBlock
  { clearlySpan :: SourceSpan
  , clearlyContent :: Text
  } deriving (Show, Eq)

-- | A binding available in scope at a hole
data Binding = Binding
  { bindingName :: Text
  , bindingType :: Text
  } deriving (Show, Eq)

-- | A hole enriched with context for filling
data EnrichedHole = EnrichedHole
  { enrichedHole :: Hole
  , enrichedModuleClearly :: Maybe ClearlyBlock  -- ^ Module-level instructions for all holes
  , enrichedHoleClearly :: Maybe ClearlyBlock    -- ^ Hole-specific instructions
  , enrichedContext :: Text                       -- ^ Surrounding code context
  } deriving (Show, Eq)

-- | Location information from GHC diagnostics
data DiagnosticSpan = DiagnosticSpan
  { diagFile :: Text
  , diagStartLine :: Int
  , diagStartCol :: Int
  , diagEndLine :: Int
  , diagEndCol :: Int
  } deriving (Show, Eq)

-- | A diagnostic message from GHC
data GHCDiagnostic = GHCDiagnostic
  { diagCode :: Maybe Int
  , diagSeverity :: Text
  , diagSpan :: DiagnosticSpan
  , diagMessage :: [Text]
  } deriving (Show, Eq)

-- | Configuration for verbosum
data Config = Config
  { configModel :: Text
  , configMaxTokens :: Int
  , configRetries :: Int
  , configVerbose :: Bool
  , configDryRun :: Bool
  } deriving (Show, Eq)

-- | Default configuration
defaultConfig :: Config
defaultConfig = Config
  { configModel = "claude-sonnet-4-5-20250929"
  , configMaxTokens = 4096
  , configRetries = 3
  , configVerbose = False
  , configDryRun = False
  }
