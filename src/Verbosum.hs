{-# LANGUAGE OverloadedStrings #-}

-- | Verbosum - AI-powered typed hole filler for Haskell
--
-- Verbosum is a CLI tool that uses the Claude API to automatically fill
-- typed holes in Haskell code. Users leave @_holes@ in their code with
-- optional @{- CLEARLY ... -}@ directive comments, then run @verbosum [FILE...]@
-- to have an LLM fill them based on types, context, and instructions.
--
-- = Usage
--
-- @
-- verbosum [OPTIONS] FILES...
--   -m, --model MODEL      Claude model (default: claude-sonnet-4-5-20250929)
--   -t, --max-tokens N     Max tokens per response (default: 4096)
--   -r, --retries N        Max retries on type errors (default: 3)
--   -v, --verbose          Verbose output
--   -n, --dry-run          Print changes without applying
-- @
--
-- Requires @ANTHROPIC_API_KEY@ environment variable.
--
-- = CLEARLY Directives
--
-- You can provide instructions to guide hole filling using CLEARLY blocks:
--
-- == Module-Level CLEARLY
--
-- A @{- CLEARLY ... -}@ block at the top of the file (before any declarations)
-- provides freeform context about all holes:
--
-- @
-- module Example where
--
-- {- CLEARLY
-- This module implements a simple greeter.
-- - foo should be a no-op transformation
-- - bar should return a friendly greeting
-- -}
--
-- foo :: Int -> Int
-- foo = _impl
-- @
--
-- == Hole-Specific CLEARLY
--
-- A @{- CLEARLY ... -}@ immediately before a hole provides specific instructions:
--
-- @
-- {- CLEARLY return "Hello, World!" -}
-- bar :: String
-- bar = _greeting
-- @
--
-- Both types of CLEARLY blocks are removed from the final output after holes
-- are filled.
module Verbosum
  ( -- * CLI
    runCLI

    -- * Core Types
  , Config(..)
  , defaultConfig
  , Hole(..)
  , ClearlyBlock(..)
  , EnrichedHole(..)
  , SourceSpan(..)

    -- * Parsing
  , parseHoles
  , parseClearlyBlocks
  , removeClearlyBlocks

    -- * GHC Integration
  , runGHCDiagnostics
  , typeCheckFile

    -- * Claude Integration
  , ClaudeClient(..)
  , initClaudeClient
  , fillHole

    -- * File Processing
  , processFile
  , processFiles
  ) where

import Verbosum.CLI (runCLI)
import Verbosum.Types
  ( Config(..)
  , defaultConfig
  , Hole(..)
  , ClearlyBlock(..)
  , EnrichedHole(..)
  , SourceSpan(..)
  )
import Verbosum.Parser
  ( parseHoles
  , parseClearlyBlocks
  , removeClearlyBlocks
  )
import Verbosum.GHC
  ( runGHCDiagnostics
  , typeCheckFile
  )
import Verbosum.Claude
  ( ClaudeClient(..)
  , initClaudeClient
  , fillHole
  )
import Verbosum.Filler
  ( processFile
  , processFiles
  )
