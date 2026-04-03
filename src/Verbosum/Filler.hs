{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Verbosum.Filler
  ( -- * Main entry point
    processFile
  , processFiles

    -- * Internal for testing
  , enrichHoles
  , replaceHole
  , fillWithRetry
  ) where

import Control.Monad (foldM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Verbosum.Types
import Verbosum.Parser
import Verbosum.GHC
import Verbosum.Claude

-- | Process multiple files
processFiles :: Config -> ClaudeClient -> [FilePath] -> IO [(FilePath, Either Text Text)]
processFiles config client = mapM (\fp -> (fp,) <$> processFile config client fp)

-- | Process a single file, filling all holes
processFile :: Config -> ClaudeClient -> FilePath -> IO (Either Text Text)
processFile config client filePath = do
  source <- TIO.readFile filePath

  -- Parse holes and CLEARLY blocks
  let holes = parseHoles source
      clearlyBlocks = parseClearlyBlocks source
      moduleClearly = findModuleClearly source clearlyBlocks

  when (configVerbose config) $ do
    putStrLn $ "Found " ++ show (length holes) ++ " holes in " ++ filePath
    putStrLn $ "Found " ++ show (length clearlyBlocks) ++ " CLEARLY blocks"

  if null holes
    then pure $ Right source
    else do
      -- Get GHC diagnostics for type information
      diagResult <- runGHCDiagnostics filePath
      case diagResult of
        Left err -> pure $ Left $ "GHC error: " <> err
        Right diags -> do
          -- Enrich holes with type info and context
          let enrichedHoles' = enrichHoles source holes clearlyBlocks moduleClearly diags

          -- Fill each hole
          filledSource <- foldM (fillNextHole config client) source enrichedHoles'

          -- Remove CLEARLY blocks from final output
          let finalSource = removeClearlyBlocks filledSource clearlyBlocks

          -- Final type check
          (success, _) <- typeCheckFile filePath
          when (configVerbose config) $
            if success
              then putStrLn "Final type check: PASSED"
              else putStrLn "Final type check: FAILED (some holes may not have been filled correctly)"

          pure $ Right finalSource
  where
    when cond action = if cond then action else pure ()

-- | Enrich holes with type information and context
enrichHoles :: Text -> [Hole] -> [ClearlyBlock] -> Maybe ClearlyBlock -> [GHCDiagnostic] -> [EnrichedHole]
enrichHoles source holes clearlyBlocks moduleClearly diags =
  map (enrichSingleHole source clearlyBlocks moduleClearly diags) holes

-- | Enrich a single hole
enrichSingleHole :: Text -> [ClearlyBlock] -> Maybe ClearlyBlock -> [GHCDiagnostic] -> Hole -> EnrichedHole
enrichSingleHole source clearlyBlocks moduleClearly diags hole =
  let enrichedWithType = enrichHoleWithDiagnostic hole diags
      holeClearly = findHoleClearly clearlyBlocks hole
      context = extractSurroundingContext source hole 15
  in EnrichedHole
    { enrichedHole = enrichedWithType
    , enrichedModuleClearly = moduleClearly
    , enrichedHoleClearly = holeClearly
    , enrichedContext = context
    }

-- | Fill the next hole in the source
fillNextHole :: Config -> ClaudeClient -> Text -> EnrichedHole -> IO Text
fillNextHole config client currentSource eHole = do
  let Hole{holeName = name', holeSpan = span'} = enrichedHole eHole

  when (configVerbose config) $
    putStrLn $ "Filling hole: " ++ T.unpack name'

  -- Fill with retry logic
  result <- fillWithRetry config client currentSource eHole

  case result of
    Left err -> do
      when (configVerbose config) $
        putStrLn $ "  Failed to fill: " ++ T.unpack err
      -- Keep original hole on failure
      pure currentSource
    Right expr -> do
      when (configVerbose config) $
        putStrLn $ "  Filled with: " ++ T.unpack expr
      -- Replace hole in source
      pure $ replaceHole currentSource span' expr
  where
    when cond action = if cond then action else pure ()

-- | Fill a hole with retry logic
fillWithRetry :: Config -> ClaudeClient -> Text -> EnrichedHole -> IO (Either Text Text)
fillWithRetry config client source eHole =
  go (configRetries config) Nothing
  where
    go 0 lastErr = pure $ Left $ maybe "Max retries exceeded" id lastErr
    go retriesLeft _ = do
      result <- fillHole client eHole source
      case result of
        Left err -> pure $ Left err
        Right expr -> do
          -- Try replacing and type checking
          let newSource = replaceHole source (holeSpan (enrichedHole eHole)) expr

          -- Write temporary file for type checking
          let tmpFile = "/tmp/verbosum_typecheck.hs"
          TIO.writeFile tmpFile newSource

          (success, diags) <- typeCheckFile tmpFile
          if success
            then pure $ Right expr
            else do
              -- Extract error message for context
              let errorMsg = T.unlines $ concatMap diagMessage $
                    filter (\d -> diagSeverity d == "error") diags
              when (configVerbose config) $
                putStrLn $ "  Type error, retrying... (" ++ show (retriesLeft - 1) ++ " retries left)"
              go (retriesLeft - 1) (Just errorMsg)

    when cond action = if cond then action else pure ()

-- | Replace a hole in source code with an expression
replaceHole :: Text -> SourceSpan -> Text -> Text
replaceHole source SourceSpan{..} expr =
  let lns = T.lines source
      (beforeLines, rest) = splitAt (spanStartLine - 1) lns
  in case rest of
       [] -> source  -- Shouldn't happen
       (holeLine:afterLines) ->
         let (beforeHole, afterStart) = T.splitAt (spanStartCol - 1) holeLine
             afterHole = T.drop (spanEndCol - spanStartCol) afterStart
             newLine = beforeHole <> expr <> afterHole
         in T.unlines $ beforeLines ++ [newLine] ++ afterLines
