{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Verbosum.CLI
  ( runCLI
  , Options(..)
  , parseOptions
  ) where

import Control.Monad (foldM, when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Numeric.Natural (Natural)
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Verbosum.Types
import Verbosum.Claude
import Verbosum.Filler

-- | CLI options
data Options = Options
  { optModel :: Text
  , optMaxTokens :: Natural
  , optRetries :: Int
  , optVerbose :: Bool
  , optDryRun :: Bool
  , optFiles :: [FilePath]
  } deriving (Show, Eq)

-- | Parse CLI options
parseOptions :: Parser Options
parseOptions = Options
  <$> strOption
      ( long "model"
     <> short 'm'
     <> metavar "MODEL"
     <> value "claude-sonnet-4-5-20250929"
     <> help "Claude model to use (default: claude-sonnet-4-5-20250929)"
      )
  <*> option auto
      ( long "max-tokens"
     <> short 't'
     <> metavar "N"
     <> value 4096
     <> help "Max tokens per response (default: 4096)"
      )
  <*> option auto
      ( long "retries"
     <> short 'r'
     <> metavar "N"
     <> value 3
     <> help "Max retries on type errors (default: 3)"
      )
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Verbose output"
      )
  <*> switch
      ( long "dry-run"
     <> short 'n'
     <> help "Print changes without applying"
      )
  <*> some (argument str (metavar "FILES..."))

-- | Parser info with help text
parserInfo :: ParserInfo Options
parserInfo = info (parseOptions <**> helper)
  ( fullDesc
 <> progDesc "Fill typed holes in Haskell code using Claude AI"
 <> header "verbosum - AI-powered typed hole filler"
  )

-- | Main entry point
runCLI :: IO ()
runCLI = do
  options <- execParser parserInfo

  -- Check for API key
  maybeApiKey <- lookupEnv "ANTHROPIC_API_KEY"
  apiKey <- case maybeApiKey of
    Nothing -> do
      hPutStrLn stderr "Error: ANTHROPIC_API_KEY environment variable is not set"
      exitFailure
    Just key -> pure (T.pack key)

  -- Build config
  let config = Config
        { configModel = optModel options
        , configMaxTokens = fromIntegral (optMaxTokens options)
        , configRetries = optRetries options
        , configVerbose = optVerbose options
        , configDryRun = optDryRun options
        }

  -- Initialize Claude client
  client <- initClaudeClient apiKey (configModel config) (optMaxTokens options)

  when (optVerbose options) $ do
    putStrLn $ "Using model: " ++ T.unpack (configModel config)
    putStrLn $ "Processing " ++ show (length (optFiles options)) ++ " file(s)"
    putStrLn ""

  -- Process files
  results <- processFiles config client (optFiles options)

  -- Handle results
  hasErrors <- foldM (handleResult options) False results

  if hasErrors
    then exitFailure
    else exitSuccess

-- | Handle the result for a single file
handleResult :: Options -> Bool -> (FilePath, Either Text Text) -> IO Bool
handleResult options hadErrors (filePath, result) =
  case result of
    Left err -> do
      hPutStrLn stderr $ "Error processing " ++ filePath ++ ": " ++ T.unpack err
      pure True
    Right newSource -> do
      if optDryRun options
        then do
          putStrLn $ "=== " ++ filePath ++ " (dry run) ==="
          TIO.putStr newSource
          putStrLn ""
        else do
          TIO.writeFile filePath newSource
          when (optVerbose options) $
            putStrLn $ "Updated: " ++ filePath
      pure hadErrors
