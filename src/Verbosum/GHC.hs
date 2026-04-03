{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Verbosum.GHC
  ( -- * GHC subprocess
    runGHCDiagnostics
  , GHCOption(..)
  , typeCheckFile

    -- * Diagnostic parsing
  , extractHoleInfo
  , isHoleDiagnostic
  , enrichHoleWithDiagnostic
  ) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe, Parser)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Process
import System.Exit (ExitCode(..))
import Verbosum.Types

data GHCOption = GHCDeferTypedHoles deriving stock (Eq, Show)

renderGHCOption :: GHCOption -> String
renderGHCOption = \case
  GHCDeferTypedHoles -> "-fdefer-typed-holes"

-- | Run GHC with JSON diagnostics on a file
-- Returns either an error or a list of diagnostics
runGHCDiagnostics :: [GHCOption] -> FilePath -> IO (Either Text [GHCDiagnostic])
runGHCDiagnostics opts filePath = do
  let args = ["-fdiagnostics-as-json", "-fno-code"] ++ fmap renderGHCOption opts ++ [filePath]
  (exitCode, _stdout, stderr) <- readProcessWithExitCode "ghc" args ""
  let output = BLC.pack stderr  -- GHC writes diagnostics to stderr
  case parseDiagnostics output of
    Right diags -> pure (Right diags)
    Left err -> case exitCode of
      ExitSuccess -> pure (Right [])
      ExitFailure _ ->
        -- Try to parse anyway, GHC may have emitted partial diagnostics
        pure (Left $ "GHC error: " <> T.pack err <> "\nstderr: " <> T.pack stderr)

-- | Parse GHC JSON diagnostics output
-- GHC emits one JSON object per line
parseDiagnostics :: BL.ByteString -> Either String [GHCDiagnostic]
parseDiagnostics output =
  let lns = filter (not . BL.null) $ BLC.lines output
  in sequence $ map parseDiagnosticLine lns

-- | Parse a single diagnostic line
parseDiagnosticLine :: BL.ByteString -> Either String GHCDiagnostic
parseDiagnosticLine line =
  case eitherDecode line of
    Left err -> Left $ "Failed to parse diagnostic: " <> err
    Right val -> case parseMaybe parseDiagnosticFromJSON val of
      Nothing -> Left "Failed to extract diagnostic fields"
      Just diag -> Right diag

-- | Parse a GHCDiagnostic from JSON
parseDiagnosticFromJSON :: Value -> Parser GHCDiagnostic
parseDiagnosticFromJSON = withObject "GHCDiagnostic" $ \obj -> do
  code <- obj .:? "code"
  severity <- obj .: "severity"
  spanObj <- obj .: "span"
  file <- spanObj .: "file"
  startArr <- spanObj .: "start"
  endArr <- spanObj .: "end"
  startLine <- startArr .: "line"
  startCol <- startArr .: "column"
  endLine <- endArr .: "line"
  endCol <- endArr .: "column"
  messageArr <- obj .: "message"
  msgs <- parseMessageArray messageArr
  pure GHCDiagnostic
    { diagCode = code
    , diagSeverity = severity
    , diagSpan = DiagnosticSpan
        { diagFile = file
        , diagStartLine = startLine
        , diagStartCol = startCol
        , diagEndLine = endLine
        , diagEndCol = endCol
        }
    , diagMessage = msgs
    }

-- | Parse the message array from GHC diagnostics
parseMessageArray :: Value -> Parser [Text]
parseMessageArray = withArray "message" $ \arr ->
  mapM extractText (toList arr)
  where
    extractText (String t) = pure t
    extractText (Object obj) = obj .: "text"
    extractText _ = pure ""
    toList = foldr (:) []

-- | Check if a diagnostic is a typed hole (code 88464)
isHoleDiagnostic :: GHCDiagnostic -> Bool
isHoleDiagnostic diag = diagCode diag == Just 88464

-- | Extract hole information from a diagnostic
-- Returns (type, bindings, valid fits)
extractHoleInfo :: GHCDiagnostic -> (Maybe Text, [Binding], [Text])
extractHoleInfo diag =
  let msgs = diagMessage diag
      fullMsg = T.unlines msgs
      holeType = extractHoleType fullMsg
      bindings = extractBindings fullMsg
      validFits = extractValidFits fullMsg
  in (holeType, bindings, validFits)

-- | Extract the type from a hole diagnostic message
-- Format: "Found hole: _name :: Type"
extractHoleType :: Text -> Maybe Text
extractHoleType msg =
  case T.breakOn "::" msg of
    (_, rest) | T.null rest -> Nothing
    (_, rest) ->
      let afterColons = T.drop 2 rest
          -- Take until newline or "In"
          typePart = T.strip $ T.takeWhile (\c -> c /= '\n') afterColons
      in if T.null typePart then Nothing else Just typePart

-- | Extract relevant bindings from a hole diagnostic
-- Format: "Relevant bindings include\n  name :: Type (bound at ...)"
extractBindings :: Text -> [Binding]
extractBindings msg =
  case T.breakOn "Relevant bindings include" msg of
    (_, rest) | T.null rest -> []
    (_, rest) ->
      let afterHeader = T.drop (T.length "Relevant bindings include") rest
          lns = T.lines afterHeader
          bindingLines = takeWhile isBindingLine lns
      in mapMaybe parseBinding bindingLines
  where
    isBindingLine line =
      let stripped = T.strip line
      in not (T.null stripped)
         && not ("Valid" `T.isPrefixOf` stripped)
         && not ("In" `T.isPrefixOf` stripped)
         && "::" `T.isInfixOf` stripped

    parseBinding line =
      let stripped = T.strip line
      in case T.breakOn "::" stripped of
           (_, rest) | T.null rest -> Nothing
           (name', rest) ->
             let typePart = T.strip $ T.drop 2 rest
                 -- Remove "(bound at ...)" suffix
                 cleanType = T.strip $ fst $ T.breakOn "(bound at" typePart
             in Just Binding
                  { bindingName = T.strip name'
                  , bindingType = cleanType
                  }

-- | Extract valid hole fits from a diagnostic
-- Format: "Valid hole fits include\n  name :: Type"
extractValidFits :: Text -> [Text]
extractValidFits msg =
  case T.breakOn "Valid hole fits include" msg of
    (_, rest) | T.null rest -> []
    (_, rest) ->
      let afterHeader = T.drop (T.length "Valid hole fits include") rest
          lns = T.lines afterHeader
          fitLines = takeWhile isFitLine (drop 1 lns)
      in map (T.strip . fst . T.breakOn "::") fitLines
  where
    isFitLine line =
      let stripped = T.strip line
      in not (T.null stripped)
         && not ("In" `T.isPrefixOf` stripped)

-- | Type check a file and return whether it succeeded along with diagnostics
typeCheckFile :: FilePath -> IO (Bool, [GHCDiagnostic])
typeCheckFile filePath = do
  result <- runGHCDiagnostics [] filePath
  case result of
    Left _ -> pure (False, [])
    Right diags ->
      let hasErrors = any (\d -> diagSeverity d == "error") diags
      in pure (not hasErrors, diags)

-- | Enrich a hole with type information from GHC diagnostics
enrichHoleWithDiagnostic :: Hole -> [GHCDiagnostic] -> Hole
enrichHoleWithDiagnostic hole diags =
  let matchingDiag = findMatchingDiagnostic hole diags
  in case matchingDiag of
       Nothing -> hole
       Just diag ->
         let (hType, bindings, fits) = extractHoleInfo diag
         in hole
              { holeType = hType
              , holeBindings = bindings
              , holeValidFits = fits
              }

-- | Find a diagnostic that matches a hole's location
findMatchingDiagnostic :: Hole -> [GHCDiagnostic] -> Maybe GHCDiagnostic
findMatchingDiagnostic hole = foldr findMatch Nothing
  where
    findMatch diag acc
      | isHoleDiagnostic diag && diagMatchesHole diag hole = Just diag
      | otherwise = acc

    diagMatchesHole diag h =
      let dSpan = diagSpan diag
          hSpan = holeSpan h
      in diagStartLine dSpan == spanStartLine hSpan
         && diagStartCol dSpan == spanStartCol hSpan
