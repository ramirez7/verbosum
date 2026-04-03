{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Verbosum.Claude
  ( -- * Client
    ClaudeClient(..)
  , initClaudeClient

    -- * Hole filling
  , fillHole
  , buildPrompt
  ) where

import Claude.V1 (makeMethods, Methods(..), getClientEnv)
import Claude.V1.Messages
import Control.Exception (try, SomeException)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Numeric.Natural (Natural)
import Verbosum.Types

-- | Claude API client
data ClaudeClient = ClaudeClient
  { clientMethods :: Methods
  , clientModel :: Text
  , clientMaxTokens :: Natural
  }

-- | Initialize a Claude client from an API key
initClaudeClient :: Text -> Text -> Natural -> IO ClaudeClient
initClaudeClient apiKey model maxTokens = do
  clientEnv <- getClientEnv "https://api.anthropic.com"
  let methods = makeMethods clientEnv apiKey (Just "2023-06-01")
  pure ClaudeClient
    { clientMethods = methods
    , clientModel = model
    , clientMaxTokens = maxTokens
    }

-- | Fill a hole using Claude
-- Returns either an error message or the filled expression
fillHole :: ClaudeClient -> EnrichedHole -> Text -> IO (Either Text Text)
fillHole client enrichedHole fullSource = do
  let prompt = buildPrompt enrichedHole fullSource
      request = _CreateMessage
        { model = clientModel client
        , messages = V.singleton Message
            { role = User
            , content = V.singleton (textContent prompt)
            , cache_control = Nothing
            }
        , max_tokens = clientMaxTokens client
        , system = Just $ systemText systemPrompt
        }
  result <- try $ createMessage (clientMethods client) request
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ "Claude API error: " <> T.pack (show e)
    Right MessageResponse{content} ->
      case extractTextResponse content of
        Nothing -> pure $ Left "No text response from Claude"
        Just response -> pure $ Right $ cleanResponse response

-- | System prompt for hole filling
systemPrompt :: Text
systemPrompt = T.unlines
  [ "You are a Haskell expert assistant that fills typed holes in Haskell code."
  , "When given a typed hole, you provide ONLY the Haskell expression that should replace the hole."
  , "Do not include any explanation, markdown formatting, or code blocks."
  , "Do not include the variable name or equals sign - only the right-hand side expression."
  , "The expression must type-check with the given type signature."
  , "Be concise and idiomatic."
  ]

-- | Build the prompt for filling a hole
buildPrompt :: EnrichedHole -> Text -> Text
buildPrompt EnrichedHole{..} fullSource =
  let Hole{..} = enrichedHole
      SourceSpan{..} = holeSpan
  in T.unlines $ concat
    [ [ "# Typed Hole to Fill"
      , ""
      , "**Hole name:** " <> holeName
      , "**Location:** Line " <> T.pack (show spanStartLine) <> ", Column " <> T.pack (show spanStartCol)
      ]
    , case holeType of
        Just t -> ["**Type:** " <> t, ""]
        Nothing -> [""]
    , if null holeBindings then [] else
        [ "## Relevant Bindings"
        , ""
        ] ++ map formatBinding holeBindings ++ [""]
    , if null holeValidFits then [] else
        [ "## Valid Hole Fits (GHC suggestions)"
        , ""
        ] ++ map (\f -> "- " <> f) holeValidFits ++ [""]
    , case enrichedModuleClearly of
        Just ClearlyBlock{clearlyContent} ->
          [ "## Module-Level Context (applies to all holes)"
          , ""
          , clearlyContent
          , ""
          ]
        Nothing -> []
    , case enrichedHoleClearly of
        Just ClearlyBlock{clearlyContent} ->
          [ "## Specific Instructions for This Hole"
          , ""
          , clearlyContent
          , ""
          ]
        Nothing -> []
    , [ "## Surrounding Code"
      , ""
      , "```haskell"
      , enrichedContext
      , "```"
      , ""
      , "## Full Source File"
      , ""
      , "```haskell"
      , fullSource
      , "```"
      , ""
      , "## Instructions"
      , ""
      , "Provide ONLY the Haskell expression to replace `" <> holeName <> "`. No explanation, no markdown, no code blocks."
      ]
    ]
  where
    formatBinding Binding{..} = "- " <> bindingName <> " :: " <> bindingType

-- | Extract text response from Claude's content blocks
extractTextResponse :: V.Vector ContentBlock -> Maybe Text
extractTextResponse blocks =
  let textBlocks = V.filter isTextBlock blocks
  in if V.null textBlocks
     then Nothing
     else Just $ T.unlines $ V.toList $ V.map getBlockText textBlocks
  where
    isTextBlock (ContentBlock_Text{}) = True
    isTextBlock _ = False

    getBlockText (ContentBlock_Text{text}) = text
    getBlockText _ = ""

-- | Clean up Claude's response
-- Remove any accidental markdown formatting, code blocks, etc.
cleanResponse :: Text -> Text
cleanResponse response =
  let stripped = T.strip response
      -- Remove markdown code blocks if present
      withoutCodeBlocks = removeCodeBlocks stripped
      -- Remove any leading/trailing quotes
      withoutQuotes = T.dropWhile (== '`') $ T.dropWhileEnd (== '`') withoutCodeBlocks
  in T.strip withoutQuotes

-- | Remove markdown code blocks
removeCodeBlocks :: Text -> Text
removeCodeBlocks t
  | "```haskell" `T.isPrefixOf` t =
      let afterStart = T.drop 10 t
          beforeEnd = T.dropWhileEnd (/= '`') $ T.dropEnd 3 afterStart
      in T.strip beforeEnd
  | "```" `T.isPrefixOf` t =
      let afterStart = T.drop 3 t
          -- Skip language identifier if present
          afterLang = T.dropWhile (/= '\n') afterStart
          content = T.drop 1 afterLang  -- Skip the newline
          beforeEnd = T.dropWhileEnd (/= '`') $ T.dropEnd 3 content
      in T.strip beforeEnd
  | otherwise = t
