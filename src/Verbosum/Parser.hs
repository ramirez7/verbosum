{-# LANGUAGE OverloadedStrings #-}

module Verbosum.Parser
  ( -- * Parsing
    parseHoles
  , parseClearlyBlocks
  , findModuleClearly
  , findHoleClearly
  , removeClearlyBlocks

    -- * Context extraction
  , extractSurroundingContext
  ) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import Verbosum.Types

-- | Parse all hole names and their positions from source code
-- Holes are identifiers starting with underscore: _name, _impl, _x, etc.
parseHoles :: Text -> [Hole]
parseHoles source = findHoles source 1 1

-- | Find holes by scanning the source character by character
findHoles :: Text -> Int -> Int -> [Hole]
findHoles source lineNum colNum
  | T.null source = []
  | T.head source == '_' =
      let (holeName', rest) = T.span isHoleChar source
      in if T.length holeName' > 1  -- Must have at least one char after _
         then let hole = makeHole holeName' (lineNum, colNum)
                  newCol = colNum + T.length holeName'
              in hole : findHoles rest lineNum newCol
         else -- Just a lone underscore, skip it
              let (newLine, newCol) = advance lineNum colNum '_'
              in findHoles (T.tail source) newLine newCol
  | otherwise =
      let c = T.head source
          (newLine, newCol) = advance lineNum colNum c
      in findHoles (T.tail source) newLine newCol

-- | Check if a character is valid in a hole identifier
isHoleChar :: Char -> Bool
isHoleChar c = c == '_' || c == '\'' || isAlphaNum c

-- | Advance line/col position for a character
advance :: Int -> Int -> Char -> (Int, Int)
advance line _ '\n' = (line + 1, 1)
advance line col _ = (line, col + 1)

-- | Make a Hole from a name and position
makeHole :: Text -> (Int, Int) -> Hole
makeHole name (line, col) = Hole
  { holeName = name
  , holeSpan = SourceSpan line col line (col + T.length name)
  , holeType = Nothing
  , holeBindings = []
  , holeValidFits = []
  }

-- | Parse all CLEARLY blocks from source code
-- Format: {- CLEARLY ... -}
parseClearlyBlocks :: Text -> [ClearlyBlock]
parseClearlyBlocks source = findClearlyBlocks source 1 1

-- | Find CLEARLY blocks by scanning the source
findClearlyBlocks :: Text -> Int -> Int -> [ClearlyBlock]
findClearlyBlocks source lineNum colNum
  | T.null source = []
  | "{- CLEARLY" `T.isPrefixOf` source || "{-CLEARLY" `T.isPrefixOf` source =
      case findBlockEnd source of
        Nothing -> []  -- Unclosed block
        Just (blockText, rest) ->
          let startLine = lineNum
              startCol = colNum
              (endLine, endCol) = computeEndPos blockText lineNum colNum
              content = extractClearlyContent blockText
              block = ClearlyBlock
                { clearlySpan = SourceSpan startLine startCol endLine endCol
                , clearlyContent = content
                }
              (newLine, newCol) = advancePos lineNum colNum blockText
          in block : findClearlyBlocks rest newLine newCol
  | otherwise =
      let c = T.head source
          (newLine, newCol) = advance lineNum colNum c
      in findClearlyBlocks (T.tail source) newLine newCol

-- | Find the end of a CLEARLY block (-})
findBlockEnd :: Text -> Maybe (Text, Text)
findBlockEnd source =
  case T.breakOn "-}" source of
    (_, rest) | T.null rest -> Nothing
    (before, rest) -> Just (before <> "-}", T.drop 2 rest)

-- | Compute end position after consuming a block
computeEndPos :: Text -> Int -> Int -> (Int, Int)
computeEndPos blockText startLine startCol =
  let lns = T.lines blockText
      numLines = length lns
  in if numLines <= 1
     then (startLine, startCol + T.length blockText)
     else (startLine + numLines - 1, T.length (last lns) + 1)

-- | Advance position after consuming text
advancePos :: Int -> Int -> Text -> (Int, Int)
advancePos lineNum colNum txt =
  let lns = T.lines txt
  in case lns of
       [] -> (lineNum, colNum)
       [single] -> (lineNum, colNum + T.length single)
       _ -> (lineNum + length lns - 1, T.length (last lns) + 1)

-- | Extract content from a CLEARLY block, stripping the markers
extractClearlyContent :: Text -> Text
extractClearlyContent block =
  let -- Remove {- CLEARLY prefix and -} suffix
      withoutPrefix = T.drop 2 block  -- Remove {-
      trimmed = T.stripStart withoutPrefix
      withoutClearly = if "CLEARLY" `T.isPrefixOf` trimmed
                       then T.drop 7 trimmed
                       else trimmed
      withoutSuffix = if "-}" `T.isSuffixOf` withoutClearly
                      then T.dropEnd 2 withoutClearly
                      else withoutClearly
  in T.strip withoutSuffix

-- | Find the module-level CLEARLY block (appears before first declaration)
-- A module-level CLEARLY appears at the top of the file, before any function definitions
findModuleClearly :: Text -> [ClearlyBlock] -> Maybe ClearlyBlock
findModuleClearly source blocks =
  let firstDeclLine = findFirstDeclarationLine source
  in case filter (\b -> spanEndLine (clearlySpan b) < firstDeclLine) blocks of
       (c:_) -> Just c
       [] -> Nothing

-- | Find the line number of the first declaration (after module header)
findFirstDeclarationLine :: Text -> Int
findFirstDeclarationLine source =
  let sourceLines = zip [1..] (T.lines source)
      -- Skip module header, imports, and blank/comment lines
      isDeclarationLine t =
        not (T.null (T.strip t))
        && not ("module " `T.isPrefixOf` T.strip t)
        && not ("import " `T.isPrefixOf` T.strip t)
        && not ("--" `T.isPrefixOf` T.strip t)
        && not ("{-" `T.isPrefixOf` T.strip t)
        && not ("-}" `T.isSuffixOf` T.strip t)
  in case filter (isDeclarationLine . snd) sourceLines of
       ((n, _):_) -> n
       [] -> maxBound  -- No declarations found

-- | Find a CLEARLY block that immediately precedes a hole
findHoleClearly :: [ClearlyBlock] -> Hole -> Maybe ClearlyBlock
findHoleClearly blocks hole =
  let holeStartLine = spanStartLine (holeSpan hole)
      -- A CLEARLY block "immediately precedes" a hole if it ends
      -- within a few lines before the hole starts
      candidates = filter (isImmediatelyBefore holeStartLine) blocks
  in case candidates of
       [] -> Nothing
       cs -> Just $ maximumBy' (\a b -> compare (spanEndLine (clearlySpan a)) (spanEndLine (clearlySpan b))) cs
  where
    isImmediatelyBefore holeLine block =
      let blockEnd = spanEndLine (clearlySpan block)
      in blockEnd > 0 && blockEnd < holeLine && (holeLine - blockEnd) <= 5
    maximumBy' f xs = foldr1 (\a b -> if f a b == GT then a else b) xs

-- | Remove all CLEARLY blocks from source code
removeClearlyBlocks :: Text -> [ClearlyBlock] -> Text
removeClearlyBlocks source blocks =
  -- Sort blocks by start line in reverse order to remove from end first
  let sortedBlocks = sortByStartLine blocks
  in foldl removeBlock source sortedBlocks
  where
    sortByStartLine = foldr insertSorted []
    insertSorted b [] = [b]
    insertSorted b (x:xs)
      | spanStartLine (clearlySpan b) > spanStartLine (clearlySpan x) = b : x : xs
      | otherwise = x : insertSorted b xs

    removeBlock src block =
      let SourceSpan sl _ el _ = clearlySpan block
          lns = T.lines src
          (before, rest) = splitAt (sl - 1) lns
          (_, after) = splitAt (el - sl + 1) rest
          result = T.unlines $ before ++ after
      in result

-- | Extract surrounding context around a hole (N lines before and after)
extractSurroundingContext :: Text -> Hole -> Int -> Text
extractSurroundingContext source hole contextLines =
  let lns = T.lines source
      holeLine = spanStartLine (holeSpan hole)
      startLine = max 1 (holeLine - contextLines)
      endLine = min (length lns) (holeLine + contextLines)
      contextLns = take (endLine - startLine + 1) $ drop (startLine - 1) lns
  in T.unlines contextLns
