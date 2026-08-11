{-# LANGUAGE OverloadedStrings #-}

module Verbosum.OmegaLOL where

import Data.ByteString.Builder qualified as B
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Text.Lazy qualified as TL

data LC =
    V String
  | F String LC
  | A LC LC
  | W LC
  deriving stock (Show, Eq)

-- test ideas: property test that parens are balanced
renderLC :: LC -> String
renderLC = TL.unpack . TLE.decodeUtf8 . B.toLazyByteString . buildLC

buildLC :: LC -> B.Builder
buildLC = \case
  V v -> B.stringUtf8 v
  F b lc -> mconcat ["(λ", B.stringUtf8 b, ".", buildLC lc, ")"]
  A f (x@(stripLCW -> A{})) -> mconcat ["(", buildLC f, " ", buildLC x, ")"]
  A f (x@(stripLCW -> F{})) -> mconcat ["(", buildLC f, " ", buildLC x, ")"]
  A f x -> mconcat [buildLC f, " ", buildLC x]
  W x -> mconcat ["\n", buildLC x]

stripLCW :: LC -> LC
stripLCW = \case
  W x -> stripLCW x
  x -> x

-- (λx.x x) (λx.x x)
-- (λx.x x) ((λx.x x) (λx.x x))
omega :: String -> String -> LC
omega x y = A (F x (A (V x) (V x))) (F y (A (V y) (V y)))

omegaLOL :: [String] -> LC
omegaLOL = \case
  [] -> error "bad"
  x : [] -> omega x x
  x : y : [] -> omega x y
  x : xs -> A (F x (A (V x) (V x))) (omegaLOL xs)

omegaLOL2D :: [[String]] -> LC
omegaLOL2D = \case
  [] -> error "bad 2d"
  ([] : rest) -> W (omegaLOL2D rest)
  (x : []) : [] -> omega x x
  (x : y : []) : [] -> omega x y
  (x : xs) : [] -> A (F x (A (V x) (V x))) (omegaLOL2D (xs : []))
  (x : []) : rest -> A (F x (A (V x) (V x))) (W (omegaLOL2D rest))
  (x : xs) : rest -> A (F x (A (V x) (V x))) (omegaLOL2D (xs : rest))
