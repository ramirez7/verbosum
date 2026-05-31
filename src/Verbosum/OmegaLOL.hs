module Verbosum.OmegaLOL where

data LC =
    V String
  | F String LC
  | A LC LC
  deriving stock (Show, Eq)

-- test ideas: property test that parens are balanced
renderLC :: LC -> String
renderLC = \case
  V v -> v
  F b lc -> mconcat ["(λ", b, ".", renderLC lc, ")"]
  A f (x@A{}) -> mconcat ["(", renderLC f, " ", renderLC x, ")"]
  A f x -> mconcat [renderLC f, " ", renderLC x]

-- (λx.x x) (λx.x x)
-- (λx.x x) ((λx.x x) (λx.x x))
omega :: String -> String -> LC
omega x y = A (F x (A (V x) (V x))) (F y (A (V y) (V y)))

omegaLOL :: [String] -> LC
omegaLOL = \case
  [] -> error "bad"
  x : y : [] -> omega x y
  x : xs -> A (F x (A (V x) (V x))) (omegaLOL xs)
