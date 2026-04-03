module Example where

{- CLEARLY
This module implements a simple greeter.
- foo should be a no-op transformation
- bar should return a friendly greeting
- They work together: bar uses foo internally
-}

foo :: Int -> Int
foo = _impl

{- CLEARLY return "Hello, World!" -}
bar :: String
bar = _greeting
