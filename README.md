# verbosum

> [*The proof is left as an exercise for the clanker.*](https://en.wikipedia.org/wiki/Proof_by_intimidation)

## Usage

1. Leave [typed holes](https://downloads.haskell.org/~ghc/7.10.3-rc1/users_guide/typed-holes.html) (`_whatever`) in your code.
2. Add additional directions in a `{- CLEARLY -}` block comment.
3. (optional but recommended) commit your changes.
4. Run `verbosum [FILE...]` and the LLM will implement all the holes per your `CLEARLY` instructions and inherent context in the code (types, comments, other code, etc). The `CLEARLY` comments will also be removed.
