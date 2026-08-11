module Verbosum.CLI where

import Options.Applicative
import Verbosum.OmegaLOL
import Control.Monad
import System.IO

main :: IO ()
main = join (execParser opts)
  where
    opts = info (mainP <**> helper)
      ( progDesc "verbosum!" )

type PIO a = Parser (IO ())

mainP :: PIO ()
mainP = hsubparser $ mconcat
  [ command "omegaLOL" (info omegaLOLP (progDesc ""))
  ]


data OmegaVars =
    VarsFile FilePath
  | VarsStdin
  | VarsArgs

omegaLOLStr :: String -> String
omegaLOLStr = renderLC . omegaLOL . words

omegaLOLP :: PIO ()
omegaLOLP = run <$> varsP <*> optional loopP <*> many (strArgument @String (metavar "VARS.."))
  where
    run varSrc mLoopN args = do
      let loop = maybe id (\n -> mconcat . replicate n) mLoopN
      vars <- loop <$> case varSrc of
               VarsFile fp -> words <$> readFile fp
               VarsStdin -> words <$> hGetContents stdin
               VarsArgs -> pure args
      
      putStrLn $ renderLC $ omegaLOL vars

    loopP = option auto (long "loop" <> metavar "NUM")

    varsP = asum
      [ VarsFile <$> strOption (long "file" <> metavar "FILEPATH")
      , flag' VarsStdin (long "stdin")
      ] <|> pure VarsArgs
