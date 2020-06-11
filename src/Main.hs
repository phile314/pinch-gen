module Main where


import Options.Applicative

import Pinch.Generate

import System.FilePath

data Options = Options
  { inputFile :: FilePath
  , outputDir :: FilePath
  }
  deriving (Show)


pOptions :: Parser Options
pOptions = Options
  <$> strOption (long "in" <> metavar "IN_FILE" <> help "Thrift input file")
  <*> strOption (long "out" <> metavar "OUT_DIR" <> help "Output folder")

main :: IO ()
main = do
  opts <- execParser pOpts

  generate (inputFile opts) (outputDir opts)

  where
    pOpts = info (pOptions <**> helper)
      ( fullDesc
      <> progDesc "Generate Haskell files from a thrift input file."
      <> header "Thrift Haskell Code Generator")
