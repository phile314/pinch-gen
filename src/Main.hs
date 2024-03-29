{-# LANGUAGE OverloadedStrings #-}

module Main where


import Options.Applicative

import Pinch.Generate

data Options = Options
  { inputFile   :: FilePath
  , outputDir   :: FilePath
  , genSettings :: Settings
  }
  deriving (Show)


pOptions :: Parser Options
pOptions = Options
  <$> strOption (long "in" <> metavar "IN_FILE" <> help "Thrift input file")
  <*> strOption (long "out" <> metavar "OUT_DIR" <> help "Output folder")
  <*> pGenSettings

pGenSettings :: Parser Settings
pGenSettings = Settings
  <$> strOption (long "hashable-vec-mod" <> help "Module containing hashable instances for vector")
  <*> flag True False (long "no-generate-arbitrary")
  <*> flag True False (long "no-generate-nfdata")
  <*> many (strOption (long "extra-import" <> metavar "IMPORT"))
  <*> strOption
        ( long "module-prefix"
        <> help "Prefix of module name for generated files, e.g. 'Gen.Agent.'"
        <> value ""
        )

main :: IO ()
main = do
  opts <- execParser pOpts

  generate (genSettings opts) (inputFile opts) (outputDir opts)

  where
    pOpts = info (pOptions <**> helper)
      ( fullDesc
      <> progDesc "Generate Haskell files from a thrift input file."
      <> header "Thrift Haskell Code Generator")
