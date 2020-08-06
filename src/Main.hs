module Main where


import Options.Applicative

import Pinch.Generate

import System.FilePath
import Data.Text as T

data Options = Options
  { inputFile :: FilePath
  , outputDir :: FilePath
  , hashableVectorInstanceModule :: T.Text
  , generateArbitrary :: Bool
  }
  deriving (Show)


pOptions :: Parser Options
pOptions = Options
  <$> strOption (long "in" <> metavar "IN_FILE" <> help "Thrift input file")
  <*> strOption (long "out" <> metavar "OUT_DIR" <> help "Output folder")
  <*> strOption (long "hashable-vec-mod" <> help "Module containing hashable instances for vector")
  <*> flag True False (long "--no-generate-arbitrary")

main :: IO ()
main = do
  opts <- execParser pOpts

  generate (Settings (hashableVectorInstanceModule opts) (generateArbitrary opts)) (inputFile opts) (outputDir opts)

  where
    pOpts = info (pOptions <**> helper)
      ( fullDesc
      <> progDesc "Generate Haskell files from a thrift input file."
      <> header "Thrift Haskell Code Generator")
