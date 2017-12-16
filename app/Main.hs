{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           WithCli

import           TeX

data CliArgs = CliArgs {
  moduleName :: String
, assignment :: String
, questions :: Int
, saveTo :: FilePath
} deriving (Show, Generic, HasArguments)

main :: IO ()
main = withCli run

run :: CliArgs -> IO ()
run CliArgs{..} = do
  student <- getStudentInfo
  let tex = mkTeX student moduleName assignment questions
  writeFile saveTo tex
