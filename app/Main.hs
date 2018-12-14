{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Numeric.Natural (Natural)
import           WithCli (Generic, HasArguments, withCli)

import           TeX (getStudentInfo, mkTeX)

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
  let tex = mkTeX student moduleName assignment (fromIntegral questions)
  writeFile saveTo tex
