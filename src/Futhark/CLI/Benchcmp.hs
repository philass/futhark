{-# LANGUAGE OverloadedStrings #-}

-- | @futhark benchcmp@
module Futhark.CLI.Benchcmp (main) where

import qualified Data.ByteString.Lazy.Char8 as BS
import Futhark.Util.Options
import Futhark.Bench
import System.Directory

-- Things that must be done...
-- Read files (TOCTOU conscious)
--
-- Compare json Results

--- 
newtype DataSetCmp = DataSetCmp {res :: (String, Float)}

data FileComparison = FileComparison String [DataSetCmp]


compareBenchResults :: BenchResult -> [BenchResult] -> Maybe [String]
compareBenchResults br1@(BenchResult fp1 dr1) (br2@(BenchResult fp dr2):_) = compareBenchResult dr1 dr2
compareBenchResults br (_: brs) = compareBenchResults br brs
compareBenchResults br [] = Nothing


compareDataResults :: DataResult -> [DataResults] -> 

-- how this is going down... 
-- [BenchResults] [BenchResults]
-- BenchResult [BenchResults]
-- BenchResult BenchResult
-- DataResult DataResults
-- Data Result Data Result
-- 
--

--
--compareJson :: [BenchResult] -> [BenchResult] -> Maybe [[String]]
--compareJson 


-- Assume only one BenchResult

--BenchResult takes the same 
--
-- Benchresult = BenchResult FilePath [DataResult]
-- Benchresult = BenchResult FilePath [String (Either T.text
--
-- DataResult = String (Either T.Text ([RunResult], T.Text))


main :: String -> [String] -> IO ()
main = mainWithOptions () [] "<file> <file>" f
  where
    f [file_a, file_b] () = Just $ do
      -- Possibly change this (and handle this in the 
      aExists <- doesFileExist file_a 
      bExists <- doesFileExist file_b 
      case (aExists, bExists) of
        (False, _) ->
          error $ "Error couldn't find " ++ file_a
        (_, False) ->
          error $ "Error couldn't find " ++ file_b
        (_, _) -> error $ "Life"
          --aBenchResults <- decodeBenchResults <$> BS.readFile file_a
          --bBenchResults <- decodeBenchResults <$> BS.readFile file_b
          --case (aBenchResults, bBenchResults) of
          --  (Left err, _) -> 
          --    error $ "Couldn't parse json of " ++ file_a
          --  (_, Left err) -> 
          --    error $ "Couldn't parse json of " ++ file_b
          --   (Right aBR, Right bBR) -> compareJson aBR bBR
--
    f _ _ =
      Nothing

-- What am I doing??
--
--
