import qualified Data.Map.Strict as Map
import Data.List
import System.IO

import CompiledMorseBook as Book

-- ShogiUsiMorse-v4.hs のデータ構造と関数をシンプルにテスト
main :: IO ()
main = do
  let currentMoves = ["2g2f", "8c8d", "7g7f", "3c3d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d"]
      allPaths = Map.elems Book.runtimePaths
      matchingPaths = filter (\p -> currentMoves `isPrefixOf` p && length p > length currentMoves) allPaths
  putStrLn $ "Matching paths count: " ++ show (length matchingPaths)
  let candidates = [ p !! length currentMoves | p <- matchingPaths ]
  putStrLn $ "Candidates (raw): " ++ show (take 10 candidates)
