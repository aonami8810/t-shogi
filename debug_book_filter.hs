{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Map.Strict as Map
import Data.Char (isDigit, isUpper, toLower, toUpper, digitToInt)
import Data.List (isPrefixOf, isSuffixOf, sortBy)
import qualified CompiledMorseBook as Book

-- ShogiUsiMorse-v4.hs から必要な型と関数をインポートまたは定義
import ShogiUsiMorse (State(..), Move(..), Piece(..), PieceType(..), Player(..),
                      initState, applyMove, generateMoves, f5SyzygyFilter,
                      isPushingKakutoPawn, isFutileLanceLift,
                      isFutileInterpositionOnRookFile, isFutileIgnoredRookVanguard,
                      isIgnoringMajorCapturable, parseMove, showMove)

replayMoves :: [String] -> State
replayMoves moves = foldl (\st mStr -> applyMove (parseMove mStr) st) initState moves

testGame1 :: IO ()
testGame1 = do
  putStrLn "=== Testing Game 1 (Ply 10 after 8b8f) ==="
  let moves = ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f"]
      st = replayMoves moves
      allPaths = Map.elems Book.runtimePaths
      matchingPaths = filter (\p -> moves `isPrefixOf` p && length p > length moves) allPaths
      candidates = [ parseMove (p !! length moves) | p <- matchingPaths ]
      legalMoves = f5SyzygyFilter st (generateMoves st)
      
  putStrLn $ "Matching paths count: " ++ show (length matchingPaths)
  putStrLn $ "Candidates: " ++ show (map showMove candidates)
  putStrLn $ "Is 2e2d in legalMoves? " ++ show (parseMove "2e2d" `elem` legalMoves)
  putStrLn $ "isPushingKakutoPawn 2e2d? " ++ show (isPushingKakutoPawn st (parseMove "2e2d"))
  putStrLn $ "isFutileLanceLift 2e2d? " ++ show (isFutileLanceLift st (parseMove "2e2d"))
  putStrLn $ "isFutileInterpositionOnRookFile 2e2d? " ++ show (isFutileInterpositionOnRookFile st (parseMove "2e2d"))
  putStrLn $ "isFutileIgnoredRookVanguard 2e2d? " ++ show (isFutileIgnoredRookVanguard st (parseMove "2e2d"))
  putStrLn $ "isIgnoringMajorCapturable 2e2d? " ++ show (isIgnoringMajorCapturable st (parseMove "2e2d"))

testGame2 :: IO ()
testGame2 = do
  putStrLn "\n=== Testing Game 2 (Ply 23 after 7g3c+) ==="
  let moves = ["2g2f", "8c8d", "2f2e", "8d8e", "7g7f", "3c3d", "6i7h", "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b", "2d3d", "2b8h+", "7i8h", "2a3c", "B*7g", "8f7f", "8h8g", "7f2f", "7g3c+"]
      st = replayMoves moves
      allPaths = Map.elems Book.runtimePaths
      matchingPaths = filter (\p -> moves `isPrefixOf` p && length p > length moves) allPaths
      candidates = [ parseMove (p !! length moves) | p <- matchingPaths ]
      legalMoves = f5SyzygyFilter st (generateMoves st)
      
  putStrLn $ "Matching paths count: " ++ show (length matchingPaths)
  putStrLn $ "Candidates: " ++ show (map showMove candidates)
  putStrLn $ "Is 3b3c in legalMoves? " ++ show (parseMove "3b3c" `elem` legalMoves)
  putStrLn $ "isPushingKakutoPawn 3b3c? " ++ show (isPushingKakutoPawn st (parseMove "3b3c"))
  putStrLn $ "isFutileLanceLift 3b3c? " ++ show (isFutileLanceLift st (parseMove "3b3c"))
  putStrLn $ "isFutileInterpositionOnRookFile 3b3c? " ++ show (isFutileInterpositionOnRookFile st (parseMove "3b3c"))
  putStrLn $ "isFutileIgnoredRookVanguard 3b3c? " ++ show (isFutileIgnoredRookVanguard st (parseMove "3b3c"))
  putStrLn $ "isIgnoringMajorCapturable 3b3c? " ++ show (isIgnoringMajorCapturable st (parseMove "3b3c"))

main :: IO ()
main = do
  testGame1
  testGame2
