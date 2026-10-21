module Main where

import System.Process
import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
  putStrLn "Starting 30-move self-play simulation with ShogiUsiMorse.exe..."
  moves <- simulateMoves [] 30
  putStrLn "\n--- Completed 30-move Self-Play Game Record ---"
  mapM_ (\(i, m) -> putStrLn $ show i ++ ": " ++ m) (zip [1..] moves)
  putStrLn "\nFull moves string:"
  putStrLn $ unwords moves

simulateMoves :: [String] -> Int -> IO [String]
simulateMoves currentMoves 0 = return currentMoves
simulateMoves currentMoves stepsLeft = do
  let posCmd = if null currentMoves 
               then "position startpos\n"
               else "position startpos moves " ++ unwords currentMoves ++ "\n"
      inputData = "isready\n" ++ posCmd ++ "go\nquit\n"
  
  (Just hin, Just hout, _, ph) <- createProcess (proc "./ShogiUsiMorse.exe" [])
    { std_in = CreatePipe, std_out = CreatePipe }
  
  hPutStr hin inputData
  hFlush hin
  
  out <- hGetContents hout
  let bestMoveLine = find ("bestmove" `isPrefixOf`) (lines out)
  case bestMoveLine of
    Just line -> do
      let bm = words line !! 1
      if bm `elem` ["resign", "draw"]
        then return currentMoves
        else do
          let nextMoves = currentMoves ++ [bm]
          putStrLn $ "[" ++ show (length nextMoves) ++ "] " ++ bm
          simulateMoves nextMoves (stepsLeft - 1)
    Nothing -> return currentMoves
