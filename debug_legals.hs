module Main where

import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import ShogiStratifiedMidgame

main :: IO ()
main = do
  putStrLn "--- Game 1 Legals ---"
  let moveStrs1 = words "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 2b8h+"
  let moves1 = map parseMoveUSI moveStrs1
  let st1 = foldl (flip applyMove) initState moves1
  putStrLn $ "Turn: " ++ show (turn st1)
  putStrLn $ "Board (8,8): " ++ show (Map.lookup (8,8) (board st1))
  putStrLn $ "Board (7,8): " ++ show (Map.lookup (7,8) (board st1))
  putStrLn $ "Board (7,9): " ++ show (Map.lookup (7,9) (board st1))
  let rawLegals = generateMoves st1
  let f5Legals = f5SyzygyFilter st1 rawLegals
  putStrLn $ "Raw legals: " ++ show (length rawLegals)
  putStrLn $ "F5 legals: " ++ show (length f5Legals)
  let captures88 = [ m | m <- f5Legals, case m of NormalMove _ (8,8) _ -> True ; _ -> False ]
  putStrLn $ "Moves capturing (8,8): " ++ show (map showMove captures88)

  putStrLn "\n--- Game 2 Legals ---"
  let moveStrs2 = words "2g2f 5c5d 2f2e 8b5b 2e2d 2c2d 2h2d P*2c 2d2c+"
  let moves2 = map parseMoveUSI moveStrs2
  let st2 = foldl (flip applyMove) initState moves2
  putStrLn $ "Turn: " ++ show (turn st2)
  putStrLn $ "Board (2,3): " ++ show (Map.lookup (2,3) (board st2))
  let rawLegals2 = generateMoves st2
  let f5Legals2 = f5SyzygyFilter st2 rawLegals2
  putStrLn $ "Raw legals: " ++ show (length rawLegals2)
  putStrLn $ "F5 legals: " ++ show (length f5Legals2)
  let captures23 = [ m | m <- f5Legals2, case m of NormalMove _ (2,3) _ -> True ; _ -> False ]
  putStrLn $ "Moves capturing (2,3): " ++ show (map showMove captures23)
