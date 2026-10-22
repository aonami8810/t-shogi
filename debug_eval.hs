module Main where

import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import ShogiStratifiedMidgame

main :: IO ()
main = do
  let moves = words "7g7f 3c3d 8h2b+ 3a2b 5g5f 8c8d 2h5h 8d8e 7i7h 8e8f 8g8f 8b8f"
  let st = foldl (flip applyMoveString) initState moves
  putStrLn $ "Turn: " ++ show (turn st)
  putStrLn $ "Hands: " ++ show (hands st)
  let legals = generateMoves st
  putStrLn $ "Total legal moves: " ++ show (length legals)
  let hasP8g = parseMove "P*8g" `elem` legals
  let hasB8g = parseMove "B*8g" `elem` legals
  putStrLn $ "Has P*8g: " ++ show hasP8g
  putStrLn $ "Has B*8g: " ++ show hasB8g
