-- Quick script to verify legal move sequences for Sente and Gote Nakabisha lines
module Main where

import ShogiStratifiedMidgame
import Data.List (foldl')
import System.Exit (exitFailure, exitSuccess)

testLine :: String -> [String] -> IO State
testLine name moveStrs = do
  putStrLn $ "=== Testing " ++ name ++ " (" ++ show (length moveStrs) ++ " plies) ==="
  let moves = map parseMoveUSI moveStrs
      step (st, idx) m =
        let legal = generateMoves st
        in if m `elem` legal
           then (applyMove m st, idx + 1)
           else
             error $ "ILLEGAL MOVE at ply " ++ show idx ++ " (" ++ show (turn st) ++ "): " ++ show m
                  ++ "\nLegal moves were: " ++ show (take 15 legal)
      (finalSt, total) = foldl' step (initState, 1) moves
  putStrLn $ "SUCCESS! " ++ name ++ " is 100% legal up to ply " ++ show (total - 1)
  return finalSt

main :: IO ()
main = do
  -- 1. 先手中飛車・美濃囲い・中央進出手順 (47 plies)
  let senteNakabishaMoves =
        [ "7g7f", "3c3d", "5g5f", "8c8d", "2h5h", "8d8e", "7i7h", "4a3b"
        , "5f5e", "5a4b", "3i3h", "7a6b", "5i4h", "6b6c", "4h3i", "3a4a"
        , "3i2h", "4b3a", "4i3i", "5c5d", "5e5d", "6c5d", "P*5e", "5d6c"
        , "6i5h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "5h5g", "8a7c"
        , "5g4f", "1c1d", "1g1f", "7c6e", "7h6g", "6e7g+", "6g7g", "6c5d"
        , "5e5d", "B*4e", "4f3e", "4e2g+", "2h1h", "2g3h", "3i3h"
        ]
  _ <- testLine "SenteNakabisha_IronMino" senteNakabishaMoves

  -- 2. 後手ゴキゲン中飛車 vs 超速3七銀手順 (36 plies)
  let goteGokigenMoves =
        [ "7g7f", "3c3d", "2g2f", "5c5d", "2f2e", "8b5b", "4i3h", "6b5c"
        , "3h3g", "4a3b", "3g4f", "5c4d", "2e2d", "2c2d", "2h2d", "P*2c"
        , "2d2h", "5a6b", "6i7h", "6b7b", "5i6h", "7b8b", "3i4h", "6a7b"
        , "7i6h", "4d5e", "4f5e", "5b5e", "P*5f", "5e5a", "8h2b+", "3a2b"
        , "B*3d", "B*4d", "3d4c+", "3b4c"
        ]
  _ <- testLine "GoteGokigen_IronDefense" goteGokigenMoves
  putStrLn "All test lines validated successfully!"
