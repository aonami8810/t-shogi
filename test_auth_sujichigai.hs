module Main where

import ShogiStratifiedMidgame

testMoveSeq :: [String] -> IO ()
testMoveSeq mvs = do
  let step (st, idx) mStr =
        let mv = parseMoveUSI mStr
            legal = generateMoves st
        in if mv `elem` legal
           then (applyMove mv st, idx + 1)
           else error $ "Illegal move at ply " ++ show (idx + 1) ++ ": " ++ mStr
  let (finalSt, count) = foldl step (initState, 0) mvs
  putStrLn $ "Success! " ++ show count ++ " plies 100% legal!"

main :: IO ()
main = do
  testMoveSeq
    [ "7g7f", "3c3d", "8h2b+", "3a2b", "B*4e", "7a6b", "4e3d", "8c8d"
    , "2g2f", "8d8e", "6i7h", "4a3b", "3d5f", "8e8f", "8g8f", "8b8f"
    , "P*8g", "8f8b", "5i6h", "5a4b", "4i5h", "6b7c", "3i4h", "7c8d"
    , "5f3d", "2c2d", "2f2e", "8d7d", "2e2d", "7d7e", "7f7e", "B*5e"
    , "3d2e", "5e2h+", "2e2h"
    ]
