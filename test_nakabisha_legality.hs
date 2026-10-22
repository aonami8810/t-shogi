module Main where

import ShogiStratifiedMidgame

testSequence :: String -> [String] -> IO ()
testSequence name mvs = do
  putStrLn $ "Testing " ++ name ++ " (" ++ show (length mvs) ++ " plies)..."
  let step (st, idx) mStr =
        let mv = parseMoveUSI mStr
            legal = generateMoves st
        in if mv `elem` legal
           then (applyMove mv st, idx + 1)
           else error $ "Illegal move at ply " ++ show (idx + 1) ++ ": " ++ mStr
  let (finalSt, count) = foldl step (initState, 0) mvs
  putStrLn $ "  [SUCCESS] All " ++ show count ++ " plies 100% legal!"

main :: IO ()
main = do
  -- 1. 先手中飛車・美濃囲い鉄壁コビン防護本筋
  testSequence "Sente Nakabisha Kobin-Shielded Line"
    [ "7g7f", "3c3d", "5g5f", "8c8d", "2h5h", "8d8e", "5f5e", "4a3b"
    , "3i3h", "5a4b", "5i4h", "7a6b", "4h3h", "3a2b", "4i5h", "6b5c"
    , "7i6h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "5e5d", "5c5d"
    , "5h5d", "P*5c", "5d5h", "6c6d", "3h2h", "7c7d", "2h1h", "2b3c"
    , "1g1f", "1c1d", "6h5g", "3c4d", "5g4f", "4d5e", "4f5e", "5c5d"
    , "B*7a"
    ]

  -- 2. 後手ゴキゲン中飛車・超速対抗片美濃本筋
  testSequence "Gote Gokigen Nakabisha Iron-Defense Line"
    [ "7g7f", "3c3d", "2g2f", "5c5d", "2f2e", "8b5b", "4i3h", "6b5c"
    , "3h3g", "4a3b", "3g4f", "5c4d", "2e2d", "2c2d", "2h2d", "P*2c"
    , "2d2h", "5a6b", "6i7h", "6b7b", "3i4h", "7a6b", "4h3g", "8c8d"
    , "3g2f", "8d8e", "8h7g", "4d3e", "2f3e", "3d3e", "P*3d", "B*1f"
    ]
