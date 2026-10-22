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
  -- 1. 先手中飛車・美濃囲い鉄壁コビン防護本筋 (41手)
  -- 1. ▲7六歩 2. △3四歩 3. ▲5六歩 4. △8四歩 5. ▲5八飛
  -- 6. △8五歩 7. ▲5五歩 8. △6二銀 9. ▲4八玉(5i4h) 10. △4二玉(5a4b)
  -- 11. ▲3八銀(3i3h) 12. △3二金(4a3b) 13. ▲3九玉(4h3i) 14. △5三銀(6b5c)
  -- 15. ▲2八玉(3i2h) 16. △8六歩 17. ▲同歩 18. △同飛 19. ▲8七歩 20. △8二飛
  -- 21. ▲5八金右(4i5h) 22. △3三銀(3a3b/2b?) 23. ▲6八銀(7i6h)
  testSequence "Sente Nakabisha Mino Line"
    [ "7g7f", "3c3d", "5g5f", "8c8d", "2h5h", "8d8e", "5f5e", "7a6b"
    , "5i4h", "5a4b", "3i3h", "4a3b", "4h3i", "6b6c", "3i2h", "8e8f"
    , "8g8f", "8b8f", "P*8g", "8f8b", "4i5h", "3a2b", "7i6h", "2b3c"
    , "5e5d", "5c5d", "5h5d", "P*5c", "5d5h", "6c6d", "1g1f", "1c1d"
    , "6h5g", "3c4d", "5g4f", "4d5e", "4f5e", "5c5d", "B*7a", "4b3a"
    , "7a5c+"
    ]

  -- 2. 後手ゴキゲン中飛車・美濃囲い鉄壁コビン防護本筋 (35手)
  testSequence "Gote Gokigen Nakabisha Iron-Defense Line"
    [ "7g7f", "3c3d", "2g2f", "5c5d", "2f2e", "8b5b", "4i3h", "6b5c"
    , "3h3g", "4a3b", "3g4f", "5c4d", "2e2d", "2c2d", "2h2d", "P*2c"
    , "2d2h", "5a6b", "6i7h", "6b7b", "3i4h", "7a6b", "4h3g", "8c8d"
    , "3g2f", "8d8e", "8h7g", "4d3e", "2f3e", "3d3e", "P*3d", "B*1f"
    , "4b3c", "1f3d", "3c4b"
    ]
