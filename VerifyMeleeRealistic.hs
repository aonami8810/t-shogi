module Main where

import ShogiStratifiedMidgame
import Data.List (intercalate)
import System.IO

-- 正しい将棋の急戦・乱戦手順の構築

-- 1. 筋違い角 (Sujichigai Bishop)
-- 1. ▲7六歩 2. △3四歩 3. ▲2二角成 4. △同銀(3a2b) 5. ▲4五角(B*4e)
-- 6. △6二銀(7a6b) 7. ▲3四角(4e3d) 8. △8四歩(8c8d) 9. ▲2六歩(2g2f) 10. △8五歩(8d8e)
-- 11. ▲7七桂(8a7c or 2h7g?) -> ▲7七桂(2i7gは不可、8i7g)
-- 正確なUSI手順を1手ずつ確認

playMoves :: State -> [String] -> Either (Int, String, [Move]) State
playMoves st [] = Right st
playMoves st (mStr:ms) =
  let mv = parseMoveUSI mStr
      legal = generateMoves st
  in if mv `elem` legal
     then playMoves (applyMove mv st) ms
     else Left (length (moveHistory st) + 1, mStr, legal)

testLine :: String -> [String] -> IO ()
testLine name mvs = do
  putStrLn $ "Testing " ++ name ++ "..."
  case playMoves initState mvs of
    Right finalSt -> do
      putStrLn $ "  [SUCCESS] " ++ show (length mvs) ++ " plies 100% legal!"
    Left (ply, badMove, legal) -> do
      putStrLn $ "  [ERROR] Ply " ++ show ply ++ ": " ++ badMove ++ " is ILLEGAL!"
      putStrLn $ "  Sample legal moves: " ++ intercalate ", " (map showMove (take 10 legal))

main :: IO ()
main = do
  -- 1. 筋違い角
  testLine "Sujichigai Bishop"
    [ "7g7f", "3c3d", "8h2b+", "3a2b", "B*4e", "7a6b", "4e3d", "8c8d"
    , "2g2f", "8d8e", "2f2e", "4a3b", "3d2e", "8e8f", "8g8f", "8b8f"
    , "P*8g", "8f8b", "6i7h", "5a4b", "5i6h", "6b7c", "4i5h", "7c8d"
    , "3i4h", "8d7d", "2e3f", "7d7e", "7f7e", "B*5e", "3f2e", "5e2h+"
    , "2e2h"
    ]

  -- 2. 原始中飛車中央急襲 (Aggressive Central Rook)
  -- 1. ▲7六歩 2. △3四歩 3. ▲5六歩(5g5f) 4. △8四歩(8c8d) 5. ▲5八飛(2h5h)
  -- 6. △8五歩(8d8e) 7. ▲5五歩(5f5e) 8. △6二銀(7a6b) 9. ▲5四歩(5e5d) 10. △5四歩(5c5d)
  -- 11. ▲同飛(5h5d) 12. △5三歩(P*5c) 13. ▲5八飛(5d5h)
  testLine "Aggressive Central Rook"
    [ "7g7f", "3c3d", "5g5f", "8c8d", "2h5h", "8d8e", "5f5e", "7a6b"
    , "5e5d", "5c5d", "5h5d", "P*5c", "5d5h", "4a3b", "7i6h", "5a4b"
    , "4i5h", "6b5c", "3i4h", "3a2b", "5i4h", "4b3a", "4h3h", "5c6d"
    , "3h2h", "6d7e", "7f7e", "B*5e", "5h5e", "3d5e", "B*7a"
    ]

  -- 3. 角換わり早繰り銀 (Early Climbing Silver)
  -- 1. ▲7六歩 2. △3四歩 3. ▲2六歩 4. △8四歩 5. ▲2五歩(2f2e)
  -- 6. △8五歩 7. ▲7八金(6i7h) 8. △3二金(4a3b) 9. ▲2四歩(2e2d) 10. △2四歩(2c2d)
  -- 11. ▲同飛(2h2d) 12. △2三歩(P*2c) 13. ▲2八飛(2d2h) 14. △8六歩(8e8f) 15. ▲8六歩(8g8f)
  -- 16. △8六飛(8b8f) 17. ▲8七歩(P*8g) 18. △8二飛(8f8b) 19. ▲3八銀(3i3h) 20. △7二銀(7a7b)
  -- 21. ▲3七銀(3h3g) 22. △6四歩(6c6d) 23. ▲4六銀(3g4f) 24. △6三銀(7b6c) 25. ▲3五歩(3g3f or 3g4f->3f3e)
  testLine "Early Climbing Silver"
    [ "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "6i7h", "4a3b"
    , "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "8e8f", "8g8f", "8b8f"
    , "P*8g", "8f8b", "3i3h", "7a7b", "3h3g", "6c6d", "3g4f", "7b6c"
    , "3g3f", "5a4b", "4f3e", "3d3e", "3f3e", "P*3d", "B*1f"
    ]

  -- 4. 早仕掛け3筋強襲 (Early 3e Blitz)
  testLine "Early 3e Blitz"
    [ "7g7f", "3c3d", "2g2f", "8c8d", "3g3f", "8d8e", "3f3e", "3d3e"
    , "2f2e", "2b3c", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "3c4d"
    , "8h7g", "4a3b", "6i7h", "5a4b", "5i6h", "6a5b", "7i6h", "7a6b"
    , "4i5h", "5c5d", "3i4h", "4c4d", "4h3g", "3b4c", "P*3d", "4d3d"
    , "B*1f"
    ]

  -- 5. 右四間飛車急戦 (Migi-Shiken Blitz)
  testLine "Migi-Shiken Blitz"
    [ "7g7f", "3c3d", "2g2f", "4c4d", "2h4h", "8c8d", "4h4f", "4a3b"
    , "3i3h", "5a4b", "5i4h", "4b3a", "4h3g", "3a2b", "3h2h", "7a6b"
    , "4f4e", "4d4e", "3g4e", "6b7b", "4i4h", "8d8e", "4h4g", "8e8f"
    , "8g8f", "8b8f", "P*8g", "8f8b", "4e3c+", "2b3c", "R*4a", "3c2b"
    , "4a4b+"
    ]

  -- 6. 袖飛車3筋急襲 (Sodebisha Blitz)
  testLine "Sodebisha Blitz"
    [ "7g7f", "3c3d", "2g2f", "8c8d", "2h3h", "8d8e", "3h3f", "4a3b"
    , "3g3f", "5a4b", "3f3e", "3d3e", "3f3e", "8d8e", "3e2d", "2c2d"
    , "2f2e", "P*2c", "2e2d", "3b2d", "2h2d", "P*2b", "B*1f", "4b3b"
    , "1f3d"
    ]

  -- 7. 嬉野流 (Ureshino)
  testLine "Ureshino"
    [ "6i7h", "3c3d", "7g7f", "8c8d", "7i6h", "8d8e", "6h7g", "4a3b"
    , "2g2f", "5a4b", "3i4h", "3a2b", "4h3g", "7a6b", "3g4f", "6b7b"
    , "4f3e", "3b3c", "3e2d", "2c2d", "2f2e", "P*2c", "2e2d", "3c2d"
    , "2h2d", "P*2b", "2d2h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b"
    , "B*4e"
    ]

  -- 8. 早石田 (Early Ishida)
  testLine "Early Ishida"
    [ "7g7f", "3c3d", "7i7h", "8c8d", "7h7f", "8d8e", "7f7e", "4a3b"
    , "7g7f", "5a4b", "2h7h", "3a2b", "7e7d", "7c7d", "7h7d", "P*7c"
    , "7d7f", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "B*5e", "4b3a"
    , "5e2b+"
    ]
