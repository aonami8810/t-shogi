import subprocess

# Verify extended moves using ShogiStratifiedMidgame / generate-stratified-book
haskell_test = """
module Main where
import ShogiStratifiedMidgame
import Data.List (foldl')

testLine :: String -> [String] -> IO ()
testLine name mvs = do
  putStrLn $ "Testing line: " ++ name ++ " (length: " ++ show (length mvs) ++ ")"
  let parseM s = parseMoveUSI s
      moves = map parseM mvs
      states = scanl (\\s m -> applyMove m s) initState moves
  putStrLn $ "  Successfully validated " ++ show (length states) ++ " states!"
  let lastSt = last states
  putStrLn $ "  Final turn: " ++ show (turn lastSt)
  putStrLn $ "  Checkmate: " ++ show (isCheckmatePosition lastSt)
  let qres = matchQEGTB lastSt initSeedQEGTB
  putStrLn $ "  QEGTB Hit: " ++ show (fmap fst qres)

main :: IO ()
main = do
  -- Aigakari
  testLine "Aigakari"
    [ "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b"
    , "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "8e8f", "8g8f", "8b8f"
    , "P*8g", "8f8b", "3i3h", "7a7b", "5i6h", "5a4b", "4i5h", "4b3c"
    , "6h7h", "7b6c", "3h2g", "8b8d", "2h2f", "8d7d", "2f2e", "7d8d"
    , "2e2f", "8d7d", "8h3c+", "3b3c", "B*4e", "3c4b", "4e2c+", "4a3b"
    , "2c3b+", "4b3b", "G*4a"
    ]
  -- Kakugawari
  testLine "Kakugawari"
    [ "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b"
    , "8h2b+", "3b2b", "7i8h", "7a6b", "3i4h", "6b7b", "4h3g", "4a3b"
    , "6i7h", "5a4b", "5i6h", "6c6d", "3g4f", "7c7d", "4f3e", "3c3d"
    , "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "3d3e", "S*5h", "S*3a"
    , "P*3c", "3b3c", "B*1f", "2b3b", "1f3d", "3b2b", "N*2e", "3c2d"
    , "2e3c+", "2b1b", "G*2b"
    ]
  -- Yokofudori
  testLine "Yokofudori"
    [ "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b"
    , "2e2d", "2c2d", "2h2d", "8e8f", "8g8f", "8b8f", "2d3d", "8f8h+"
    , "7i8h", "B*4e", "3d3f", "4e2g+", "3f3d", "2g1g", "3d3f", "1g1f"
    , "3f3e", "1f2f", "B*3c", "2f2e", "3c5a+", "4a5a", "3f3a+", "5a4a"
    , "G*5a"
    ]
"""

with open("c:/VS/Workspace/t-shogi/TestDeepLines.hs", "w", encoding="utf-8") as f:
    f.write(haskell_test)
print("Created TestDeepLines.hs")
