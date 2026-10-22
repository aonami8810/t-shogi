
module Main where
import ShogiStratifiedMidgame
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  let mvs = 
        [ "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b"
        , "8h2b+", "3b2b", "7i8h", "7a6b", "3i4h", "6b7b", "4h3g", "4a3b"
        , "6i7h", "5a4b", "5i6h", "6c6d", "3g4f", "7c7d", "4f3e", "3c3d"
        , "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "3d3e", "S*5h", "S*3a"
        , "P*3c", "3b3c", "B*1f", "2b3b", "1f3d", "3b2b", "N*2e", "3c2d"
        , "2e3c+", "2b1b"
        ]
      moves = map parseMoveUSI mvs
      st = foldl (\s m -> applyMove m s) initState moves
      bd = board st
      kSq = findKing Gote bd
  putStrLn $ "Gote King Sq: " ++ show kSq
  putStrLn $ "Pieces near king:"
  mapM_ print [ (sq, p) | (sq, p) <- Map.toList bd, fst sq <= 4, snd sq <= 4 ]
  putStrLn $ "Hands: " ++ show (hands st)
  let legalMvs = f5SyzygyFilter st (generateMoves st)
  mapM_ (putStrLn . showMove) (take 10 legalMvs)
