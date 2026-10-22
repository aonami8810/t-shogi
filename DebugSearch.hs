import System.IO
import qualified Data.Map.Strict as Map
import Data.List (foldl')
import ShogiUsiMorse (initState, parseMove, applyMove, searchAlphaBeta, loadMidgameWeights, showMove, State(..))

main :: IO ()
main = do
  w <- loadMidgameWeights
  let moves = words "7g7f 3c3d 8h2b+ 3a2b 2g2f 8c8d 2f2e 8d8e 7i7h 2b3c 3i3h 4a3b 5i4h 5a4b 4h3i 6a5b 3i2h 7a6b 1g1f 1c1d"
      st = foldl' (\s mStr -> applyMove (parseMove mStr) s) initState moves
      (score, pv) = searchAlphaBeta w 1 (-99999.0) 99999.0 st
  putStrLn $ "Score: " ++ show score
  putStrLn $ "PV: " ++ unwords (map showMove pv)
