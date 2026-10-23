import qualified Data.Map as Map
import Data.List (isPrefixOf)
import qualified CompiledMorseBook as Book

main :: IO ()
main = do
  putStrLn "=== Game 1 Search ==="
  let g1 = ["7g7f", "8c8d", "2g2f", "4a3b", "6i7h", "8d8e"]
  let allPaths = Map.elems Book.runtimePaths
  let m1 = filter (\p -> g1 `isPrefixOf` p && length p > length g1) allPaths
  putStrLn $ "g1 matches: " ++ show (length m1)
  case m1 of
    (p:_) -> putStrLn $ "g1 next move: " ++ (p !! length g1)
    []    -> putStrLn "g1 no match!"

  putStrLn "\n=== Game 2 Search ==="
  let g2 = ["2g2f", "8c8d", "2f2e", "8d8e", "7g7f", "3c3d", "5i5h", "8e8f", "8g8f", "5a5b", "8h2b+"]
  let m2 = filter (\p -> g2 `isPrefixOf` p && length p > length g2) allPaths
  putStrLn $ "g2 matches: " ++ show (length m2)
  case m2 of
    (p:_) -> putStrLn $ "g2 next move: " ++ (p !! length g2)
    []    -> putStrLn "g2 no match!"
