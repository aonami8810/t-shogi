import qualified Data.Map as Map
import Data.List (isPrefixOf)
import qualified CompiledMorseBook as Book

main :: IO ()
main = do
  let m1 = ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f"]
  let allPaths = Map.elems Book.runtimePaths
  let match1 = filter (\p -> m1 `isPrefixOf` p && length p > length m1) allPaths
  putStrLn $ "m1 matches count: " ++ show (length match1)
  case match1 of
    (p:_) -> putStrLn $ "m1 next move: " ++ (p !! length m1)
    []    -> putStrLn "m1 no match!"

  let m2 = ["2g2f", "8c8d", "2f2e", "8d8e", "7g7f", "3c3d", "6i7h", "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b", "2d3d", "2b8h+", "7i8h", "2a3c", "B*7g", "8f7f", "8h8g", "7f2f", "7g3c+"]
  let match2 = filter (\p -> m2 `isPrefixOf` p && length p > length m2) allPaths
  putStrLn $ "m2 matches count: " ++ show (length match2)
  case match2 of
    (p:_) -> putStrLn $ "m2 next move: " ++ (p !! length m2)
    []    -> putStrLn "m2 no match!"
