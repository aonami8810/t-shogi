{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.List
import qualified Data.Map as Map

-- 直接ソースコードの関数をテストするためにロード
import qualified ShogiUsiMorse as M

main :: IO ()
main = do
  let moves = words "7g7f 3c3d 6g6f 8c8d 2h5h 8d8e 7i6h 8e8f"
  putStrLn "TestMoves starting..."
