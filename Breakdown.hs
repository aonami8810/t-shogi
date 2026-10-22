{-# LANGUAGE RecordWildCards #-}
module Main where

import System.IO
import qualified Data.Map as Map
import Data.List
import Data.Maybe

-- ShogiUsiMorse-v4 をインポートして関数を呼ぶ
-- 同じディレクトリにあるので main を除いたテスト用スクリプトとして直接ソースを include または import
import qualified ShogiUsiMorse as M

main :: IO ()
main = putStrLn "Breakdown"
