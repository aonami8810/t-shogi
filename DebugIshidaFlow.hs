{-# LANGUAGE RecordWildCards #-}
module Main where

import System.IO
import qualified Data.Map as Map
import Data.List
import Data.Maybe

-- ShogiUsiMorse-v4.hs の関数をテストするためにロード
import qualified ShogiUsiMorse as M

main :: IO ()
main = do
  putStrLn "Debug Ishida Flow"
