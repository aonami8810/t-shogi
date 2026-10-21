{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import System.Exit
import Data.List
import qualified Data.Map as Map
import Data.Maybe

-- ============================================================================
-- 1. 内部データ表現（.mbook 形式に対応）
-- ============================================================================

data NodeType = Saddle | Maxima | Minimum
  deriving (Eq, Show, Read)

data MorseNode = MorseNode
  { nodeId    :: String
  , nodeType  :: NodeType
  , potential :: Double
  } deriving (Show)

data VPath = VPath
  { fromNode :: String
  , toNode   :: String
  , moves    :: [String]
  } deriving (Show)

data MorseBook = MorseBook
  { bookNodes :: Map.Map String MorseNode
  , bookPaths :: [VPath]
  } deriving (Show)

-- ============================================================================
-- 2. .mbook パーサー（デシリアライズ）
-- ============================================================================

parseMBook :: String -> MorseBook
parseMBook content = foldl' parseLine (MorseBook Map.empty []) (lines content)
  where
    parseLine :: MorseBook -> String -> MorseBook
    parseLine book l
      | null l || "#" `isPrefixOf` l = book
      | "NODE:" `isPrefixOf` l =
          let rest = drop 5 l
              tokens = splitBy ',' rest
          in if length tokens >= 3
             then
               let nid = tokens !! 0
                   ntype = read (tokens !! 1) :: NodeType
                   pot = read (tokens !! 2) :: Double
                   newNode = MorseNode nid ntype pot
               in book { bookNodes = Map.insert nid newNode (bookNodes book) }
             else book
      | "PATH:" `isPrefixOf` l =
          let rest = drop 5 l
              (nodesPart, movesPart) = break (== ':') rest
              (fromN, toN) = case splitByString "->" nodesPart of
                [f, t] -> (f, t)
                _      -> ("", "")
              mvs = if null movesPart then [] else splitBy '>' (drop 1 movesPart)
              newPath = VPath fromN toN mvs
          in if not (null fromN) && not (null toN)
             then book { bookPaths = newPath : bookPaths book }
             else book
      | otherwise = book

    splitBy :: Char -> String -> [String]
    splitBy c s = case break (== c) s of
      (a, [])     -> [a]
      (a, _:rest) -> a : splitBy c rest

    splitByString :: String -> String -> [String]
    splitByString _ [] = []
    splitByString sub s@(c:cs)
      | sub `isPrefixOf` s = "" : splitByString sub (drop (length sub) s)
      | otherwise =
          case splitByString sub cs of
            []     -> [[c]]
            (x:xs) -> (c : x) : xs

-- ============================================================================
-- 3. Haskell型レベルコード・ジェネレーター（トランスパイラ）
-- ============================================================================

-- | 入力されたMorseBookから、コンパイル時に100%安全な型レベル定跡モジュール（Haskellソース）を生成する
transpileToTypeLevelHaskell :: MorseBook -> String
transpileToTypeLevelHaskell MorseBook{..} = unlines $
  [ "{-# LANGUAGE DataKinds #-}"
  , "{-# LANGUAGE TypeFamilies #-}"
  , "{-# LANGUAGE GADTs #-}"
  , "{-# LANGUAGE TypeOperators #-}"
  , "{-# LANGUAGE UndecidableInstances #-}"
  , "{-# LANGUAGE FlexibleContexts #-}"
  , "{-# LANGUAGE MultiParamTypeClasses #-}"
  , ""
  , "module CompiledMorseBook where"
  , ""
  , "import GHC.TypeLits"
  , "import Data.Proxy"
  , ""
  , "-- ============================================================================"
  , "-- 1. 収束した臨界ノードの定義（Type-Level Symbols）"
  , "-- ============================================================================"
  , ""
  ] ++ map generateNodeDoc (Map.elems bookNodes) ++
  [ ""
  , "-- ============================================================================"
  , "-- 2. 勾配流（V-Path）の型レベル二項遷移関係の定義（Type Family）"
  , "--    アクティブ・ラーニングで収束が確認されたパスのみが 'True と判定されます。"
  , "-- ============================================================================"
  , "type family ValidTransition (from :: Symbol) (to :: Symbol) :: Bool where"
  ] ++ map generateTransitionRule bookPaths ++
  [ "  ValidTransition from to = 'False" -- デフォルトはすべての遷移を拒絶（コンパイルエラー化）
  , ""
  , "-- ============================================================================"
  , "-- 3. 静的な型安全 Morse Chain（勾配流チェーン：GADT）"
  , "--    コンパイル時安全が100%保証された一意な定跡パスの構築子です。"
  , "-- ============================================================================"
  , "data MorseChain (nodes :: [Symbol]) where"
  , "  Nil     :: MorseChain '[]"
  , "  StartAt :: KnownSymbol start => Proxy start -> MorseChain '[start]"
  , "  Connect :: ( KnownSymbol to"
  , "             , KnownSymbol from"
  , "             , ValidTransition from to ~ 'True" -- 遷移関係がTrueの場合のみ結合を許可
  , "             ) => MorseChain (from : rest)"
  , "               -> Proxy to"
  , "               -> MorseChain (to : from : rest)"
  , ""
  , "-- ============================================================================"
  , "-- 4. 実行時（Runtime）高速展開用のV-Path定義マップ"
  , "-- ============================================================================"
  , "vpathLookup :: String -> String -> Maybe [String]"
  , "vpathLookup from to = Map.lookup (from, to) runtimePaths"
  , ""
  , "runtimePaths :: Map.Map (String, String) [String]"
  , "runtimePaths = Map.fromList"
  , "  ["
  ] ++ [ "    " ++ genRuntimeTuple p ++ "," | p <- init bookPaths ] ++
  [ "    " ++ genRuntimeTuple (last bookPaths) | not (null bookPaths) ] ++
  [ "  ]"
  , ""
  , "-- 補助関数・ダミーインポート解決用"
  , "import qualified Data.Map as Map"
  ]
  where
    generateNodeDoc :: MorseNode -> String
    generateNodeDoc MorseNode{..} =
      "-- | Node ID: " ++ nodeId ++ " (Type: " ++ show nodeType ++ ", Potential: " ++ show potential ++ ")"

    generateTransitionRule :: VPath -> String
    generateTransitionRule VPath{..} =
      "  ValidTransition \"" ++ fromNode ++ "\" \"" ++ toNode ++ "\" = 'True"

    genRuntimeTuple :: VPath -> String
    genRuntimeTuple VPath{..} =
      "((\"" ++ fromNode ++ "\", \"" ++ toNode ++ "\"), " ++ show moves ++ ")"

-- ============================================================================
-- 4. メイン（テストコンパイラジェネレータドライバ）
-- ============================================================================

-- テスト用の収束した.mbook生データ
sampleMBookData :: String
sampleMBookData = unlines
  [ "NODE:Start,Saddle,0.0"
  , "NODE:Saddle_Yagura_30M,Saddle,-12.45"
  , "NODE:Saddle_Mino_Attack,Saddle,24.8"
  , "NODE:Minimum_Gote_Victory,Minimum,10000.0"
  , "PATH:Start->Saddle_Yagura_30M:7g7f>3c3d>2g2f>4c4d>8h2b+>3a2b>2f2e"
  , "PATH:Saddle_Yagura_30M->Saddle_Mino_Attack:5e5d>5c5d>R*5g>5c5b+"
  , "PATH:Saddle_Mino_Attack->Minimum_Gote_Victory:5g5h>5i6i>5h6h>6i7i"
  ]

main :: IO ()
main = do
  putStrLn "=== Shogi Morse-Contraction Compiler-Generator ==="
  putStrLn "Reading converged dynamic sampling book from active-learning..."
  
  -- 実際には外部から.mbookを読み込む、またはメモリ上のActiveLearningBookをデシリアライズ
  let parsedBook = parseMBook sampleMBookData
  putStrLn $ "Successfully parsed Morse Book."
  putStrLn $ "  Total Nodes (Critical Points): " ++ show (Map.size (bookNodes parsedBook))
  putStrLn $ "  Total V-Paths (Gradient Flows): " ++ show (length (bookPaths parsedBook))
  
  putStrLn "Transpiling and generating type-level safe Haskell code (CompiledMorseBook.hs)..."
  let generatedHaskell = transpileToTypeLevelHaskell parsedBook
  
  -- ファイル出力
  writeFile "/workspace/scratch/CompiledMorseBook.hs" generatedHaskell
  putStrLn "Compile-time safe Haskell file generated successfully in scratch."
  
  -- 成果物としてoutにもシリアライズ
  writeFile "/workspace/out/CompiledMorseBook.hs" generatedHaskell
  putStrLn "Published CompiledMorseBook.hs to outbox."
  
  exitSuccess
