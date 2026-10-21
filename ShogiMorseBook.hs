{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module ShogiMorseBook
  ( -- * 1. .mbook データ構造
    MorseNodeID
  , MorseNodeType(..)
  , MorseNode(..)
  , VPath(..)
  , MorseBook(..)
    -- * 2. 型レベル定跡展開モジュール
  , MorseChain(..)
  , ValidTransition
  , NextNode
    -- * 3. シリアライズ・パーサー
  , serializeBook
  , deserializeBook
  ) where

import GHC.Generics (Generic)
import GHC.TypeLits
import Data.Proxy (Proxy(..))
import qualified Data.Map as Map
import Data.List (intercalate)
import Text.Read (readMaybe)

-- ============================================================================
-- 1. モース収縮定跡 (.mbook) のデータ構造定義
-- ============================================================================

type MorseNodeID = String

-- | 臨界局面（Critical Cell）の種類。
-- 離散モース理論における極大点（自玉危機）、サドル点（分岐勝負所）、極小点（詰み収縮）を表現。
data MorseNodeType = Maxima | Saddle | Minimum
  deriving (Eq, Show, Read, Generic)

-- | 臨界局面ノード。
-- 局面のボード構造は省略され、一意なIDとモースポテンシャル評価値、臨界タイプを持つ。
data MorseNode = MorseNode
  { nodeId    :: MorseNodeID
  , nodeType  :: MorseNodeType
  , potential :: Double
  } deriving (Eq, Show, Read, Generic)

-- | 臨界点同士を繋ぐ「必然手順の坂道（V-path）」
-- 始点から終点へ、一切の分岐なしでスライドする一本道の指し手のリスト。
data VPath = VPath
  { pathSource :: MorseNodeID  -- ^ 始点の臨界ノードID
  , pathTarget :: MorseNodeID  -- ^ 終点の臨界ノードID
  , moves      :: [String]       -- ^ USI指し手文字列の連続 (例: ["7g7f", "3g3f"])
  } deriving (Eq, Show, Read, Generic)

-- | モース収縮定跡 (.mbook)
-- 数十億の将棋盤面ツリーを、数千〜数万の臨界点 (nodes) と必然手順 (vpaths) に完全収縮した複体（Complex）。
data MorseBook = MorseBook
  { bookNodes :: Map.Map MorseNodeID MorseNode
  , bookPaths :: Map.Map MorseNodeID [VPath] -- ^ 各始点ノードから出ている V-path のリスト (サドル点なら複数分岐)
  } deriving (Eq, Show, Read, Generic)


-- ============================================================================
-- 2. 型レベル定跡展開モジュール (Type-Level Verification)
-- ============================================================================

-- | 定跡チェーンの型レベル表現。
-- コンパイル時に、ある臨界局面から別の臨界局面への遷移が、
-- 定跡に登録された正当な V-path（勾配流）に沿っているかを型レベルで保証する。
data MorseChain (nodes :: [Symbol]) where
  NilChain  :: MorseChain '[]
  Single    :: Proxy node -> MorseChain '[node]
  -- | 臨界ノード `from` から `to` へ、必然手順 `moves` を介した遷移を型レベルで結合
  Connect   :: (ValidTransition from to ~ 'True)
            => Proxy from 
            -> Proxy to 
            -> MorseChain (to ': rest) 
            -> MorseChain (from ': to ': rest)

-- | 型レベル遷移検証。
-- 定跡データベース上の隣接関係（勾配流の接続性）を型レベルの関連テーブルとして定義する。
type family ValidTransition (from :: Symbol) (to :: Symbol) :: Bool where
  -- 平手開始局面 (Start) から最初のサドル点 (Saddle1: 5四歩等での分岐) への遷移は有効
  ValidTransition "Start" "Saddle_美濃急戦" = 'True
  ValidTransition "Start" "Saddle_相振り飛車" = 'True
  
  -- サドル局面からそれぞれの局所極小（詰み）または次のサドル点への遷移
  ValidTransition "Saddle_美濃急戦" "Saddle_終盤戦A" = 'True
  ValidTransition "Saddle_美濃急戦" "Local_Min_詰み回避" = 'True
  ValidTransition "Saddle_相振り飛車" "Saddle_終盤戦B" = 'True
  
  -- 終盤のサドル点から、極小点（完全な詰み）への収縮
  ValidTransition "Saddle_終盤戦A" "Minimum_詰み確定" = 'True
  ValidTransition "Saddle_終盤戦B" "Minimum_詰み確定" = 'True
  
  -- それ以外の遷移はすべて無効（定跡外の非正規遷移 ＝ コンパイルエラー）
  ValidTransition a b = 'False

-- | 特定のノードから遷移可能な次のノードを型レベルで探索・解決するヘルパー
type family NextNode (current :: Symbol) :: Symbol where
  NextNode "Start"         = "Saddle_美濃急戦"
  NextNode "Saddle_美濃急戦" = "Saddle_終盤戦A"
  NextNode "Saddle_終盤戦A"  = "Minimum_詰み確定"
  NextNode a               = TypeError (Text "Error: Node " :<>: Text a :<>: Text " is a leaf node or cannot transition further in Morse Complex.")


-- ============================================================================
-- 3. 効率的なシリアライズ ＆ 高速パース (.mbook パーサー)
-- ============================================================================

-- | .mbook 定跡のテキストシリアライズ (高速に読み込めるように平滑な形式で保存)
serializeBook :: MorseBook -> String
serializeBook MorseBook {..} =
  let nodeLines = [ "NODE:" ++ nid ++ "," ++ show (nodeType n) ++ "," ++ show (potential n)
                  | (nid, n) <- Map.toList bookNodes ]
      pathLines = [ "PATH:" ++ src ++ "->" ++ tgt ++ ":" ++ intercalate ">" mvs
                  | (_, paths) <- Map.toList bookPaths
                  , VPath src tgt mvs <- paths ]
  in unlines (nodeLines ++ pathLines)

-- | .mbook 定跡の高速デシリアライズ（パーサー）
-- 1行ずつのパターンマッチと Map への蓄積により、巨大な定跡を多項式時間でメモリ展開します。
deserializeBook :: String -> Maybe MorseBook
deserializeBook str =
  let lns = lines str
      parsed = foldl parseLine (Just (Map.empty, Map.empty)) lns
  in case parsed of
    Just (nodes, paths) -> Just (MorseBook nodes paths)
    Nothing             -> Nothing
  where
    parseLine :: Maybe (Map.Map MorseNodeID MorseNode, Map.Map MorseNodeID [VPath]) 
              -> String 
              -> Maybe (Map.Map MorseNodeID MorseNode, Map.Map MorseNodeID [VPath])
    parseLine Nothing _ = Nothing
    parseLine (Just (nodes, paths)) line
      | "NODE:" `isPrefixOf` line =
          let content = drop 5 line
              (nid, rest) = break (== ',') content
          in case rest of
            (',':typeAndPot) ->
              let (tStr, ',':pStr) = break (== ',') typeAndPot
              in do
                nt <- readMaybe tStr
                pot <- readMaybe pStr
                let nNode = MorseNode nid nt pot
                return (Map.insert nid nNode nodes, paths)
            _ -> Nothing

      | "PATH:" `isPrefixOf` line =
          let content = drop 5 line
              (src, rest1) = break (== '-') content
              -- "->target:move1>move2"
              targetAndMoves = drop 2 rest1 -- "->" をスキップ
              (tgt, ':':mvsStr) = break (== ':') targetAndMoves
              mvs = splitOn '>' mvsStr
              vpath = VPath src tgt mvs
              updatedPaths = Map.insertWith (++) src [vpath] paths
          in return (nodes, updatedPaths)

      | otherwise = Just (nodes, paths) -- コメント行や空行はスキップ

    isPrefixOf :: String -> String -> Bool
    isPrefixOf pref str' = pref == take (length pref) str'

    splitOn :: Char -> String -> [String]
    splitOn _ "" = []
    splitOn c s =
      let (first, rest) = break (== c) s
      in first : case rest of
        []     -> []
        (_:r)  -> splitOn c r
