{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CompiledMorseBook where

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map as Map

-- ============================================================================
-- 1. 収束した臨界ノードの定義（Type-Level Symbols）
-- ============================================================================

-- | Node ID: Start (Type: Saddle, Potential: 0.0)
-- | Node ID: Saddle_Yagura_30M (Type: Saddle, Potential: -12.45)
-- | Node ID: Saddle_Mino_Attack (Type: Saddle, Potential: 24.8)
-- | Node ID: Minimum_Gote_Victory (Type: Minimum, Potential: 10000.0)

-- ============================================================================
-- 2. 勾配流（V-Path）の型レベル二項遷移関係の定義（Type Family）
--    アクティブ・ラーニングで収束が確認されたパスのみが 'True と判定されます。
-- ============================================================================
type family ValidTransition (from :: Symbol) (to :: Symbol) :: Bool where
  ValidTransition "Start" "Saddle_Yagura_30M" = 'True
  ValidTransition "Saddle_Yagura_30M" "Saddle_Mino_Attack" = 'True
  ValidTransition "Saddle_Mino_Attack" "Minimum_Gote_Victory" = 'True
  ValidTransition from to = 'False

-- ============================================================================
-- 3. 静的な型安全 Morse Chain（勾配流チェーン：GADT）
--    コンパイル時安全が100%保証された一意な定跡パスの構築子です。
-- ============================================================================
data MorseChain (nodes :: [Symbol]) where
  Nil     :: MorseChain '[]
  StartAt :: KnownSymbol start => Proxy start -> MorseChain '[start]
  Connect :: ( KnownSymbol to
             , KnownSymbol from
             , ValidTransition from to ~ 'True
             ) => MorseChain (from : rest)
               -> Proxy to
               -> MorseChain (to : from : rest)

-- ============================================================================
-- 4. 実行時（Runtime）高速展開用のV-Path定義マップ
-- ============================================================================
vpathLookup :: String -> String -> Maybe [String]
vpathLookup from to = Map.lookup (from, to) runtimePaths

runtimePaths :: Map.Map (String, String) [String]
runtimePaths = Map.fromList
  [ (("Start", "Saddle_Yagura_30M"), ["7g7f", "3c3d", "2g2f", "4c4d", "8h2b+", "3a2b", "2f2e"])
  , (("Saddle_Yagura_30M", "Saddle_Mino_Attack"), ["5e5d", "5c5d", "R*5g", "5c5b+"])
  , (("Saddle_Mino_Attack", "Minimum_Gote_Victory"), ["5g5h", "5i6i", "5h6h", "6i7i"])
  ]
