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

-- | Node ID: Minimum_Ply_512_GuardedOpening_Convergence (Type: Saddle, Potential: 7356.686382985772)
-- | Node ID: Saddle_Ply_100_3e2d (Type: Saddle, Potential: -5190.894814764079)
-- | Node ID: Saddle_Ply_120_1c1b (Type: Saddle, Potential: -5231.617369597097)
-- | Node ID: Saddle_Ply_136_1a1b (Type: Saddle, Potential: -4008.0489337898007)
-- | Node ID: Saddle_Ply_140_2d3c (Type: Saddle, Potential: -4802.336635994236)
-- | Node ID: Saddle_Ply_160_1b1a (Type: Saddle, Potential: -5927.449834177516)
-- | Node ID: Saddle_Ply_180_1b1a (Type: Saddle, Potential: -5569.364750943918)
-- | Node ID: Saddle_Ply_200_6a5b (Type: Saddle, Potential: -6895.81048795415)
-- | Node ID: Saddle_Ply_20_4c4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Saddle_Ply_220_5b4b (Type: Saddle, Potential: -8796.53378827958)
-- | Node ID: Saddle_Ply_240_1a2a (Type: Saddle, Potential: -11413.315762178048)
-- | Node ID: Saddle_Ply_260_1d1c (Type: Saddle, Potential: -11398.881465046135)
-- | Node ID: Saddle_Ply_280_2c1d (Type: Saddle, Potential: -11908.252259886769)
-- | Node ID: Saddle_Ply_288_2c2d (Type: Saddle, Potential: -11118.754783906183)
-- | Node ID: Saddle_Ply_296_2e3e (Type: Saddle, Potential: -11249.860574937464)
-- | Node ID: Saddle_Ply_300_2d2c (Type: Saddle, Potential: -11110.31209129269)
-- | Node ID: Saddle_Ply_304_2d2c (Type: Saddle, Potential: -11110.31209129269)
-- | Node ID: Saddle_Ply_320_1c1b (Type: Saddle, Potential: -13715.903831567626)
-- | Node ID: Saddle_Ply_340_1b1a (Type: Saddle, Potential: -15007.814259220162)
-- | Node ID: Saddle_Ply_360_1b1a (Type: Saddle, Potential: -14996.3368965408)
-- | Node ID: Saddle_Ply_380_1b1a (Type: Saddle, Potential: -14999.397233873438)
-- | Node ID: Saddle_Ply_400_1b1a (Type: Saddle, Potential: -15663.460960495464)
-- | Node ID: Saddle_Ply_40_4a3b (Type: Saddle, Potential: 0.0)
-- | Node ID: Saddle_Ply_420_1b1a (Type: Saddle, Potential: -15669.11830540064)
-- | Node ID: Saddle_Ply_432_1b1a (Type: Saddle, Potential: -14670.35335439683)
-- | Node ID: Saddle_Ply_440_1b1a (Type: Saddle, Potential: -14674.417081018853)
-- | Node ID: Saddle_Ply_448_1b1a (Type: Saddle, Potential: -14667.293017064188)
-- | Node ID: Saddle_Ply_460_1b1a (Type: Saddle, Potential: -14680.074425924031)
-- | Node ID: Saddle_Ply_472_1a1b (Type: Saddle, Potential: -15357.36927598279)
-- | Node ID: Saddle_Ply_480_1a1b (Type: Saddle, Potential: -15363.026620887964)
-- | Node ID: Saddle_Ply_488_1a1b (Type: Saddle, Potential: -15353.30554936076)
-- | Node ID: Saddle_Ply_48_4e4f (Type: Saddle, Potential: -2301.0111931692754)
-- | Node ID: Saddle_Ply_496_1a1b (Type: Saddle, Potential: -15371.44364623469)
-- | Node ID: Saddle_Ply_500_1a1b (Type: Saddle, Potential: -15371.44364623469)
-- | Node ID: Saddle_Ply_504_1b1a (Type: Saddle, Potential: -15334.710042568699)
-- | Node ID: Saddle_Ply_512_1b1a (Type: Saddle, Potential: -15334.710042568699)
-- | Node ID: Saddle_Ply_56_1a1b (Type: Saddle, Potential: -3989.545680197227)
-- | Node ID: Saddle_Ply_60_1e4b (Type: Saddle, Potential: -3979.360596600378)
-- | Node ID: Saddle_Ply_80_4b3c (Type: Saddle, Potential: -3238.9856803490134)
-- | Node ID: Saddle_Ply_88_4c3c (Type: Saddle, Potential: -4084.589875318836)
-- | Node ID: Start (Type: Saddle, Potential: 0.0)

-- ============================================================================
-- 2. 勾配流（V-Path）の型レベル二項遷移関係の定義（Type Family）
--    アクティブ・ラーニングで収束が確認されたパスのみが 'True と判定されます。
-- ============================================================================
type family ValidTransition (from :: Symbol) (to :: Symbol) :: Bool where
  ValidTransition "Saddle_Ply_504_1b1a" "Saddle_Ply_512_1b1a" = 'True
  ValidTransition "Saddle_Ply_500_1a1b" "Saddle_Ply_504_1b1a" = 'True
  ValidTransition "Saddle_Ply_496_1a1b" "Saddle_Ply_500_1a1b" = 'True
  ValidTransition "Saddle_Ply_488_1a1b" "Saddle_Ply_496_1a1b" = 'True
  ValidTransition "Saddle_Ply_480_1a1b" "Saddle_Ply_488_1a1b" = 'True
  ValidTransition "Saddle_Ply_472_1a1b" "Saddle_Ply_480_1a1b" = 'True
  ValidTransition "Saddle_Ply_460_1b1a" "Saddle_Ply_472_1a1b" = 'True
  ValidTransition "Saddle_Ply_448_1b1a" "Saddle_Ply_460_1b1a" = 'True
  ValidTransition "Saddle_Ply_440_1b1a" "Saddle_Ply_448_1b1a" = 'True
  ValidTransition "Saddle_Ply_432_1b1a" "Saddle_Ply_440_1b1a" = 'True
  ValidTransition "Saddle_Ply_420_1b1a" "Saddle_Ply_432_1b1a" = 'True
  ValidTransition "Saddle_Ply_400_1b1a" "Saddle_Ply_420_1b1a" = 'True
  ValidTransition "Saddle_Ply_380_1b1a" "Saddle_Ply_400_1b1a" = 'True
  ValidTransition "Saddle_Ply_360_1b1a" "Saddle_Ply_380_1b1a" = 'True
  ValidTransition "Saddle_Ply_340_1b1a" "Saddle_Ply_360_1b1a" = 'True
  ValidTransition "Saddle_Ply_320_1c1b" "Saddle_Ply_340_1b1a" = 'True
  ValidTransition "Saddle_Ply_304_2d2c" "Saddle_Ply_320_1c1b" = 'True
  ValidTransition "Saddle_Ply_300_2d2c" "Saddle_Ply_304_2d2c" = 'True
  ValidTransition "Saddle_Ply_296_2e3e" "Saddle_Ply_300_2d2c" = 'True
  ValidTransition "Saddle_Ply_288_2c2d" "Saddle_Ply_296_2e3e" = 'True
  ValidTransition "Saddle_Ply_280_2c1d" "Saddle_Ply_288_2c2d" = 'True
  ValidTransition "Saddle_Ply_260_1d1c" "Saddle_Ply_280_2c1d" = 'True
  ValidTransition "Saddle_Ply_240_1a2a" "Saddle_Ply_260_1d1c" = 'True
  ValidTransition "Saddle_Ply_220_5b4b" "Saddle_Ply_240_1a2a" = 'True
  ValidTransition "Saddle_Ply_200_6a5b" "Saddle_Ply_220_5b4b" = 'True
  ValidTransition "Saddle_Ply_180_1b1a" "Saddle_Ply_200_6a5b" = 'True
  ValidTransition "Saddle_Ply_160_1b1a" "Saddle_Ply_180_1b1a" = 'True
  ValidTransition "Saddle_Ply_140_2d3c" "Saddle_Ply_160_1b1a" = 'True
  ValidTransition "Saddle_Ply_136_1a1b" "Saddle_Ply_140_2d3c" = 'True
  ValidTransition "Saddle_Ply_120_1c1b" "Saddle_Ply_136_1a1b" = 'True
  ValidTransition "Saddle_Ply_100_3e2d" "Saddle_Ply_120_1c1b" = 'True
  ValidTransition "Saddle_Ply_88_4c3c" "Saddle_Ply_100_3e2d" = 'True
  ValidTransition "Saddle_Ply_80_4b3c" "Saddle_Ply_88_4c3c" = 'True
  ValidTransition "Saddle_Ply_60_1e4b" "Saddle_Ply_80_4b3c" = 'True
  ValidTransition "Saddle_Ply_56_1a1b" "Saddle_Ply_60_1e4b" = 'True
  ValidTransition "Saddle_Ply_48_4e4f" "Saddle_Ply_56_1a1b" = 'True
  ValidTransition "Saddle_Ply_40_4a3b" "Saddle_Ply_48_4e4f" = 'True
  ValidTransition "Saddle_Ply_20_4c4d" "Saddle_Ply_40_4a3b" = 'True
  ValidTransition "Start" "Saddle_Ply_20_4c4d" = 'True
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
  [
    (("Saddle_Ply_504_1b1a", "Saddle_Ply_512_1b1a"), ["4e4f","1a1b","4f4e","1b1a","4e4f","1a1b","4f4e","1b1a"]),
    (("Saddle_Ply_500_1a1b", "Saddle_Ply_504_1b1a"), ["4c4d","8c8d","4d4e","1b1a"]),
    (("Saddle_Ply_496_1a1b", "Saddle_Ply_500_1a1b"), ["4c4d","1b1a","4d4c","1a1b"]),
    (("Saddle_Ply_488_1a1b", "Saddle_Ply_496_1a1b"), ["4f4e","1b1a","4e4f","1a1b","4f4d","1b1a","4d4c","1a1b"]),
    (("Saddle_Ply_480_1a1b", "Saddle_Ply_488_1a1b"), ["4d4e","1b1a","4e4f","1a1b","4f4e","1b1a","4e4f","1a1b"]),
    (("Saddle_Ply_472_1a1b", "Saddle_Ply_480_1a1b"), ["4e4f","1b1a","4f4e","1a1b","4e4f","1b1a","4f4d","1a1b"]),
    (("Saddle_Ply_460_1b1a", "Saddle_Ply_472_1a1b"), ["4c4b","P*2b","4b4c","8b8c","4c4d","2b2c","4d4e","1a1b","4e4f","1b1a","4f4e","1a1b"]),
    (("Saddle_Ply_448_1b1a", "Saddle_Ply_460_1b1a"), ["4f4e","1a1b","4e4f","1b1a","4f4d","1a1b","4d4c","1b1a","4c4d","1a1b","4d4c","1b1a"]),
    (("Saddle_Ply_440_1b1a", "Saddle_Ply_448_1b1a"), ["4d4e","1a1b","4e4f","1b1a","4f4e","1a1b","4e4f","1b1a"]),
    (("Saddle_Ply_432_1b1a", "Saddle_Ply_440_1b1a"), ["4e4f","1a1b","4f4e","1b1a","4e4f","1a1b","4f4d","1b1a"]),
    (("Saddle_Ply_420_1b1a", "Saddle_Ply_432_1b1a"), ["4c4b","5e5f","4b4c","N*2a","4c4d","1a1b","4d4e","1b1a","4e4f","1a1b","4f4e","1b1a"]),
    (("Saddle_Ply_400_1b1a", "Saddle_Ply_420_1b1a"), ["4d4e","1a1b","4e4f","1b1a","4f4e","1a1b","4e4f","1b1a","4f4e","1a1b","4e4f","1b1a","4f4d","1a1b","4d4c","1b1a","4c4d","1a1b","4d4c","1b1a"]),
    (("Saddle_Ply_380_1b1a", "Saddle_Ply_400_1b1a"), ["4e4d","1a1b","4d4c+","1b1a","4c4d","1a1b","4d4e","1b1a","4e4f","1a1b","4f4e","1b1a","4e4f","1a1b","4f4e","1b1a","4e4f","1a1b","4f4d","1b1a"]),
    (("Saddle_Ply_360_1b1a", "Saddle_Ply_380_1b1a"), ["4f4e","1a1b","4e4f","1b1a","4f4e","1a1b","4e4d","1b1a","4d4f","1a1b","4f4e","1b1a","4e4f","1a1b","4f4e","1b1a","4e4f","1a1b","4f4e","1b1a"]),
    (("Saddle_Ply_340_1b1a", "Saddle_Ply_360_1b1a"), ["4b4a","1a1b","4e4f","1b1a","4a4b","1a1b","4b4a","1b1a","4a4b","1a1b","4b4a","1b1a","4a4b","1a1b","4b5b","1b1a","4f4e","1a1b","4e4f","1b1a"]),
    (("Saddle_Ply_320_1c1b", "Saddle_Ply_340_1b1a"), ["4b4a","P*4b","4a4b","1b1a","4b4a","P*4b","4a4b","1a1b","4b4a","P*4b","4a4b","1b1a","4b4a","1a1b","4a4b","1b1a","4b4a","1a1b","4a4b","1b1a"]),
    (("Saddle_Ply_304_2d2c", "Saddle_Ply_320_1c1b"), ["4b4a","2c2d","4a4b","2d2e","4b4a","2e3e","4a4b","3d4e","4b4a","3e2d","4f4g","2d1c","4g4e","P*4b","4a4b","1c1b"]),
    (("Saddle_Ply_300_2d2c", "Saddle_Ply_304_2d2c"), ["4b4a","2c2d","4a4b","2d2c"]),
    (("Saddle_Ply_296_2e3e", "Saddle_Ply_300_2d2c"), ["4b4a","3e2d","4a4b","2d2c"]),
    (("Saddle_Ply_288_2c2d", "Saddle_Ply_296_2e3e"), ["4b4a","2d2e","4a4b","2e2d","4b4a","2d2e","4a4b","2e3e"]),
    (("Saddle_Ply_280_2c1d", "Saddle_Ply_288_2c2d"), ["4b4a","1d2c","4a4b","2c2d","4b4a","2d2c","4a4b","2c2d"]),
    (("Saddle_Ply_260_1d1c", "Saddle_Ply_280_2c1d"), ["4b4a","1c1d","4a4b","1d1c","4b4a","1c1d","4a4b","1d2c","4b4a","2c1c","4a4b","1c1d","4b4a","1d1c","4a4b","1c1d","4b4a","1d2c","4a4b","2c1d"]),
    (("Saddle_Ply_240_1a2a", "Saddle_Ply_260_1d1c"), ["4b4a","2a1b","4a4b","1b1a","4b4a","1a1b","4a4b","1b1a","4b4a","1a1b","4a4b","1b1a","4b4a","1a2b","4a4b","2b1c","4b4a","1c1d","4a4b","1d1c"]),
    (("Saddle_Ply_220_5b4b", "Saddle_Ply_240_1a2a"), ["4e4f","2e3d","4a4b","2a2b","4b4a","2b1c","4a4b","1c1b","4b4a","1b1a","4a4b","1a1b","4b4a","1b1a","4a4b","1a1b","4b4a","1b1a","4a4b","1a2a"]),
    (("Saddle_Ply_200_6a5b", "Saddle_Ply_220_5b4b"), ["3f3e","3c5a","3e3d","1b1a","4a4b","1a1b","3a4a","1b1a","4a5a","1a1b","4b4a","1b1a","3g2e","1a1b","1d1c","1b2a","3d3c+","2d2e","3c4b","5b4b"]),
    (("Saddle_Ply_180_1b1a", "Saddle_Ply_200_6a5b"), ["2i2h","1a1b","2h2i","1b1a","2i2h","1a1b","2h2g","1b1a","2g2e","1a1b","2e2g","1b1a","2g2e","1a1b","2e2g","1b1a","2g2e","1a1b","2e4e","6a5b"]),
    (("Saddle_Ply_160_1b1a", "Saddle_Ply_180_1b1a"), ["3i2i","1a1b","2i2h","1b1a","2h2i","1a1b","2i2h","1b1a","2h2i","1a1b","2i2h","1b1a","2h2g","1a1b","2g2i","1b1a","2i2h","1a1b","2h2i","1b1a"]),
    (("Saddle_Ply_140_2d3c", "Saddle_Ply_160_1b1a"), ["3b3a","1a1b","1f1e","1b1a","1e1d","3c2d","3a3b","4b3c","3b3a","1a1b","3h3i","1b1a","3i3h","1a1b","3h3i","1b1a","3i3h","1a1b","3h3i","1b1a"]),
    (("Saddle_Ply_136_1a1b", "Saddle_Ply_140_2d3c"), ["3b3a","1b1a","3a3b","2d3c"]),
    (("Saddle_Ply_120_1c1b", "Saddle_Ply_136_1a1b"), ["2f2h","1b1c","2h3h","1c1b","2i3g","1b1c","3a4a","1c1b","3b3a","1b1a","3a3b","1a1b","3b3a","1b1a","3a3b","1a1b"]),
    (("Saddle_Ply_100_3e2d", "Saddle_Ply_120_1c1b"), ["2b2a","2d3e","2a2b","3e2d","2b3b","3c2c","2g2h","2c1b","2h2f","1b1c","1g1f","1c1b","2f2h","1b1c","2h2f","1c1b","2f2h","1b1c","2h2f","1c1b"]),
    (("Saddle_Ply_88_4c3c", "Saddle_Ply_100_3e2d"), ["2a2b","3b2c","2b2a","2c2d","2h2g","2d3e","2a2b","3e2d","2b2a","2d3e","2a2b","3e2d"]),
    (("Saddle_Ply_80_4b3c", "Saddle_Ply_88_4c3c"), ["2a2b","3c4b","2b2a","4b3c","2a2b","3c4b","2b2a","4c3c"]),
    (("Saddle_Ply_60_1e4b", "Saddle_Ply_80_4b3c"), ["P*1g","1d1e","2b2a","4b3c","2a2b","3c4b","2b2a","4b3c","2a2b","3c4b","2b2a","4b3c","2a2b","5a4b","2b2a","4b4c","2a2b","3c4b","2b2a","4b3c"]),
    (("Saddle_Ply_56_1a1b", "Saddle_Ply_60_1e4b"), ["2b3a","1b1d","2c2b","1e4b"]),
    (("Saddle_Ply_48_4e4f", "Saddle_Ply_56_1a1b"), ["2d2c+","3c4e","1d1c+","2b3c","1c2b","3c1e","3i4h","1a1b"]),
    (("Saddle_Ply_40_4a3b", "Saddle_Ply_48_4e4f"), ["4g4f","1c1d","2e2d","3a3b","1g1f","2a3c","1f1d","4e4f"]),
    (("Saddle_Ply_20_4c4d", "Saddle_Ply_40_4a3b"), ["1i1h","6e6f","2f2e","3c3d","1g1f","8d8e","2g2f","8c8d","3g3f","5c5d","6g6f","7c7d","7g7f","6d6e","5g5f","9a9b","4f4e","6c6d","6i5h","4a3b"]),
    (("Start", "Saddle_Ply_20_4c4d"), ["P*2e","P*5e","5g5f","3c2d","2e2d","3b3c","P*5g","P*1c","1c1b","5e5f","1d1c","5d5e","P*6c","P*1b","1e1d","4d4e","1h1g","1c1d","1f1e","4c4d"])
  ]
