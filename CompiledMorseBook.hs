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

-- | Node ID: Minimum_Ply_512_Convergence (Type: Minimum, Potential: -13625.7932470996)
-- | Node ID: Saddle_Ply_100_7a6b (Type: Saddle, Potential: 8899.470616870032)
-- | Node ID: Saddle_Ply_125_4i5h (Type: Saddle, Potential: -408.44587962474145)
-- | Node ID: Saddle_Ply_130_2d5d (Type: Saddle, Potential: 9507.970523134647)
-- | Node ID: Saddle_Ply_150_7i8i (Type: Saddle, Potential: 11852.390199494372)
-- | Node ID: Saddle_Ply_175_S*6d (Type: Saddle, Potential: -11256.747554815738)
-- | Node ID: Saddle_Ply_200_L*1g (Type: Saddle, Potential: 13201.31700863069)
-- | Node ID: Saddle_Ply_210_N*2d (Type: Saddle, Potential: 12431.088341325596)
-- | Node ID: Saddle_Ply_220_S*1a (Type: Saddle, Potential: 10997.52389932081)
-- | Node ID: Saddle_Ply_225_9b9c (Type: Saddle, Potential: -10000.0)
-- | Node ID: Saddle_Ply_250_L*2b (Type: Saddle, Potential: 10052.500738086548)
-- | Node ID: Saddle_Ply_25_4h4g (Type: Saddle, Potential: 3474.1467510114376)
-- | Node ID: Saddle_Ply_275_S*6e (Type: Saddle, Potential: -14162.366724336567)
-- | Node ID: Saddle_Ply_300_P*6b (Type: Saddle, Potential: 14801.910038607557)
-- | Node ID: Saddle_Ply_30_4c5c (Type: Saddle, Potential: 5179.316166703565)
-- | Node ID: Saddle_Ply_310_P*7b (Type: Saddle, Potential: 15384.768245828593)
-- | Node ID: Saddle_Ply_320_L*1b (Type: Saddle, Potential: 15522.352123878933)
-- | Node ID: Saddle_Ply_325_1a2a (Type: Saddle, Potential: -6392.39601742289)
-- | Node ID: Saddle_Ply_350_S*1b (Type: Saddle, Potential: 14983.928822618911)
-- | Node ID: Saddle_Ply_360_L*1a (Type: Saddle, Potential: 15866.904414668366)
-- | Node ID: Saddle_Ply_375_1a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Saddle_Ply_380_1b2a (Type: Saddle, Potential: 0.0)
-- | Node ID: Saddle_Ply_400_7c6d (Type: Saddle, Potential: 14207.303600623367)
-- | Node ID: Saddle_Ply_40_2b7g+ (Type: Saddle, Potential: 7211.827108331045)
-- | Node ID: Saddle_Ply_425_S*9f (Type: Saddle, Potential: -3694.736737228461)
-- | Node ID: Saddle_Ply_440_8c8d (Type: Saddle, Potential: 12169.084118927332)
-- | Node ID: Saddle_Ply_450_L*1c (Type: Saddle, Potential: 13540.241287073895)
-- | Node ID: Saddle_Ply_460_8d8f (Type: Saddle, Potential: 12774.917850102978)
-- | Node ID: Saddle_Ply_475_7b8b (Type: Saddle, Potential: -10000.0)
-- | Node ID: Saddle_Ply_500_8g8c (Type: Saddle, Potential: 16749.157614485393)
-- | Node ID: Saddle_Ply_50_6h6i+ (Type: Saddle, Potential: 8342.780008807898)
-- | Node ID: Saddle_Ply_510_6b5c (Type: Saddle, Potential: 17825.93701905547)
-- | Node ID: Saddle_Ply_60_5b3b (Type: Saddle, Potential: 3657.5389429885895)
-- | Node ID: Saddle_Ply_75_P*5b (Type: Saddle, Potential: 7724.499140347784)
-- | Node ID: Saddle_Ply_90_3h2i (Type: Saddle, Potential: 7640.01009318562)
-- | Node ID: Start (Type: Saddle, Potential: 0.0)

-- ============================================================================
-- 2. 勾配流（V-Path）の型レベル二項遷移関係の定義（Type Family）
--    アクティブ・ラーニングで収束が確認されたパスのみが 'True と判定されます。
-- ============================================================================
type family ValidTransition (from :: Symbol) (to :: Symbol) :: Bool where
  ValidTransition "Saddle_Ply_510_6b5c" "Minimum_Ply_512_Convergence" = 'True
  ValidTransition "Saddle_Ply_500_8g8c" "Saddle_Ply_510_6b5c" = 'True
  ValidTransition "Saddle_Ply_475_7b8b" "Saddle_Ply_500_8g8c" = 'True
  ValidTransition "Saddle_Ply_460_8d8f" "Saddle_Ply_475_7b8b" = 'True
  ValidTransition "Saddle_Ply_450_L*1c" "Saddle_Ply_460_8d8f" = 'True
  ValidTransition "Saddle_Ply_440_8c8d" "Saddle_Ply_450_L*1c" = 'True
  ValidTransition "Saddle_Ply_425_S*9f" "Saddle_Ply_440_8c8d" = 'True
  ValidTransition "Saddle_Ply_400_7c6d" "Saddle_Ply_425_S*9f" = 'True
  ValidTransition "Saddle_Ply_380_1b2a" "Saddle_Ply_400_7c6d" = 'True
  ValidTransition "Saddle_Ply_375_1a1b" "Saddle_Ply_380_1b2a" = 'True
  ValidTransition "Saddle_Ply_360_L*1a" "Saddle_Ply_375_1a1b" = 'True
  ValidTransition "Saddle_Ply_350_S*1b" "Saddle_Ply_360_L*1a" = 'True
  ValidTransition "Saddle_Ply_325_1a2a" "Saddle_Ply_350_S*1b" = 'True
  ValidTransition "Saddle_Ply_320_L*1b" "Saddle_Ply_325_1a2a" = 'True
  ValidTransition "Saddle_Ply_310_P*7b" "Saddle_Ply_320_L*1b" = 'True
  ValidTransition "Saddle_Ply_300_P*6b" "Saddle_Ply_310_P*7b" = 'True
  ValidTransition "Saddle_Ply_275_S*6e" "Saddle_Ply_300_P*6b" = 'True
  ValidTransition "Saddle_Ply_250_L*2b" "Saddle_Ply_275_S*6e" = 'True
  ValidTransition "Saddle_Ply_225_9b9c" "Saddle_Ply_250_L*2b" = 'True
  ValidTransition "Saddle_Ply_220_S*1a" "Saddle_Ply_225_9b9c" = 'True
  ValidTransition "Saddle_Ply_210_N*2d" "Saddle_Ply_220_S*1a" = 'True
  ValidTransition "Saddle_Ply_200_L*1g" "Saddle_Ply_210_N*2d" = 'True
  ValidTransition "Saddle_Ply_175_S*6d" "Saddle_Ply_200_L*1g" = 'True
  ValidTransition "Saddle_Ply_150_7i8i" "Saddle_Ply_175_S*6d" = 'True
  ValidTransition "Saddle_Ply_130_2d5d" "Saddle_Ply_150_7i8i" = 'True
  ValidTransition "Saddle_Ply_125_4i5h" "Saddle_Ply_130_2d5d" = 'True
  ValidTransition "Saddle_Ply_100_7a6b" "Saddle_Ply_125_4i5h" = 'True
  ValidTransition "Saddle_Ply_90_3h2i" "Saddle_Ply_100_7a6b" = 'True
  ValidTransition "Saddle_Ply_75_P*5b" "Saddle_Ply_90_3h2i" = 'True
  ValidTransition "Saddle_Ply_60_5b3b" "Saddle_Ply_75_P*5b" = 'True
  ValidTransition "Saddle_Ply_50_6h6i+" "Saddle_Ply_60_5b3b" = 'True
  ValidTransition "Saddle_Ply_40_2b7g+" "Saddle_Ply_50_6h6i+" = 'True
  ValidTransition "Saddle_Ply_30_4c5c" "Saddle_Ply_40_2b7g+" = 'True
  ValidTransition "Saddle_Ply_25_4h4g" "Saddle_Ply_30_4c5c" = 'True
  ValidTransition "Start" "Saddle_Ply_25_4h4g" = 'True
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
    (("Saddle_Ply_510_6b5c", "Minimum_Ply_512_Convergence"), ["1a2a","8c7b"]),
    (("Saddle_Ply_500_8g8c", "Saddle_Ply_510_6b5c"), ["2a1a","S*2a","1a2a","P*3a","S*6a","G*1a","2a1a","B*2a","6a7b+","6b5c"]),
    (("Saddle_Ply_475_7b8b", "Saddle_Ply_500_8g8c"), ["S*1b","8b8c","7d8c","1a1b","S*2b","S*7c","8c7c","1b2a","2b2c","2a1a","2c2d","1a1b","S*1c","1b2c","S*3c","2c1b","S*1a","1b1a","G*2a","1a2a","3f3g+","S*7d","7c6b","7d8c+","8g8c"]),
    (("Saddle_Ply_460_8d8f", "Saddle_Ply_475_7b8b"), ["S*7c","8f8g","2a1a","3a3b","7c7b+","3b3c","7b7a","3c3d","7a7b","3d3e","7b7a","3e3f","7a7b","3g4g","7b8b"]),
    (("Saddle_Ply_450_L*1c", "Saddle_Ply_460_8d8f"), ["1b1a","S*1b","1a1b","1c1d","S*8f","8e7d","1b1a","S*2a","1a2a","8d8f"]),
    (("Saddle_Ply_440_8c8d", "Saddle_Ply_450_L*1c"), ["3d2c","6h7h","G*7e","9g7e","G*8f","7e8f","B*7e","8f7e","2c1b","L*1c"]),
    (("Saddle_Ply_425_S*9f", "Saddle_Ply_440_8c8d"), ["9e8d","3c2d","7e9g+","9f8e","8d7e","6c6f","7f6f","2d2e","1b1c","9d8c+","7e8e","P*8d","1c8c","2e3d","8c8d"]),
    (("Saddle_Ply_400_7c6d", "Saddle_Ply_425_S*9f"), ["P*8e","8d8e","P*7f","7e7f","L*7e","6d7e","7b6c","L*1c","1b1c","7d1d","1c2b","1d1b","2b3c","P*6g","L*7g","7f7g","P*7h","7g8f","S*7f","6e7f","S*8g","8f9e","S*9d","6g6h+","S*9f"]),
    (("Saddle_Ply_380_1b2a", "Saddle_Ply_400_7c6d"), ["1a2a","2c3e","2a1a","2d3f","L*6e","5d6e","R*5d","6d7e","1a1b","L*1a","1b1a","2g3g","1a1b","3e2g+","5d5a+","3f4h+","5a8a","P*8d","8a7b","7c6d"]),
    (("Saddle_Ply_375_1a1b", "Saddle_Ply_380_1b2a"), ["3b2b","1b2b","B*1b","2b1a","1b2a"]),
    (("Saddle_Ply_360_L*1a", "Saddle_Ply_375_1a1b"), ["2a1a","G*1b","1a2a","3b3g+","2a1b","1d1e","4c2e+","3g3b","P*2b","1c1d","2e1e","1d1e","1b1a","P*3a","1a1b"]),
    (("Saddle_Ply_350_S*1b", "Saddle_Ply_360_L*1a"), ["1a1b","1e1f","1b1a","P*1b","1a1b","P*1c","1b1a","4b3b","1a2a","L*1a"]),
    (("Saddle_Ply_325_1a2a", "Saddle_Ply_350_S*1b"), ["P*8a","3a4a","6e7d","G*5c","6c5c","L*5d","6d5d","S*6c","6b6c","2a3b","4c4a","P*4c","L*4b","3b2b","1g2g","4c4b","4a4b","L*3b","S*1b","B*4c","1f1g+","2b1b","5c6d","1b1a","S*1b"]),
    (("Saddle_Ply_320_L*1b", "Saddle_Ply_325_1a2a"), ["1a1b","5c4c","1b1a","S*2a","1a2a"]),
    (("Saddle_Ply_310_P*7b", "Saddle_Ply_320_L*1b"), ["P*5c","5d5c","3b2b","1c1d","2b3a","P*3b","L*6e","9e6e","S*4c","L*1b"]),
    (("Saddle_Ply_300_P*6b", "Saddle_Ply_310_P*7b"), ["2a1a","1d1e","1a1b","N*1d","1b1a","6e6d","S*4c","1d2f","4c3b+","P*7b"]),
    (("Saddle_Ply_275_S*6e", "Saddle_Ply_300_P*6b"), ["5d6e","2a1a","S*2a","1a2a","1c1d","2a1b","S*1c","1b1a","R*5d","N*7d","1e1f","S*6b","6c6b","7d6b+","S*1b","B*6c","5c6c","6b6a","B*7c","6a6b","1b2a","6b6c","6d6c","1a2a","P*6b"]),
    (("Saddle_Ply_250_L*2b", "Saddle_Ply_275_S*6e"), ["8f8e","6e5d","8e8d","7d8d","9c9b","N*2c","9b9c","P*3b","9c8c","1d1e","8c7c","S*1d","7c6b","G*4a","6b6c","5d6c","9f9e","G*5d","1a2a","8d9e","2a1a","P*5e","1a2a","1d2e","S*6e"]),
    (("Saddle_Ply_225_9b9c", "Saddle_Ply_250_L*2b"), ["1g1h+","6a7a","1f1g+","P*6f","6e6f","S*6b","8a9c","6b5c+","6c5c","7a8a","P*1b","8g8f","1b1c","8a9a","N*1b","L*6e","5d6e","9a9b","S*3a","9g9f","1c1d","9b9c","G*1c","N*5d","L*2b"]),
    (("Saddle_Ply_220_S*1a", "Saddle_Ply_225_9b9c"), ["1b1a","S*2a","5a6a","1h1i","9b9c"]),
    (("Saddle_Ply_210_N*2d", "Saddle_Ply_220_S*1a"), ["3d2c","1e1f","2c1c","N*1b","1c1b","3g3h+","N*8d","2g3i+","8d9b+","S*1a"]),
    (("Saddle_Ply_200_L*1g", "Saddle_Ply_210_N*2d"), ["3f4e","5e6d","4e5d","4d5d","2g2f","N*2g","2f3e","P*4e","3e3d","N*2d"]),
    (("Saddle_Ply_175_S*6d", "Saddle_Ply_200_L*1g"), ["5c6d","3h2g","6d6c","2g1g","1i2i","S*4d","P*1f","1g1f","P*1e","1f1g","L*1f","1g2g","5e4d","P*6d","1f1h+","G*5c","6c5c","G*5e","5d5e","6d6c+","5c6c","4g3f","P*3g","6a5a","L*1g"]),
    (("Saddle_Ply_150_7i8i", "Saddle_Ply_175_S*6d"), ["6b6a+","P*4g","5g4g","P*3h","4h4i","8i9i","N*5c","1h1i","4i3h","6d5c","P*6d","6c6d","P*6c","6d6c","S*6d","6c6d","P*6c","P*2h","P*7d","7c7d","P*5d","6d5d","S*5e","9i5e","S*6d"]),
    (("Saddle_Ply_130_2d5d", "Saddle_Ply_150_7i8i"), ["L*8d","8c8d","N*7e","7d6d","P*6c","5d6c","7e8c+","P*4h","8c8d","B*6h","8d7c","6c7c","5h4h","6h7i+","P*6c","6d6c","P*6d","2h6d","P*6b","7i8i"]),
    (("Saddle_Ply_125_4i5h", "Saddle_Ply_130_2d5d"), ["3i3d+","P*6d","3d2d","5c5d","2d5d"]),
    (("Saddle_Ply_100_7a6b", "Saddle_Ply_125_4i5h"), ["2c3d","3h3g","2f2e","S*4i","2e2d","S*5h","6h5h","4i5h+","5i5h","3g4g","4f4g","G*4h","4g4h","P*5g","5h4i","B*3i","4b5b","6c6d","5b6b","6d7d","6b5c","3i2h+","4h5g","R*3i","4i5h"]),
    (("Saddle_Ply_90_3h2i", "Saddle_Ply_100_7a6b"), ["3b2c","2i3h+","R*4a","5a6b","4a4b+","N*5b","P*6c","6b6c","P*6b","7a6b"]),
    (("Saddle_Ply_75_P*5b", "Saddle_Ply_90_3h2i"), ["6a5b","S*6a","5a6a","B*5a","6a5a","P*5c","5b5c","2a3b","S*4i","2g2f","6i6h","5h6h","4i3h","4h5i","3h2i"]),
    (("Saddle_Ply_60_5b3b", "Saddle_Ply_75_P*5b"), ["7g6h","1e1g+","8h1a+","1g1h","1a2a","L*3h","3i3h","S*5h","4i5h","P*4f","P*5b","5a5b","5g4f","5b5a","P*5b"]),
    (("Saddle_Ply_50_6h6i+", "Saddle_Ply_60_5b3b"), ["5i4h","P*6h","4f4b+","6b4b","P*5b","4b5b","S*4a","1c1e","4a3b+","5b3b"]),
    (("Saddle_Ply_40_2b7g+", "Saddle_Ply_50_6h6i+"), ["6h7g","P*5h","6i5h","P*5g","5h5g","5d6e","1g1f","P*6h","1f1e","6h6i+"]),
    (("Saddle_Ply_30_4c5c", "Saddle_Ply_40_2b7g+"), ["P*5d","5c5d","5h5f","1c1d","5f4f","1d1e","1i1h","1b1c","1h1g","2b7g+"]),
    (("Saddle_Ply_25_4h4g", "Saddle_Ply_30_4c5c"), ["P*4f","6f6e","5e5f","P*5c","4c5c"]),
    (("Start", "Saddle_Ply_25_4h4g"), ["2h5h","8b6b","4g4f","1a1b","1g1f","4a5b","5g5f","6c6d","1f1e","3a4b","5f5e","4c4d","4f4e","5c5d","4e4d","6d6e","7i6h","3c3d","4d4c","5b4c","P*4h","P*4g","6g6f","5d5e","4h4g"])
  ]
