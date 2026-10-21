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

-- | Node ID: Minimum_Ply_512_Convergence (Type: Minimum, Potential: 9779.436568552306)
-- | Node ID: Saddle_Ply_100_1b1a (Type: Saddle, Potential: 731.0748468116483)
-- | Node ID: Saddle_Ply_10_5d5e (Type: Saddle, Potential: 4581.369772446983)
-- | Node ID: Saddle_Ply_125_S*1c (Type: Saddle, Potential: 691.8750039403243)
-- | Node ID: Saddle_Ply_130_1c1b (Type: Saddle, Potential: 3956.0297511366794)
-- | Node ID: Saddle_Ply_150_L*6h (Type: Saddle, Potential: -4490.541166613273)
-- | Node ID: Saddle_Ply_160_G*7i (Type: Saddle, Potential: 1609.2888822374193)
-- | Node ID: Saddle_Ply_170_5e5d (Type: Saddle, Potential: 1333.0505492912437)
-- | Node ID: Saddle_Ply_175_S*5e (Type: Saddle, Potential: 2087.5648478321505)
-- | Node ID: Saddle_Ply_200_P*3h (Type: Saddle, Potential: 11457.66371389029)
-- | Node ID: Saddle_Ply_20_N*5f (Type: Saddle, Potential: 1499.4494063344403)
-- | Node ID: Saddle_Ply_220_P*2f (Type: Saddle, Potential: 9958.806447237213)
-- | Node ID: Saddle_Ply_225_L*5c (Type: Saddle, Potential: -5409.704885629347)
-- | Node ID: Saddle_Ply_230_1h1i+ (Type: Saddle, Potential: 10799.92823840637)
-- | Node ID: Saddle_Ply_250_L*1c (Type: Saddle, Potential: -1056.5231451490504)
-- | Node ID: Saddle_Ply_25_P*5c (Type: Saddle, Potential: 821.6313257332198)
-- | Node ID: Saddle_Ply_275_1d2c (Type: Saddle, Potential: 0.0)
-- | Node ID: Saddle_Ply_280_1c1g+ (Type: Saddle, Potential: -1688.1493965277314)
-- | Node ID: Saddle_Ply_300_9a9b (Type: Saddle, Potential: -6841.796110691312)
-- | Node ID: Saddle_Ply_30_P*5e (Type: Saddle, Potential: 3978.142473912748)
-- | Node ID: Saddle_Ply_325_1a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Saddle_Ply_350_8b7b (Type: Saddle, Potential: -6064.143816142289)
-- | Node ID: Saddle_Ply_360_9c9b (Type: Saddle, Potential: 7015.17065533352)
-- | Node ID: Saddle_Ply_375_P*9c (Type: Saddle, Potential: 7582.7695246742915)
-- | Node ID: Saddle_Ply_400_9b9a (Type: Saddle, Potential: -10000.0)
-- | Node ID: Saddle_Ply_40_4i6i (Type: Saddle, Potential: 7205.160487936695)
-- | Node ID: Saddle_Ply_425_1a1b (Type: Saddle, Potential: 10000.0)
-- | Node ID: Saddle_Ply_450_S*2c (Type: Saddle, Potential: -4547.050628056471)
-- | Node ID: Saddle_Ply_475_2c2b (Type: Saddle, Potential: 7463.52204725939)
-- | Node ID: Saddle_Ply_500_8g9f (Type: Saddle, Potential: -6101.385518818275)
-- | Node ID: Saddle_Ply_50_2a1c (Type: Saddle, Potential: 0.0)
-- | Node ID: Saddle_Ply_510_9f9g (Type: Saddle, Potential: 1010.9881003234855)
-- | Node ID: Saddle_Ply_70_9c9i+ (Type: Saddle, Potential: 4639.158999874038)
-- | Node ID: Saddle_Ply_75_P*1b (Type: Saddle, Potential: 1592.317345114745)
-- | Node ID: Saddle_Ply_90_1b1a (Type: Saddle, Potential: 2945.217373559468)
-- | Node ID: Start (Type: Saddle, Potential: 0.0)

-- ============================================================================
-- 2. 勾配流（V-Path）の型レベル二項遷移関係の定義（Type Family）
--    アクティブ・ラーニングで収束が確認されたパスのみが 'True と判定されます。
-- ============================================================================
type family ValidTransition (from :: Symbol) (to :: Symbol) :: Bool where
  ValidTransition "Saddle_Ply_510_9f9g" "Minimum_Ply_512_Convergence" = 'True
  ValidTransition "Saddle_Ply_500_8g9f" "Saddle_Ply_510_9f9g" = 'True
  ValidTransition "Saddle_Ply_475_2c2b" "Saddle_Ply_500_8g9f" = 'True
  ValidTransition "Saddle_Ply_450_S*2c" "Saddle_Ply_475_2c2b" = 'True
  ValidTransition "Saddle_Ply_425_1a1b" "Saddle_Ply_450_S*2c" = 'True
  ValidTransition "Saddle_Ply_400_9b9a" "Saddle_Ply_425_1a1b" = 'True
  ValidTransition "Saddle_Ply_375_P*9c" "Saddle_Ply_400_9b9a" = 'True
  ValidTransition "Saddle_Ply_360_9c9b" "Saddle_Ply_375_P*9c" = 'True
  ValidTransition "Saddle_Ply_350_8b7b" "Saddle_Ply_360_9c9b" = 'True
  ValidTransition "Saddle_Ply_325_1a1b" "Saddle_Ply_350_8b7b" = 'True
  ValidTransition "Saddle_Ply_300_9a9b" "Saddle_Ply_325_1a1b" = 'True
  ValidTransition "Saddle_Ply_280_1c1g+" "Saddle_Ply_300_9a9b" = 'True
  ValidTransition "Saddle_Ply_275_1d2c" "Saddle_Ply_280_1c1g+" = 'True
  ValidTransition "Saddle_Ply_250_L*1c" "Saddle_Ply_275_1d2c" = 'True
  ValidTransition "Saddle_Ply_230_1h1i+" "Saddle_Ply_250_L*1c" = 'True
  ValidTransition "Saddle_Ply_225_L*5c" "Saddle_Ply_230_1h1i+" = 'True
  ValidTransition "Saddle_Ply_220_P*2f" "Saddle_Ply_225_L*5c" = 'True
  ValidTransition "Saddle_Ply_200_P*3h" "Saddle_Ply_220_P*2f" = 'True
  ValidTransition "Saddle_Ply_175_S*5e" "Saddle_Ply_200_P*3h" = 'True
  ValidTransition "Saddle_Ply_170_5e5d" "Saddle_Ply_175_S*5e" = 'True
  ValidTransition "Saddle_Ply_160_G*7i" "Saddle_Ply_170_5e5d" = 'True
  ValidTransition "Saddle_Ply_150_L*6h" "Saddle_Ply_160_G*7i" = 'True
  ValidTransition "Saddle_Ply_130_1c1b" "Saddle_Ply_150_L*6h" = 'True
  ValidTransition "Saddle_Ply_125_S*1c" "Saddle_Ply_130_1c1b" = 'True
  ValidTransition "Saddle_Ply_100_1b1a" "Saddle_Ply_125_S*1c" = 'True
  ValidTransition "Saddle_Ply_90_1b1a" "Saddle_Ply_100_1b1a" = 'True
  ValidTransition "Saddle_Ply_75_P*1b" "Saddle_Ply_90_1b1a" = 'True
  ValidTransition "Saddle_Ply_70_9c9i+" "Saddle_Ply_75_P*1b" = 'True
  ValidTransition "Saddle_Ply_50_2a1c" "Saddle_Ply_70_9c9i+" = 'True
  ValidTransition "Saddle_Ply_40_4i6i" "Saddle_Ply_50_2a1c" = 'True
  ValidTransition "Saddle_Ply_30_P*5e" "Saddle_Ply_40_4i6i" = 'True
  ValidTransition "Saddle_Ply_25_P*5c" "Saddle_Ply_30_P*5e" = 'True
  ValidTransition "Saddle_Ply_20_N*5f" "Saddle_Ply_25_P*5c" = 'True
  ValidTransition "Saddle_Ply_10_5d5e" "Saddle_Ply_20_N*5f" = 'True
  ValidTransition "Start" "Saddle_Ply_10_5d5e" = 'True
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
    (("Saddle_Ply_510_9f9g", "Minimum_Ply_512_Convergence"), ["L*8g","9g9h"]),
    (("Saddle_Ply_500_8g9f", "Saddle_Ply_510_9f9g"), ["1b2a","P*2b","2a1a","2g2f","1a2b","2f2e","1e2e","1g2g","P*8f","9f9g"]),
    (("Saddle_Ply_475_2c2b", "Saddle_Ply_500_8g9f"), ["B*2c","2b1a","2c4a","3a4a","N*2a","1a2a","R*1a","2a1a","R*1c","P*9d","9e9d","P*9c","P*2a","1b1c","1g1f","1a1b","1f1g","1c1d","9d9e","1d1e","9e9f","L*8f","9f8g","8f8c+","8g9f"]),
    (("Saddle_Ply_450_S*2c", "Saddle_Ply_475_2c2b"), ["2b2c","1d2c","L*9b","9c9b","S*8b","9b8b","S*8a","1c1d","G*9b","8b7c","N*5c","P*2b","2a2b","L*1b","2b2c","7c8d","1a1b","S*1c","2c1c","8d9e","5c4a+","G*2c","1c2c","P*2b","2c2b"]),
    (("Saddle_Ply_425_1a1b", "Saddle_Ply_450_S*2c"), ["1c2e","L*9b","9a9b","L*8b","2e3g+","S*9a","L*1a","2a1a","9b9a","2b2a+","P*2b","2a2b","9a8b","P*7b","8b9b","S*8b","L*1c","1a2a","S*1a","2a1a","S*1d","8b9c+","9b9c","1b2a","S*2c"]),
    (("Saddle_Ply_400_9b9a", "Saddle_Ply_425_1a1b"), ["3b3a+","1d1e","7b8a","9a9b","8a9a","9b8b","7c7b","8b9a","P*2b","L*1b","1a1b","L*1a","1b1a","1f1g","7b7a+","N*1b","1a1b","N*1c","1b1a","N*1b","1a1b","S*1a","1b1a","S*1b","1a1b"]),
    (("Saddle_Ply_375_P*9c", "Saddle_Ply_400_9b9a"), ["9b9c","P*9b","9a9b","7c7b+","1f1g","P*7c","P*1b","1a1b","P*1a","1b1a","1g1f","P*9e","P*1b","P*2b","P*2a","1a2a","L*1a","P*3b","1b1c","2a1a","P*2a","2b2a+","1c1d","L*8b","9b9a"]),
    (("Saddle_Ply_360_9c9b", "Saddle_Ply_375_P*9c"), ["P*7c","P*1b","1a1b","1g1f","1b1a","P*1b","1a1b","1f1g","1b1a","P*1b","1a1b","8a9a","1b1a","1g1f","P*9c"]),
    (("Saddle_Ply_350_8b7b", "Saddle_Ply_360_9c9b"), ["N*7d","7b8a","1a1b","P*1a","1b2a","7c7d","2a1a","1f1g","P*9b","9c9b"]),
    (("Saddle_Ply_325_1a1b", "Saddle_Ply_350_8b7b"), ["1f1g+","1b2b","1g1f","9d8b+","7b8b","P*9c","8b9c","L*8b","P*1b","2b1b","9b8b","P*7b","8b7b","P*6b","1f1g","6b6a+","P*1a","1b1a","1g1f","P*6b","P*1b","L*8b","7b8b","P*7b","8b7b"]),
    (("Saddle_Ply_300_9a9b", "Saddle_Ply_325_1a1b"), ["1a2b","P*1b","2b1a","1b1c","P*9c","9b9a","N*9d","P*2a","9c9b+","9a9b","1a1b","L*2b","1b1a","L*1b","1a2a","N*1a","2a1a","P*2a","1a1b","1c1d","1b2a","1d1e","2a1a","1e1f","1a1b"]),
    (("Saddle_Ply_280_1c1g+", "Saddle_Ply_300_9a9b"), ["L*9d","P*2c","2d2c","P*2b","2c1b","1g2h","9d9c+","9b9c","1b1a","P*1b","1a1b","1h2i","1b1a","1i1h","P*9b","9c9b","P*9c","9b9a","9c9b+","9a9b"]),
    (("Saddle_Ply_275_1d2c", "Saddle_Ply_280_1c1g+"), ["1c1d","2c1d","L*1c","1d2d","1c1g+"]),
    (("Saddle_Ply_250_L*1c", "Saddle_Ply_275_1d2c"), ["1d2c","1c1d","2c1d","L*1c","1d2c","1c1g+","2c1b","1g1h","P*9b","P*1a","1b1c","8b9b","L*9c","9b9c","1c1d","P*2d","1d2d","1a1b","P*9d","9c9b","9d9c+","8a9c","2d1d","L*1c","1d2c"]),
    (("Saddle_Ply_230_1h1i+", "Saddle_Ply_250_L*1c"), ["S*5b","5a6b","1d1c","P*1b","1c1d","6b5b","G*4b","5b6a","G*5a","6a6b","G*6a","6b7b","R*6b","7a6b","6a7a","7b8b","7a7b","6b7b","4b5b","L*1c"]),
    (("Saddle_Ply_225_L*5c", "Saddle_Ply_230_1h1i+"), ["5b6a","5c5a+","6a5a","1e1d","1h1i+"]),
    (("Saddle_Ply_220_P*2f", "Saddle_Ply_225_L*5c"), ["1f1e","2f2g+","5c5b+","4a5b","L*5c"]),
    (("Saddle_Ply_200_P*3h", "Saddle_Ply_220_P*2f"), ["4h3h","P*2h","6h5g","P*3g","3h2h","P*1h","2h1g","3g3h+","P*5c","5d5c","P*5b","5c5b","P*5c","5b5c","P*5b","5c5b","P*5c","5b4a","1g1f","P*2f"]),
    (("Saddle_Ply_175_S*5e", "Saddle_Ply_200_P*3h"), ["5d5e","5c5b+","6b5b","P*5d","5b2b","1a2b+","5e5d","2b4d","5d6d","S*5d","5g6g","4d7a","6a7a","2a3a","6d5d","3a4a","6g7g","8i7g","L*6h","6i5h","P*4h","7i6h","P*5g","5h4h","P*3h"]),
    (("Saddle_Ply_170_5e5d", "Saddle_Ply_175_S*5e"), ["3e4d","5c4d","P*5c","5f5g+","S*5e"]),
    (("Saddle_Ply_160_G*7i", "Saddle_Ply_170_5e5d"), ["6h7i","P*5f","P*4f","4e5d","L*5c","5b5c","L*5e","5d5e","2e3e","5e5d"]),
    (("Saddle_Ply_150_L*6h", "Saddle_Ply_160_G*7i"), ["7i6h","G*7i","6h7i","2d2e","L*4d","4c4d","2i2e","L*6h","7i6h","G*7i"]),
    (("Saddle_Ply_130_1c1b", "Saddle_Ply_150_L*6h"), ["L*2b","1b1c","G*1b","1c1b","B*1a","1b1c","R*1d","1c2c","1d1b+","2c3d","1b1d","3d4e","1d2e","S*7i","2e2i","7i6h+","5i6h","S*7i","6h7i","L*6h"]),
    (("Saddle_Ply_125_S*1c", "Saddle_Ply_130_1c1b"), ["1b1c","S*1b","L*6h","7h6h","1c1b"]),
    (("Saddle_Ply_100_1b1a", "Saddle_Ply_125_S*1c"), ["L*1b","1a1b","L*1c","1b1c","3f3e","3d3e","L*1b","1c1b","4f4e","S*6i","N*1c","1b1a","7i6i","L*5h","1c2a+","1a1b","N*1c","5h5i+","6h5i","1b1c","L*1d","1c2c","1d1b+","2c1b","S*1c"]),
    (("Saddle_Ply_90_1b1a", "Saddle_Ply_100_1b1a"), ["L*1b","1a1b","3g3f","L*6h","7h6h","L*7h","6h7h","L*6h","5g6h","1b1a"]),
    (("Saddle_Ply_75_P*1b", "Saddle_Ply_90_1b1a"), ["2b1b","P*1c","1b1a","1c1b+","1a1b","P*1c","1b1a","1c1b+","1a1b","4h3g","S*6i","6h6i","G*7h","6i7h","1b1a"]),
    (("Saddle_Ply_70_9c9i+", "Saddle_Ply_75_P*1b"), ["8h9i","4b3b","P*2b","3b2b","P*1b"]),
    (("Saddle_Ply_50_2a1c", "Saddle_Ply_70_9c9i+"), ["3f3e","1c2e","S*4a","2b3a","3e3d","3c3d","4a5b+","5c5b","4g4f","3a9g+","9i9g","2e3g+","9g9c+","9a9c","G*4a","5a4a","L*4b","4a4b","P*3b","9c9i+"]),
    (("Saddle_Ply_40_4i6i", "Saddle_Ply_50_2a1c"), ["6h6i","L*6h","5h6h","G*7i","6i7i","P*5b","2e2d","2c2d","3g3f","2a1c"]),
    (("Saddle_Ply_30_P*5e", "Saddle_Ply_40_4i6i"), ["5f5e","P*5g","5e5c+","5b5c","6h5g","R*3i","L*4i","3i4i+","5i6h","4i6i"]),
    (("Saddle_Ply_25_P*5c", "Saddle_Ply_30_P*5e"), ["4b5c","5h5f","L*5h","4i5h","P*5e"]),
    (("Saddle_Ply_20_N*5f", "Saddle_Ply_25_P*5c"), ["L*5b","4a5b","5g5f","5e5f","P*5c"]),
    (("Saddle_Ply_10_5d5e", "Saddle_Ply_20_N*5f"), ["1d1c+","1b1c","1i1h","1c1h+","2g2f","L*6h","2f2e","1h2i","7i6h","N*5f"]),
    (("Start", "Saddle_Ply_10_5d5e"), ["2h5h","8b6b","3i4h","1a1b","1g1f","3a4b","1f1e","5c5d","1e1d","5d5e"])
  ]
