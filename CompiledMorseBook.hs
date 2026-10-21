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

-- | Node ID: Minimum_Ply_246_Convergence (Type: Minimum, Potential: -10000.0)
-- | Node ID: Saddle_Ply_100_3a4b (Type: Saddle, Potential: 6656.219527942447)
-- | Node ID: Saddle_Ply_110_G*5i (Type: Saddle, Potential: 6518.5026519184385)
-- | Node ID: Saddle_Ply_125_7f7c (Type: Saddle, Potential: 6094.7315450046635)
-- | Node ID: Saddle_Ply_130_P*6g (Type: Saddle, Potential: 319.88461915834074)
-- | Node ID: Saddle_Ply_150_9h9i+ (Type: Saddle, Potential: -4535.985092188611)
-- | Node ID: Saddle_Ply_175_1a2a (Type: Saddle, Potential: 0.0)
-- | Node ID: Saddle_Ply_200_6d5e (Type: Saddle, Potential: -2542.955190251536)
-- | Node ID: Saddle_Ply_220_3i2i (Type: Saddle, Potential: 0.0)
-- | Node ID: Saddle_Ply_225_7d8c (Type: Saddle, Potential: 3528.6869711260724)
-- | Node ID: Saddle_Ply_230_N*8f (Type: Saddle, Potential: 10000.0)
-- | Node ID: Saddle_Ply_240_2i3i (Type: Saddle, Potential: 0.0)
-- | Node ID: Saddle_Ply_25_4d3c+ (Type: Saddle, Potential: 2827.141279661868)
-- | Node ID: Saddle_Ply_30_2b7g+ (Type: Saddle, Potential: 899.3683137199205)
-- | Node ID: Saddle_Ply_50_5a4a (Type: Saddle, Potential: 3065.2910902324193)
-- | Node ID: Saddle_Ply_70_2b1a (Type: Saddle, Potential: 10000.0)
-- | Node ID: Saddle_Ply_75_6i7i (Type: Saddle, Potential: 949.967241024811)
-- | Node ID: Saddle_Ply_80_P*6g (Type: Saddle, Potential: 8219.01491300177)
-- | Node ID: Start (Type: Saddle, Potential: 0.0)

-- ============================================================================
-- 2. 勾配流（V-Path）の型レベル二項遷移関係の定義（Type Family）
--    アクティブ・ラーニングで収束が確認されたパスのみが 'True と判定されます。
-- ============================================================================
type family ValidTransition (from :: Symbol) (to :: Symbol) :: Bool where
  ValidTransition "Saddle_Ply_240_2i3i" "Minimum_Ply_246_Convergence" = 'True
  ValidTransition "Saddle_Ply_230_N*8f" "Saddle_Ply_240_2i3i" = 'True
  ValidTransition "Saddle_Ply_225_7d8c" "Saddle_Ply_230_N*8f" = 'True
  ValidTransition "Saddle_Ply_220_3i2i" "Saddle_Ply_225_7d8c" = 'True
  ValidTransition "Saddle_Ply_200_6d5e" "Saddle_Ply_220_3i2i" = 'True
  ValidTransition "Saddle_Ply_175_1a2a" "Saddle_Ply_200_6d5e" = 'True
  ValidTransition "Saddle_Ply_150_9h9i+" "Saddle_Ply_175_1a2a" = 'True
  ValidTransition "Saddle_Ply_130_P*6g" "Saddle_Ply_150_9h9i+" = 'True
  ValidTransition "Saddle_Ply_125_7f7c" "Saddle_Ply_130_P*6g" = 'True
  ValidTransition "Saddle_Ply_110_G*5i" "Saddle_Ply_125_7f7c" = 'True
  ValidTransition "Saddle_Ply_100_3a4b" "Saddle_Ply_110_G*5i" = 'True
  ValidTransition "Saddle_Ply_80_P*6g" "Saddle_Ply_100_3a4b" = 'True
  ValidTransition "Saddle_Ply_75_6i7i" "Saddle_Ply_80_P*6g" = 'True
  ValidTransition "Saddle_Ply_70_2b1a" "Saddle_Ply_75_6i7i" = 'True
  ValidTransition "Saddle_Ply_50_5a4a" "Saddle_Ply_70_2b1a" = 'True
  ValidTransition "Saddle_Ply_30_2b7g+" "Saddle_Ply_50_5a4a" = 'True
  ValidTransition "Saddle_Ply_25_4d3c+" "Saddle_Ply_30_2b7g+" = 'True
  ValidTransition "Start" "Saddle_Ply_25_4d3c+" = 'True
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
    (("Saddle_Ply_240_2i3i", "Minimum_Ply_246_Convergence"), ["7h7g","6g7g","2a1a","3i2i","G*7d","L*9e"]),
    (("Saddle_Ply_230_N*8f", "Saddle_Ply_240_2i3i"), ["9f9e","P*8d","8e8d","9c9d","9e9f","2i3i","8b9a","3i2i","L*7h","2i3i"]),
    (("Saddle_Ply_225_7d8c", "Saddle_Ply_230_N*8f"), ["6e6f","8c8b+","P*8e","8f8e","N*8f"]),
    (("Saddle_Ply_220_3i2i", "Saddle_Ply_225_7d8c"), ["N*6f","P*5f","S*7d","9e8g+","7d8c"]),
    (("Saddle_Ply_200_6d5e", "Saddle_Ply_220_3i2i"), ["2a1a","4h3i","4d5b+","5e6f","7h6g","5g6g","1a2a","G*7g","8g9f","P*6e","N*7f","6f7f","2a1a","N*9e","1a2a","3i2i","2a1a","2i3i","1a2a","3i2i"]),
    (("Saddle_Ply_175_1a2a", "Saddle_Ply_200_6d5e"), ["P*5d","1h5d","5c5d","6e6d","B*9g","N*4d","5d6d","2a3a","6d7d","2b2a","9g7i+","5b5a+","7i5g","P*7c","3i4i","7c7b+","P*6g","P*6d","7d8e","P*8f","8e7d","P*7c","7d6d","P*6c","6d5e"]),
    (("Saddle_Ply_150_9h9i+", "Saddle_Ply_175_1a2a"), ["5c4a+","L*9h","1c1b","S*9e","G*5c","5b5c","3a2b+","G*8f","8g8f","R*8h","1b1a","1g1h","9g8h","9e8f","4e1h","8f7g","8h7g","7i7h","6h7h","N*8g","7g8g","P*6d","L*5b","P*7e","1a2a"]),
    (("Saddle_Ply_130_P*6g", "Saddle_Ply_150_9h9i+"), ["7c5c+","6a5b","6h5h","6g6h+","5c5b","5a5b","5h6h","5b5a","L*5b","5a4a","S*3a","4a5b","P*4b","5b5a","S*6a","L*9h","6a5b+","5a5b","N*5c","9h9i+"]),
    (("Saddle_Ply_125_7f7c", "Saddle_Ply_130_P*6g"), ["9f9g","2c1c","6g7i+","8h9g","P*6g"]),
    (("Saddle_Ply_110_G*5i", "Saddle_Ply_125_7f7c"), ["5d8a","3e4e","8a4e","P*7f","7g7f","8b2b","2a2b","P*5h","2b2c","P*7g","8i7g","N*6g","7i8h","L*9f","7f7c"]),
    (("Saddle_Ply_100_3a4b", "Saddle_Ply_110_G*5i"), ["3b2a+","4b5a","P*2c","1b2c","P*2b","2f3e","P*7h","P*7g","6g7g","G*5i"]),
    (("Saddle_Ply_80_P*6g", "Saddle_Ply_100_3a4b"), ["5h6g","4i6g+","6h6g","3h4h","S*3a","4h2f","B*2a","S*5i","2a4c+","4a3a","4c5d","P*6h","7i6h","S*7i","3f3e","1a1b","6i7i","5i4h+","S*3b","3a4b"]),
    (("Saddle_Ply_75_6i7i", "Saddle_Ply_80_P*6g"), ["B*4i","1d1c+","2a1c","5i6i","P*6g"]),
    (("Saddle_Ply_70_2b1a", "Saddle_Ply_75_6i7i"), ["4d4a+","3b4a","P*1d","S*7i","6i7i"]),
    (("Saddle_Ply_50_5a4a", "Saddle_Ply_70_2b1a"), ["1c2c","1f1g+","B*3a","4b3a","5i4i","G*3i","4i5i","B*4i","2c2b","3a2b","S*4b","4a3b","2g2f","3b4b","L*4d","4b3b","P*1b","4i3h+","1b1a+","2b1a"]),
    (("Saddle_Ply_30_2b7g+", "Saddle_Ply_50_5a4a"), ["1g1f","7g6h","5h6h","1e1f","1i1h","1c1d","1h1g","1d1e","7d7c+","1f1g+","2i1g","6b7c","1g2e","L*5h","4i5h","1e1f","2e1c+","S*4i","G*4a","5a4a"]),
    (("Saddle_Ply_25_4d3c+", "Saddle_Ply_30_2b7g+"), ["5b4b","3c4b","3a4b","7e7d","2b7g+"]),
    (("Start", "Saddle_Ply_25_4d3c+"), ["2h5h","1a1b","3i4h","1c1d","6g6f","1b1c","7g7f","1d1e","8h7g","7a6b","7i6h","4a5b","4g4f","6c6d","7f7e","4c4d","3g3f","6d6e","6f6e","4d4e","7g4d","5c5d","4f4e","7c7d","4d3c+"])
  ]
