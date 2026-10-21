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

-- | Node ID: Minimum_Ply_332_TonshiFree_Convergence (Type: Saddle, Potential: 8335.0)
-- | Node ID: Saddle_Ply_100_5a5b (Type: Saddle, Potential: 3468.0755928752214)
-- | Node ID: Saddle_Ply_104_2f2g+ (Type: Saddle, Potential: 3956.1082673075643)
-- | Node ID: Saddle_Ply_120_8d8e (Type: Saddle, Potential: 5757.781945752314)
-- | Node ID: Saddle_Ply_140_7f7g+ (Type: Saddle, Potential: 5323.75915793987)
-- | Node ID: Saddle_Ply_160_7c6d (Type: Saddle, Potential: 3357.234413775105)
-- | Node ID: Saddle_Ply_180_3f2g (Type: Saddle, Potential: 2637.5944802132785)
-- | Node ID: Saddle_Ply_192_2h2i (Type: Saddle, Potential: 2661.9388306363853)
-- | Node ID: Saddle_Ply_200_3h5f (Type: Saddle, Potential: 1478.6422090056303)
-- | Node ID: Saddle_Ply_208_2g2h+ (Type: Saddle, Potential: 2015.021352893506)
-- | Node ID: Saddle_Ply_20_2c2d (Type: Saddle, Potential: 249.99999999999994)
-- | Node ID: Saddle_Ply_216_2c3c (Type: Saddle, Potential: 5177.368745091641)
-- | Node ID: Saddle_Ply_220_R*4e (Type: Saddle, Potential: 6109.144711061155)
-- | Node ID: Saddle_Ply_240_7h6i+ (Type: Saddle, Potential: 7290.14175754086)
-- | Node ID: Saddle_Ply_260_L*9b (Type: Saddle, Potential: 7438.8124786004855)
-- | Node ID: Saddle_Ply_280_4h5h (Type: Saddle, Potential: 7723.733444428873)
-- | Node ID: Saddle_Ply_300_4e4d (Type: Saddle, Potential: 8500.040041477976)
-- | Node ID: Saddle_Ply_304_4b3b (Type: Saddle, Potential: 10173.110901017038)
-- | Node ID: Saddle_Ply_320_5c6c (Type: Saddle, Potential: 12612.971421424205)
-- | Node ID: Saddle_Ply_40_8f8g+ (Type: Saddle, Potential: -1940.2889948589154)
-- | Node ID: Saddle_Ply_48_5g6f+ (Type: Saddle, Potential: 538.7404861700172)
-- | Node ID: Saddle_Ply_60_B*6h (Type: Saddle, Potential: 1640.0455710924346)
-- | Node ID: Saddle_Ply_64_3d4e (Type: Saddle, Potential: -642.315676922539)
-- | Node ID: Saddle_Ply_80_8i3i+ (Type: Saddle, Potential: 2647.5462168626905)
-- | Node ID: Saddle_Ply_88_2i4i (Type: Saddle, Potential: 5135.184538595254)
-- | Node ID: Start (Type: Saddle, Potential: 0.0)

-- ============================================================================
-- 2. 勾配流（V-Path）の型レベル二項遷移関係の定義（Type Family）
--    アクティブ・ラーニングで収束が確認されたパスのみが 'True と判定されます。
-- ============================================================================
type family ValidTransition (from :: Symbol) (to :: Symbol) :: Bool where
  ValidTransition "Saddle_Ply_320_5c6c" "Minimum_Ply_332_TonshiFree_Convergence" = 'True
  ValidTransition "Saddle_Ply_304_4b3b" "Saddle_Ply_320_5c6c" = 'True
  ValidTransition "Saddle_Ply_300_4e4d" "Saddle_Ply_304_4b3b" = 'True
  ValidTransition "Saddle_Ply_280_4h5h" "Saddle_Ply_300_4e4d" = 'True
  ValidTransition "Saddle_Ply_260_L*9b" "Saddle_Ply_280_4h5h" = 'True
  ValidTransition "Saddle_Ply_240_7h6i+" "Saddle_Ply_260_L*9b" = 'True
  ValidTransition "Saddle_Ply_220_R*4e" "Saddle_Ply_240_7h6i+" = 'True
  ValidTransition "Saddle_Ply_216_2c3c" "Saddle_Ply_220_R*4e" = 'True
  ValidTransition "Saddle_Ply_208_2g2h+" "Saddle_Ply_216_2c3c" = 'True
  ValidTransition "Saddle_Ply_200_3h5f" "Saddle_Ply_208_2g2h+" = 'True
  ValidTransition "Saddle_Ply_192_2h2i" "Saddle_Ply_200_3h5f" = 'True
  ValidTransition "Saddle_Ply_180_3f2g" "Saddle_Ply_192_2h2i" = 'True
  ValidTransition "Saddle_Ply_160_7c6d" "Saddle_Ply_180_3f2g" = 'True
  ValidTransition "Saddle_Ply_140_7f7g+" "Saddle_Ply_160_7c6d" = 'True
  ValidTransition "Saddle_Ply_120_8d8e" "Saddle_Ply_140_7f7g+" = 'True
  ValidTransition "Saddle_Ply_104_2f2g+" "Saddle_Ply_120_8d8e" = 'True
  ValidTransition "Saddle_Ply_100_5a5b" "Saddle_Ply_104_2f2g+" = 'True
  ValidTransition "Saddle_Ply_88_2i4i" "Saddle_Ply_100_5a5b" = 'True
  ValidTransition "Saddle_Ply_80_8i3i+" "Saddle_Ply_88_2i4i" = 'True
  ValidTransition "Saddle_Ply_64_3d4e" "Saddle_Ply_80_8i3i+" = 'True
  ValidTransition "Saddle_Ply_60_B*6h" "Saddle_Ply_64_3d4e" = 'True
  ValidTransition "Saddle_Ply_48_5g6f+" "Saddle_Ply_60_B*6h" = 'True
  ValidTransition "Saddle_Ply_40_8f8g+" "Saddle_Ply_48_5g6f+" = 'True
  ValidTransition "Saddle_Ply_20_2c2d" "Saddle_Ply_40_8f8g+" = 'True
  ValidTransition "Start" "Saddle_Ply_20_2c2d" = 'True
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
    (("Saddle_Ply_320_5c6c", "Minimum_Ply_332_TonshiFree_Convergence"), ["1b1a","4a4g+","1a1b","N*9b","1b1a","5a6a","1a1b","6a7a","1b2b","6b7b","2b1a","7a8a"]),
    (("Saddle_Ply_304_4b3b", "Saddle_Ply_320_5c6c"), ["7b6b","L*9e","1a1b","L*4a","1b1a","3b4b","1a1b","4c5c","1b1a","4b5a","1a1b","5c6b","1b1a","G*5c","1a1b","5c6c"]),
    (("Saddle_Ply_300_4e4d", "Saddle_Ply_304_4b3b"), ["2b1a","4d5d","6b7b","4b3b"]),
    (("Saddle_Ply_280_4h5h", "Saddle_Ply_300_4e4d"), ["1i1h","4g4h+","1h1g","4d4g+","L*5d","2f1g","5c6c","4c4b","5d5c+","P*9d","5c5d","4g4c","9b8b","4h4g","8b7b","4g4f","7b7c","4f4e","7c7d","4e4d"]),
    (("Saddle_Ply_260_L*9b", "Saddle_Ply_280_4h5h"), ["9g9f","2h2g","9f9e","G*1f","P*6e","1f2f","9e8d","P*9a","9d9c+","4c4d","8d8c","4d4c","9c9b","4e4d","8c8b","5g5f","8b9a","5h5g","P*1i","4h5h"]),
    (("Saddle_Ply_240_7h6i+", "Saddle_Ply_260_L*9b"), ["7b6b","7i7h","6c5c","5h6h","1a1b","4f5h+","1b2b","6i7i","6d5d","5i6i","5d4c","4b4c","7d6d","L*9e","9f9e","4i5i","L*2h","2i2h","9e9d","L*9b"]),
    (("Saddle_Ply_220_R*4e", "Saddle_Ply_240_7h6i+"), ["N*3h","2h3h","3g2g","S*7h","P*6f","6e6f","8d7d","P*9d","7b7c","P*3g","8b7b","N*4f","1a1b","4f5h+","1b2b","N*4f","7c6c","6i5i","2b1a","7h6i+"]),
    (("Saddle_Ply_216_2c3c", "Saddle_Ply_220_R*4e"), ["9c9d","3c4b","9d8d","R*4e"]),
    (("Saddle_Ply_208_2g2h+", "Saddle_Ply_216_2c3c"), ["1b1a+","P*2g","4c5c","5f4e","5c3c","4e2c","3f3g","2c3c"]),
    (("Saddle_Ply_200_3h5f", "Saddle_Ply_208_2g2h+"), ["G*3f","5h6i","4d5d","P*4g","5d6d","P*2g","1a2a","2g2h+"]),
    (("Saddle_Ply_192_2h2i", "Saddle_Ply_200_3h5f"), ["3c3d","2i2h","3d4d","2h2i","5d4e","4g5h","P*5f","3h5f"]),
    (("Saddle_Ply_180_3f2g", "Saddle_Ply_192_2h2i"), ["9b9c","2g3h","P*2f","3e3f","2c3c","3g4h","2a2b","3f3g+","5b4b","3g2h","2b2c","2h2i"]),
    (("Saddle_Ply_160_7c6d", "Saddle_Ply_180_3f2g"), ["8a7b","L*9h","8f7e","6d5e","6a6b","5e4f","6b5b","5d5e","2c4c","6c7b","G*8b","4f4g","7a7b","R*7h","2b2c","7h7i+","S*5d","3d4e","G*8f","3f2g"]),
    (("Saddle_Ply_140_7f7g+", "Saddle_Ply_160_7c6d"), ["8b9b","L*8g","R*8b","P*7f","8b8a+","N*7a","P*6d","N*9d","L*6c","7b6c","N*8c","2g2h","8c7a+","6a6b","7a6a","6b7c","N*8c","2h3i","8c7a+","7c6d"]),
    (("Saddle_Ply_120_8d8e", "Saddle_Ply_140_7f7g+"), ["9g9f","8e8f","8g9g","8f8g+","7f8g","P*8f","8g8f","P*8h","P*5b","6a5b","P*1b","3c4c","7e7d","6b6a","7d7c+","P*7f","6d7b+","7a7b","7c8b","7f7g+"]),
    (("Saddle_Ply_104_2f2g+", "Saddle_Ply_120_8d8e"), ["8f9e","5f5g","6h6g","6f6g","9e8f","4e4f","N*6d","5b6b","1b1a","4f3g+","2b2c","G*3c","1c2b","P*8d","8f8g","8d8e"]),
    (("Saddle_Ply_100_5a5b", "Saddle_Ply_104_2f2g+"), ["1b2b+","4g3f+","1a1b","2f2g+"]),
    (("Saddle_Ply_88_2i4i", "Saddle_Ply_100_5a5b"), ["5h4i","3h4i+","R*1b","B*4g","6i7h","N*8f","7h8g","N*9e","8g8f","2e2f","N*4c","5a5b"]),
    (("Saddle_Ply_80_8i3i+", "Saddle_Ply_88_2i4i"), ["N*4i","3i2i","7h7i","G*8i","7i6i","S*3h","1b1a","2i4i"]),
    (("Saddle_Ply_64_3d4e", "Saddle_Ply_80_8i3i+"), ["B*5h","4c3d","1h1c+","P*2e","L*7g","5g5f","L*3b","4g3i+","2h2e","P*2d","P*5g","2d2e","4i3i","R*8i","6i7h","8i3i+"]),
    (("Saddle_Ply_60_B*6h", "Saddle_Ply_64_3d4e"), ["5h6h","N*4g","5i6i","3d4e"]),
    (("Saddle_Ply_48_5g6f+", "Saddle_Ply_60_B*6h"), ["1f1e","6d6e","1e1d","4c5c","P*8e","5c4c","1d1c+","4e5g+","1c1b","L*5h","6h5h","B*6h"]),
    (("Saddle_Ply_40_8f8g+", "Saddle_Ply_48_5g6f+"), ["7h7g","8g8h","7g7f","B*5g","5h6h","8h9i","1g1f","5g6f+"]),
    (("Saddle_Ply_20_2c2d", "Saddle_Ply_40_8f8g+"), ["2f2e","8c8d","2e2d","3a3b","2d2c+","3b3c","2c2b","3c3d","B*3a","5b4c","3a5c+","8d8e","2b3a","1a1b","P*2b","1c1d","2b2a+","8e8f","7i7h","8f8g+"]),
    (("Start", "Saddle_Ply_20_2c2d"), ["1i1h","9a9b","5g5f","5c5d","6i5h","4a5b","4g4f","4c4d","6g6f","6c6d","7g7f","3c3d","8i7g","2a3c","7g6e","3c4e","7f7e","3d3e","2g2f","2c2d"])
  ]
