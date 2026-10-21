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

-- | Node ID: Minimum_Ply_512_Convergence (Type: Saddle, Potential: 5045.853056367876)
-- | Node ID: Saddle_Ply_100_5d5e (Type: Saddle, Potential: -7231.308667486713)
-- | Node ID: Saddle_Ply_10_3a4b (Type: Saddle, Potential: 16.491940350466514)
-- | Node ID: Saddle_Ply_125_P*1d (Type: Saddle, Potential: 6849.396235883353)
-- | Node ID: Saddle_Ply_150_1c2b (Type: Saddle, Potential: 991.4017632854589)
-- | Node ID: Saddle_Ply_175_8f7g (Type: Saddle, Potential: 837.0058753671494)
-- | Node ID: Saddle_Ply_200_2b2c (Type: Saddle, Potential: -2171.1959522553816)
-- | Node ID: Saddle_Ply_225_N*1e (Type: Saddle, Potential: -571.247595692333)
-- | Node ID: Saddle_Ply_230_2b3a (Type: Saddle, Potential: 3157.2914767029547)
-- | Node ID: Saddle_Ply_250_P*7f (Type: Saddle, Potential: -3102.1003177332937)
-- | Node ID: Saddle_Ply_25_5h5g (Type: Saddle, Potential: 1375.4580206235544)
-- | Node ID: Saddle_Ply_260_L*9g (Type: Saddle, Potential: -2296.6329196133593)
-- | Node ID: Saddle_Ply_270_2e1f (Type: Saddle, Potential: -457.8209909209556)
-- | Node ID: Saddle_Ply_275_P*1g (Type: Saddle, Potential: 563.0245035770229)
-- | Node ID: Saddle_Ply_300_1i2i (Type: Saddle, Potential: 2796.2735929428663)
-- | Node ID: Saddle_Ply_30_4e5g (Type: Saddle, Potential: 1669.836794267316)
-- | Node ID: Saddle_Ply_325_9b9c (Type: Saddle, Potential: -3924.4982507635946)
-- | Node ID: Saddle_Ply_350_8g7h (Type: Saddle, Potential: -2371.165899547632)
-- | Node ID: Saddle_Ply_360_B*8c (Type: Saddle, Potential: -3769.0296556039225)
-- | Node ID: Saddle_Ply_375_9c9d (Type: Saddle, Potential: 2998.704694744705)
-- | Node ID: Saddle_Ply_400_4h4i (Type: Saddle, Potential: 1508.1887336160057)
-- | Node ID: Saddle_Ply_425_9d8c (Type: Saddle, Potential: -1580.125336758681)
-- | Node ID: Saddle_Ply_450_6f6g (Type: Saddle, Potential: -851.0745646791378)
-- | Node ID: Saddle_Ply_475_6c7c (Type: Saddle, Potential: 4787.605192787881)
-- | Node ID: Saddle_Ply_490_3b3c (Type: Saddle, Potential: -5051.196057896047)
-- | Node ID: Saddle_Ply_500_2b2c (Type: Saddle, Potential: -4309.978453319069)
-- | Node ID: Saddle_Ply_50_4a3a (Type: Saddle, Potential: 2249.502414949751)
-- | Node ID: Saddle_Ply_75_5i6i (Type: Saddle, Potential: 1028.1526571676704)
-- | Node ID: Start (Type: Saddle, Potential: 0.0)

-- ============================================================================
-- 2. 勾配流（V-Path）の型レベル二項遷移関係の定義（Type Family）
--    アクティブ・ラーニングで収束が確認されたパスのみが 'True と判定されます。
-- ============================================================================
type family ValidTransition (from :: Symbol) (to :: Symbol) :: Bool where
  ValidTransition "Saddle_Ply_500_2b2c" "Minimum_Ply_512_Convergence" = 'True
  ValidTransition "Saddle_Ply_490_3b3c" "Saddle_Ply_500_2b2c" = 'True
  ValidTransition "Saddle_Ply_475_6c7c" "Saddle_Ply_490_3b3c" = 'True
  ValidTransition "Saddle_Ply_450_6f6g" "Saddle_Ply_475_6c7c" = 'True
  ValidTransition "Saddle_Ply_425_9d8c" "Saddle_Ply_450_6f6g" = 'True
  ValidTransition "Saddle_Ply_400_4h4i" "Saddle_Ply_425_9d8c" = 'True
  ValidTransition "Saddle_Ply_375_9c9d" "Saddle_Ply_400_4h4i" = 'True
  ValidTransition "Saddle_Ply_360_B*8c" "Saddle_Ply_375_9c9d" = 'True
  ValidTransition "Saddle_Ply_350_8g7h" "Saddle_Ply_360_B*8c" = 'True
  ValidTransition "Saddle_Ply_325_9b9c" "Saddle_Ply_350_8g7h" = 'True
  ValidTransition "Saddle_Ply_300_1i2i" "Saddle_Ply_325_9b9c" = 'True
  ValidTransition "Saddle_Ply_275_P*1g" "Saddle_Ply_300_1i2i" = 'True
  ValidTransition "Saddle_Ply_270_2e1f" "Saddle_Ply_275_P*1g" = 'True
  ValidTransition "Saddle_Ply_260_L*9g" "Saddle_Ply_270_2e1f" = 'True
  ValidTransition "Saddle_Ply_250_P*7f" "Saddle_Ply_260_L*9g" = 'True
  ValidTransition "Saddle_Ply_230_2b3a" "Saddle_Ply_250_P*7f" = 'True
  ValidTransition "Saddle_Ply_225_N*1e" "Saddle_Ply_230_2b3a" = 'True
  ValidTransition "Saddle_Ply_200_2b2c" "Saddle_Ply_225_N*1e" = 'True
  ValidTransition "Saddle_Ply_175_8f7g" "Saddle_Ply_200_2b2c" = 'True
  ValidTransition "Saddle_Ply_150_1c2b" "Saddle_Ply_175_8f7g" = 'True
  ValidTransition "Saddle_Ply_125_P*1d" "Saddle_Ply_150_1c2b" = 'True
  ValidTransition "Saddle_Ply_100_5d5e" "Saddle_Ply_125_P*1d" = 'True
  ValidTransition "Saddle_Ply_75_5i6i" "Saddle_Ply_100_5d5e" = 'True
  ValidTransition "Saddle_Ply_50_4a3a" "Saddle_Ply_75_5i6i" = 'True
  ValidTransition "Saddle_Ply_30_4e5g" "Saddle_Ply_50_4a3a" = 'True
  ValidTransition "Saddle_Ply_25_5h5g" "Saddle_Ply_30_4e5g" = 'True
  ValidTransition "Saddle_Ply_10_3a4b" "Saddle_Ply_25_5h5g" = 'True
  ValidTransition "Start" "Saddle_Ply_10_3a4b" = 'True
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
    (("Saddle_Ply_500_2b2c", "Minimum_Ply_512_Convergence"), ["1b1a+","L*9e","P*2d","2c3b","S*4b","P*8d","9d8d","L*8e","8d8e","N*8f","8e8d","8f9h+"]),
    (("Saddle_Ply_490_3b3c", "Saddle_Ply_500_2b2c"), ["9e9f","9g9f","N*2c","3c3b","P*2b","3a2b","1c2b","3b2b","P*1b","2b2c"]),
    (("Saddle_Ply_475_6c7c", "Saddle_Ply_490_3b3c"), ["N*9e","9d9e","S*9d","9c9d","G*9c","8c9c","G*9f","P*5f","5e5d","L*5e","5d4c","5e5c+","4c3b","L*3c","3b3c"]),
    (("Saddle_Ply_450_6f6g", "Saddle_Ply_475_6c7c"), ["P*6f","6g6f","P*5f","6f5f","P*5e","5f6g","P*6f","6g6f","P*6d","P*8c","8b8c","P*9a","9b9a","P*9b","9a9b","6f5e","6d6c+","P*9a","9b9a","N*7c","8c7c","N*8c","7c8c","N*7c","6c7c"]),
    (("Saddle_Ply_425_9d8c", "Saddle_Ply_450_6f6g"), ["P*9d","8c9d","P*8c","9d8c","P*9d","P*5c","L*9e","5c5b+","9e9g+","8c9d","P*8c","9d8c","P*9d","8c9d","P*8c","9d8c","P*9d","8c9d","L*8c","9d8c","L*9d","8c9d","5e6f","P*7f","6f6g"]),
    (("Saddle_Ply_400_4h4i", "Saddle_Ply_425_9d8c"), ["4g3h","4i5h","P*4h","5h6g","P*6h","6g6h","P*6g","6h6g","P*6f","6g6f","P*6e","6f6e","P*6d","6e6d","B*5d","P*9d","5d7b+","6d5e","7b9d","P*8c","9d8c","P*9d","8c9d","P*8c","9d8c"]),
    (("Saddle_Ply_375_9c9d", "Saddle_Ply_400_4h4i"), ["4h5i","P*4i","5i4i","P*4h","2i1i","L*3i","4i3i","P*2i","1i2i","N*4i","3i4i","N*5i","4i5i","N*5h","5i5h","N*5g","5h5g","S*4g","5g4h","S*3h","P*9c","9d9c","2i3h","G*5h","4h4i"]),
    (("Saddle_Ply_360_B*8c", "Saddle_Ply_375_9c9d"), ["7b8b","8c4g+","P*4i","4g2i","L*5h","5i5h","L*4h","P*9b","9a9b","5h4i","4h4c+","P*9d","P*4h","4i4h","9c9d"]),
    (("Saddle_Ply_350_8g7h", "Saddle_Ply_360_B*8c"), ["9c9h","P*8b","9h7h","S*9c","7h6i","G*5i","6i5i","4i5i","9b9c","B*8c"]),
    (("Saddle_Ply_325_9b9c", "Saddle_Ply_350_8g7h"), ["P*9b","9c9b","P*9c","9d9c","P*9d","9e9d","1g2g","3g2g","P*8c","8d8c+","P*9a","9b9a","L*9b","9c9b","L*8b","8c8b","N*9c","9d9c","N*9d","9c9d","N*9c","9d9c","N*7b","8b7b","8g7h"]),
    (("Saddle_Ply_300_1i2i", "Saddle_Ply_325_9b9c"), ["R*3h","P*9d","9e9d","P*8d","8e8d","P*9e","3h5h","9e9f","S*4i","2i3i","P*1i","3i4i","5h9h","9i9h","L*2i","9f9g+","P*7h","P*9e","8d9e","9g8g","P*8d","P*9b","9c9b","P*9c","9b9c"]),
    (("Saddle_Ply_275_P*1g", "Saddle_Ply_300_1i2i"), ["1f1g","P*2g","9g9h+","P*1f","1e1f","G*3g","1g1h","P*1i","1h1i","P*1h","1i1h","P*1i","1h1i","P*1h","1i1h","P*1i","1h1i","P*1h","1i1h","P*1i","1h1i","N*3i","1f1g+","N*2i","1i2i"]),
    (("Saddle_Ply_270_2e1f", "Saddle_Ply_275_P*1g"), ["2d1c+","P*8e","7f8e","L*9g","P*1g"]),
    (("Saddle_Ply_260_L*9g", "Saddle_Ply_270_2e1f"), ["3c2d","1d2d","P*2e","2d1d","9d9c+","9g9i+","S*2d","1d2e","G*3e","2e1f"]),
    (("Saddle_Ply_250_P*7f", "Saddle_Ply_260_L*9g"), ["8f7f","P*9g","9h9g","P*9d","9e9d","P*9e","9f9e","P*9f","9g9f","L*9g"]),
    (("Saddle_Ply_230_2b3a", "Saddle_Ply_250_P*7f"), ["B*1a","N*9f","9g9f","P*9g","9h9g","G*9h","9i9h","N*7g","7e7g","S*9i","8h9i","1c1d","1a3c+","P*9e","9f9e","P*9f","9g9f","P*8f","7g8f","P*7f"]),
    (("Saddle_Ply_225_N*1e", "Saddle_Ply_230_2b3a"), ["1d1e","N*3c","2c3c","N*1d","2b3a"]),
    (("Saddle_Ply_200_2b2c", "Saddle_Ply_225_N*1e"), ["P*2b","2c2b","P*2c","2b2c","L*3b","P*7h","7g7h","S*6h","7h8i","P*8h","8i9h","6h6i+","3b3a+","7i6h+","P*2b","1c2b","L*1c","1b1c","6f8h","6h5h","P*1b","1c1b","L*1c","1b1c","N*1e"]),
    (("Saddle_Ply_175_8f7g", "Saddle_Ply_200_2b2c"), ["L*7e","6d7e","L*8f","7g8f","N*6f","7e6f","N*7e","8f7e","N*7g","7f7g","S*7i","P*1c","2b1c","P*2b","3b2b","P*2c","2b2c","P*2b","2c2b","P*2c","2b2c","P*2b","2c2b","P*2c","2b2c"]),
    (("Saddle_Ply_150_1c2b", "Saddle_Ply_175_8f7g"), ["N*1c","2b1c","G*2b","1c2b","6h6i","P*6h","6i6h","P*7h","6h7h","P*6h","7h6h","P*7h","6h7h","P*7g","7h7g","P*7f","7g7f","P*7g","7f7g","P*7f","7g7f","P*8f","8c8f","L*7g","8f7g"]),
    (("Saddle_Ply_125_P*1d", "Saddle_Ply_150_1c2b"), ["1c1d","P*1c","2b1c","P*2b","1c2b","P*1c","2b1c","P*2b","1c2b","P*1c","2b1c","L*2b","1c2b","L*1c","2b1c","L*2b","1c2b","N*1c","2b1c","G*2b","1c2b","N*1c","2b1c","G*2b","1c2b"]),
    (("Saddle_Ply_100_5d5e", "Saddle_Ply_125_P*1d"), ["5f5e","6c6d","9c8c","3d3e","1g3e","7f6h+","7h6h","4b3a","3e5c+","3a2a","5c6d","3c2b","S*3a","2b3a","S*1a","2a1a","P*1b","1a1b","S*1a","1b1a","P*1b","1a1b","P*2b","3a2b","P*1d"]),
    (("Saddle_Ply_75_5i6i", "Saddle_Ply_100_5d5e"), ["N*7g","6i7h","7g8i+","7h8i","2h2g","5g3i","2g1g","3i1g","P*7h","4e5c+","7h7i+","8i7i","P*7h","8h7h","6b5c","7h7a+","5a4b","7a1a","P*7g","1a9a","N*7f","9a9c","7g7h","7i7h","5d5e"]),
    (("Saddle_Ply_50_4a3a", "Saddle_Ply_75_5i6i"), ["G*7a","6a7a","G*3b","3a3b","3h4i","S*2i","3g4e","2i1h+","7e7d","1i2h+","3i2h","G*5i","4h5i","L*4h","5i4h","1h2h","7d7c+","G*3i","4i5i","S*5g","7c6b","7a6b","4h5g","7i6i","5i6i"]),
    (("Saddle_Ply_30_4e5g", "Saddle_Ply_50_4a3a"), ["4i3h","5g6i+","N*3c","4b3c","5i4h","6i7i","3h2h","S*3h","2h3h","N*3f","3g3f","G*3g","2i3g","G*1h","1i1h","B*1i","N*5c","5b5c","S*3a","4a3a"]),
    (("Saddle_Ply_25_5h5g", "Saddle_Ply_30_4e5g"), ["7c6e","N*5h","6e5g","4h5g","4e5g"]),
    (("Saddle_Ply_10_3a4b", "Saddle_Ply_25_5h5g"), ["B*4h","7a6b","B*5i","5c5d","5g5f","2c2d","8i7g","2a3c","7g6e","3c4e","6e7c","8a7c","7f7e","N*5g","5h5g"]),
    (("Start", "Saddle_Ply_10_3a4b"), ["2h6h","8b5b","4i5h","3c3d","7g7f","2b8h","6h8h","B*4i","5i4i","3a4b"])
  ]
