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

-- | Node ID: Minimum_Ply_512_Equilibrium_Convergence (Type: Saddle, Potential: 3721.596423799717)
-- | Node ID: Saddle_Ply_100_2b2c (Type: Saddle, Potential: -1438.4186919302556)
-- | Node ID: Saddle_Ply_104_3d3e (Type: Saddle, Potential: -600.8701542733518)
-- | Node ID: Saddle_Ply_120_2c2b (Type: Saddle, Potential: -2878.8420780292627)
-- | Node ID: Saddle_Ply_140_2c2b (Type: Saddle, Potential: -2965.347916132904)
-- | Node ID: Saddle_Ply_160_1a2a (Type: Saddle, Potential: -2440.6203097466864)
-- | Node ID: Saddle_Ply_168_5c6d (Type: Saddle, Potential: -2089.3170593275536)
-- | Node ID: Saddle_Ply_176_1a2a (Type: Saddle, Potential: -2099.0080454677554)
-- | Node ID: Saddle_Ply_180_1a2a (Type: Saddle, Potential: -2099.0080454677554)
-- | Node ID: Saddle_Ply_192_1a2a (Type: Saddle, Potential: -2418.7892591648138)
-- | Node ID: Saddle_Ply_200_2b1c (Type: Saddle, Potential: -3491.1355182382513)
-- | Node ID: Saddle_Ply_20_4d3e (Type: Saddle, Potential: 0.0)
-- | Node ID: Saddle_Ply_220_4g4h (Type: Saddle, Potential: -1843.5099764564748)
-- | Node ID: Saddle_Ply_240_1a1b (Type: Saddle, Potential: -3858.765089103621)
-- | Node ID: Saddle_Ply_248_5i5h (Type: Saddle, Potential: -5027.832552833479)
-- | Node ID: Saddle_Ply_260_1a1b (Type: Saddle, Potential: -4638.701362481594)
-- | Node ID: Saddle_Ply_280_5h5i (Type: Saddle, Potential: -4579.978174964821)
-- | Node ID: Saddle_Ply_300_1d1e (Type: Saddle, Potential: -5069.794820760618)
-- | Node ID: Saddle_Ply_320_1b1a (Type: Saddle, Potential: -5361.786362238799)
-- | Node ID: Saddle_Ply_340_1b1a (Type: Saddle, Potential: -5053.5770055655485)
-- | Node ID: Saddle_Ply_360_L*3a (Type: Saddle, Potential: -5384.156130558113)
-- | Node ID: Saddle_Ply_380_P*1b (Type: Saddle, Potential: -3917.2094493829154)
-- | Node ID: Saddle_Ply_400_1a1b (Type: Saddle, Potential: -7273.970223523225)
-- | Node ID: Saddle_Ply_40_5c5d (Type: Saddle, Potential: 0.0)
-- | Node ID: Saddle_Ply_420_1a1b (Type: Saddle, Potential: -6998.769567861987)
-- | Node ID: Saddle_Ply_440_1a1b (Type: Saddle, Potential: -7552.381654555749)
-- | Node ID: Saddle_Ply_460_1b1a (Type: Saddle, Potential: -8349.689775150782)
-- | Node ID: Saddle_Ply_480_1b1a (Type: Saddle, Potential: -8346.629437818145)
-- | Node ID: Saddle_Ply_48_2c2d (Type: Saddle, Potential: -148.7567460828298)
-- | Node ID: Saddle_Ply_500_1b1a (Type: Saddle, Potential: -8719.410846677985)
-- | Node ID: Saddle_Ply_56_2c1c (Type: Saddle, Potential: -891.0961290013973)
-- | Node ID: Saddle_Ply_60_2c1c (Type: Saddle, Potential: -895.8716004435355)
-- | Node ID: Saddle_Ply_64_1c2c (Type: Saddle, Potential: -429.6066598051126)
-- | Node ID: Saddle_Ply_80_2f3g (Type: Saddle, Potential: -394.4245467247756)
-- | Node ID: Start (Type: Saddle, Potential: 0.0)

-- ============================================================================
-- 2. 勾配流（V-Path）の型レベル二項遷移関係の定義（Type Family）
--    アクティブ・ラーニングで収束が確認されたパスのみが 'True と判定されます。
-- ============================================================================
type family ValidTransition (from :: Symbol) (to :: Symbol) :: Bool where
  ValidTransition "Saddle_Ply_500_1b1a" "Minimum_Ply_512_Equilibrium_Convergence" = 'True
  ValidTransition "Saddle_Ply_480_1b1a" "Saddle_Ply_500_1b1a" = 'True
  ValidTransition "Saddle_Ply_460_1b1a" "Saddle_Ply_480_1b1a" = 'True
  ValidTransition "Saddle_Ply_440_1a1b" "Saddle_Ply_460_1b1a" = 'True
  ValidTransition "Saddle_Ply_420_1a1b" "Saddle_Ply_440_1a1b" = 'True
  ValidTransition "Saddle_Ply_400_1a1b" "Saddle_Ply_420_1a1b" = 'True
  ValidTransition "Saddle_Ply_380_P*1b" "Saddle_Ply_400_1a1b" = 'True
  ValidTransition "Saddle_Ply_360_L*3a" "Saddle_Ply_380_P*1b" = 'True
  ValidTransition "Saddle_Ply_340_1b1a" "Saddle_Ply_360_L*3a" = 'True
  ValidTransition "Saddle_Ply_320_1b1a" "Saddle_Ply_340_1b1a" = 'True
  ValidTransition "Saddle_Ply_300_1d1e" "Saddle_Ply_320_1b1a" = 'True
  ValidTransition "Saddle_Ply_280_5h5i" "Saddle_Ply_300_1d1e" = 'True
  ValidTransition "Saddle_Ply_260_1a1b" "Saddle_Ply_280_5h5i" = 'True
  ValidTransition "Saddle_Ply_248_5i5h" "Saddle_Ply_260_1a1b" = 'True
  ValidTransition "Saddle_Ply_240_1a1b" "Saddle_Ply_248_5i5h" = 'True
  ValidTransition "Saddle_Ply_220_4g4h" "Saddle_Ply_240_1a1b" = 'True
  ValidTransition "Saddle_Ply_200_2b1c" "Saddle_Ply_220_4g4h" = 'True
  ValidTransition "Saddle_Ply_192_1a2a" "Saddle_Ply_200_2b1c" = 'True
  ValidTransition "Saddle_Ply_180_1a2a" "Saddle_Ply_192_1a2a" = 'True
  ValidTransition "Saddle_Ply_176_1a2a" "Saddle_Ply_180_1a2a" = 'True
  ValidTransition "Saddle_Ply_168_5c6d" "Saddle_Ply_176_1a2a" = 'True
  ValidTransition "Saddle_Ply_160_1a2a" "Saddle_Ply_168_5c6d" = 'True
  ValidTransition "Saddle_Ply_140_2c2b" "Saddle_Ply_160_1a2a" = 'True
  ValidTransition "Saddle_Ply_120_2c2b" "Saddle_Ply_140_2c2b" = 'True
  ValidTransition "Saddle_Ply_104_3d3e" "Saddle_Ply_120_2c2b" = 'True
  ValidTransition "Saddle_Ply_100_2b2c" "Saddle_Ply_104_3d3e" = 'True
  ValidTransition "Saddle_Ply_80_2f3g" "Saddle_Ply_100_2b2c" = 'True
  ValidTransition "Saddle_Ply_64_1c2c" "Saddle_Ply_80_2f3g" = 'True
  ValidTransition "Saddle_Ply_60_2c1c" "Saddle_Ply_64_1c2c" = 'True
  ValidTransition "Saddle_Ply_56_2c1c" "Saddle_Ply_60_2c1c" = 'True
  ValidTransition "Saddle_Ply_48_2c2d" "Saddle_Ply_56_2c1c" = 'True
  ValidTransition "Saddle_Ply_40_5c5d" "Saddle_Ply_48_2c2d" = 'True
  ValidTransition "Saddle_Ply_20_4d3e" "Saddle_Ply_40_5c5d" = 'True
  ValidTransition "Start" "Saddle_Ply_20_4d3e" = 'True
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
    (("Saddle_Ply_500_1b1a", "Minimum_Ply_512_Equilibrium_Convergence"), ["2e2d","1d1e","2d2e","1a1b","2e2f","1b1a","2f2h","1a1b","2h2i","1b1a","2i2h","1e1f"]),
    (("Saddle_Ply_480_1b1a", "Saddle_Ply_500_1b1a"), ["2h2i","1a1b","2i2h","1b1a","2h2i","1a1b","2i2g","1b1a","2g2f","1a1b","2f2g","1b1a","2g2f","1a1b","2f2e","1b1a","2e2d","1a1b","2d2e","1b1a"]),
    (("Saddle_Ply_460_1b1a", "Saddle_Ply_480_1b1a"), ["2g2h","1a1b","2h2i","1b1a","2i2h","1a1b","2h2i","1b1a","2i2h","1a1b","2h2i","1b1a","2i2g","1a1b","2g2h","1b1a","2h2i","1a1b","2i2h","1b1a"]),
    (("Saddle_Ply_440_1a1b", "Saddle_Ply_460_1b1a"), ["2d2e","1b1a","2e2d","1a1b","2d2e","1b1a","2e2d","1a1b","2d2e","1b1a","2e2c","N*2a","2c2d","5h5i","2d2e","1c1d","2e2f","1a1b","2f2g","1b1a"]),
    (("Saddle_Ply_420_1a1b", "Saddle_Ply_440_1a1b"), ["2f1f","1b1a","1f1g","1a1b","1g1f","1b1a","1f1e","1a1b","1e2e","1b1a","2e1e","1a1b","1e2e","1b1a","2e1e","1a1b","1e2e","1b1a","2e3d","1a1b"]),
    (("Saddle_Ply_400_1a1b", "Saddle_Ply_420_1a1b"), ["2e1e","1b1a","1e2e","1a1b","2e1e","1b1a","1e1f","1a1b","1f1g","1b1a","1g1f","1a1b","1f1g","1b1a","1g1f","1a1b","1f1g","1b1a","1g2f","1a1b"]),
    (("Saddle_Ply_380_P*1b", "Saddle_Ply_400_1a1b"), ["1d3d","1b1c","1e1f","1a1b","1f1e","S*1d","1e1f","1d2e","1f1e","1c1d","3d1d","P*1c","1d2d","1b1a","1e2e","1a1b","2e1e","1b1a","1e2e","1a1b"]),
    (("Saddle_Ply_360_L*3a", "Saddle_Ply_380_P*1b"), ["2h1h","1a1b","1h2g","1b1a","2g1h","1a1b","1h2g","1b1a","2g1h","1a1b","1h2g","1b1a","2g2f","1a1b","2f1f","1b1a","1f1e","3a3d","R*1d","P*1b"]),
    (("Saddle_Ply_340_1b1a", "Saddle_Ply_360_L*3a"), ["1h2h","1a1b","2h2g","1b1a","2g1h","1a1b","1h2g","1b1a","2g1h","1a1b","1h2g","1b1a","2g1h","1a1b","1h2h","1b1a","3e3d","L*2c","P*2e","L*3a"]),
    (("Saddle_Ply_320_1b1a", "Saddle_Ply_340_1b1a"), ["2g2f","1a1b","2f2g","1b1a","2g2f","1a1b","3f3e","1b1a","2f2g","1a1b","2g1h","1b1a","1h2g","1a1b","2g1h","1b1a","1h2g","1a1b","2g1h","1b1a"]),
    (("Saddle_Ply_300_1d1e", "Saddle_Ply_320_1b1a"), ["1g1h","1e1f","1h2h","2a1a","2h1h","1a1b","1h2h","1b1a","2h1h","1a1b","1h2h","1b1a","2h1h","1a1b","1h2g","1b1a","2g2f","1a1b","2f2g","1b1a"]),
    (("Saddle_Ply_280_5h5i", "Saddle_Ply_300_1d1e"), ["2d1d","5i5h","1d1e","1b1c","1e1f","1a1b","1f1e","1b1a","1e1f","1a1b","1f1e","1b1a","1e1f","1a1b","1f1e","1b2a","1e1f","1c1d","1f1g","1d1e"]),
    (("Saddle_Ply_260_1a1b", "Saddle_Ply_280_5h5i"), ["1f1e","1b2a","1e2d","2a1a","2d1e","1a1b","1e1f","1b1a","1f1e","1a1b","1e1f","1b1a","1f1e","1a1b","1e2d","1b1a","2d1c","P*1b","1c2d","5h5i"]),
    (("Saddle_Ply_248_5i5h", "Saddle_Ply_260_1a1b"), ["1d1e","1b1c","1e1f","1a1b","1f1e","1b1a","1e1f","1a1b","1f1e","1b1a","1e1f","1a1b"]),
    (("Saddle_Ply_240_1a1b", "Saddle_Ply_248_5i5h"), ["1f1e","1b2a","1e2d","2a1a","2d1c","P*1b","1c1d","5i5h"]),
    (("Saddle_Ply_220_4g4h", "Saddle_Ply_240_1a1b"), ["N*2c","2b2c","1c2c","P*2b","2c1c","P*1b","1c1d","4h5i","1d1e","1b1c","1e1f","1a1b","1f1e","1b1a","1e1f","1a1b","1f1e","1b1a","1e1f","1a1b"]),
    (("Saddle_Ply_200_2b1c", "Saddle_Ply_220_4g4h"), ["2e1e","1c1b","1e1d","1b1a","1d1c","9b2b","L*1b","2b1b","1c2c","1b2b","2c1c","2b1b","1c2c","1b2b","2c1c","2b1b","1c2c","1b2b","2c1c","4g4h"]),
    (("Saddle_Ply_192_1a2a", "Saddle_Ply_200_2b1c"), ["2d2c","2a1a","2c2b","4f4g+","3c4c","1a2b","3d2e","2b1c"]),
    (("Saddle_Ply_180_1a2a", "Saddle_Ply_192_1a2a"), ["1e1d","2a3a","1d2d","3a2a","2d3d","2a1a","2e3c+","1a2a","2f2e","2a1a","2e2d","1a2a"]),
    (("Saddle_Ply_176_1a2a", "Saddle_Ply_180_1a2a"), ["1e1d","2a1a","1d1e","1a2a"]),
    (("Saddle_Ply_168_5c6d", "Saddle_Ply_176_1a2a"), ["1e1d","5i6i","1d1e","1a2a","1e1d","2a1a","1d1e","1a2a"]),
    (("Saddle_Ply_160_1a2a", "Saddle_Ply_168_5c6d"), ["1e1d","2a1a","1d1e","1a2a","1e1d","2a1a","1d1e","5c6d"]),
    (("Saddle_Ply_140_2c2b", "Saddle_Ply_160_1a2a"), ["1d1c","3h3i","1e1d","3i4i","1d2d","4d4e","2d1d","4e4f","S*3f","4i5i","1d1e","5b4a","1e1d","4a3a","1d1e","3a2a","1e1d","2a1a","1d1e","1a2a"]),
    (("Saddle_Ply_120_2c2b", "Saddle_Ply_140_2c2b"), ["5h4g","2b2c","4g3f","2c2b","3f2g","2b2c","2g1f","2c2b","1f1e","2b2c","1e1f","2c2b","1f1e","2b2c","1e1f","2c2b","1f1e","2b2c","P*1d","2c2b"]),
    (("Saddle_Ply_104_3d3e", "Saddle_Ply_120_2c2b"), ["4g4f","3e3f","2g2f","3f3g+","3h4g","2b2c","5f6f","5a5b","4g5f","2c2b","4f4e","3g3h","5f6e","2b2c","5g5f","2c2b"]),
    (("Saddle_Ply_100_2b2c", "Saddle_Ply_104_3d3e"), ["3f4f","2c2b","4f5f","3d3e"]),
    (("Saddle_Ply_80_2f3g", "Saddle_Ply_100_2b2c"), ["2i3g","2e2f","P*2h","4d5c","5h4h","4c4d","4h5h","3e3f","3g2e","G*2b","3h3f","2b2c","3f3h","2c2b","3h3f","2b2c","3i3h","2c2b","2h2g","2b2c"]),
    (("Saddle_Ply_64_1c2c", "Saddle_Ply_80_2f3g"), ["2g2h","1b1c","2h2g","1c1d","2g2h","1d1e","2h2g","1e2f","2g2h","2c2d","3g3f","2d3e","3h3g","3c4e","2h3h","2f3g"]),
    (("Saddle_Ply_60_2c1c", "Saddle_Ply_64_1c2c"), ["2f2e","2d2e","2h2g","1c2c"]),
    (("Saddle_Ply_56_2c1c", "Saddle_Ply_60_2c1c"), ["1f1e","1c2c","2g2f","2c1c"]),
    (("Saddle_Ply_48_2c2d", "Saddle_Ply_56_2c1c"), ["1d1c","2b2c","1c1b","2c1b","1h1g","3b2c","1g1f","2c1c"]),
    (("Saddle_Ply_40_5c5d", "Saddle_Ply_48_2c2d"), ["1i1h","1c1d","1g1f","1a1c","1f1e","2a3c","1e1d","2c2d"]),
    (("Saddle_Ply_20_4d3e", "Saddle_Ply_40_5c5d"), ["4h5h","3e4d","5h4h","4d3e","4h5h","4a3b","5h4h","3a2b","4h5h","8b9b","4i3h","8c8d","5h4h","2b4d","4h5h","3c3d","5i4h","5d5e","6i7h","5c5d"]),
    (("Start", "Saddle_Ply_20_4d3e"), ["4h5h","3e4d","5h4h","4d3e","4h5h","3e4d","5h4h","4d3e","4h5h","3e4d","5h4h","4d3e","4h5h","3e4d","5h4h","4d3e","4h5h","3e4d","5h4h","4d3e"])
  ]
