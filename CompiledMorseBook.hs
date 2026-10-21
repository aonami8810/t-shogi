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

-- | Node ID: Minimum_Ply_512_Convergence (Type: Saddle, Potential: -6072.72395800892)
-- | Node ID: Saddle_Ply_128_B*1i (Type: Saddle, Potential: 6041.33114441923)
-- | Node ID: Saddle_Ply_152_3b2b (Type: Saddle, Potential: 1125.8999114028663)
-- | Node ID: Saddle_Ply_160_6d6c (Type: Saddle, Potential: 6971.594347744998)
-- | Node ID: Saddle_Ply_192_5c5b (Type: Saddle, Potential: -39.503149254214094)
-- | Node ID: Saddle_Ply_208_6a6b (Type: Saddle, Potential: -1405.2295688539107)
-- | Node ID: Saddle_Ply_216_4c5c (Type: Saddle, Potential: 3556.6179263114723)
-- | Node ID: Saddle_Ply_224_S*3i (Type: Saddle, Potential: 5914.758532952678)
-- | Node ID: Saddle_Ply_248_7a7b (Type: Saddle, Potential: -551.2911011253495)
-- | Node ID: Saddle_Ply_256_7a8a (Type: Saddle, Potential: 5248.708898874651)
-- | Node ID: Saddle_Ply_288_8b8a (Type: Saddle, Potential: -3364.1395993043598)
-- | Node ID: Saddle_Ply_304_7a7b (Type: Saddle, Potential: 5845.431500401247)
-- | Node ID: Saddle_Ply_320_R*2i (Type: Saddle, Potential: 3976.009214772807)
-- | Node ID: Saddle_Ply_32_7a6a (Type: Saddle, Potential: 734.2616140118441)
-- | Node ID: Saddle_Ply_352_9b8a (Type: Saddle, Potential: -1614.8454068939373)
-- | Node ID: Saddle_Ply_360_7a8a (Type: Saddle, Potential: 810.8110042638505)
-- | Node ID: Saddle_Ply_368_8a8b (Type: Saddle, Potential: 5731.623873669737)
-- | Node ID: Saddle_Ply_384_S*2i (Type: Saddle, Potential: 10091.959479363022)
-- | Node ID: Saddle_Ply_416_8a7a (Type: Saddle, Potential: -2087.4192113523595)
-- | Node ID: Saddle_Ply_432_9b8c (Type: Saddle, Potential: -2847.3027255361917)
-- | Node ID: Saddle_Ply_448_9a9b (Type: Saddle, Potential: -114.7873419413326)
-- | Node ID: Saddle_Ply_456_8a8b (Type: Saddle, Potential: 2455.604783211916)
-- | Node ID: Saddle_Ply_480_3i2h+ (Type: Saddle, Potential: 804.7907967609216)
-- | Node ID: Saddle_Ply_496_9b9a (Type: Saddle, Potential: 588.1960510495414)
-- | Node ID: Saddle_Ply_504_8a8b (Type: Saddle, Potential: 6791.369608523417)
-- | Node ID: Saddle_Ply_512_B*2i (Type: Saddle, Potential: 5996.814867099829)
-- | Node ID: Saddle_Ply_64_6h6i+ (Type: Saddle, Potential: -3368.664649710624)
-- | Node ID: Saddle_Ply_96_5i2i+ (Type: Saddle, Potential: -1824.8101493539461)
-- | Node ID: Start (Type: Saddle, Potential: 0.0)

-- ============================================================================
-- 2. 勾配流（V-Path）の型レベル二項遷移関係の定義（Type Family）
--    アクティブ・ラーニングで収束が確認されたパスのみが 'True と判定されます。
-- ============================================================================
type family ValidTransition (from :: Symbol) (to :: Symbol) :: Bool where
  ValidTransition "Saddle_Ply_504_8a8b" "Saddle_Ply_512_B*2i" = 'True
  ValidTransition "Saddle_Ply_496_9b9a" "Saddle_Ply_504_8a8b" = 'True
  ValidTransition "Saddle_Ply_480_3i2h+" "Saddle_Ply_496_9b9a" = 'True
  ValidTransition "Saddle_Ply_456_8a8b" "Saddle_Ply_480_3i2h+" = 'True
  ValidTransition "Saddle_Ply_448_9a9b" "Saddle_Ply_456_8a8b" = 'True
  ValidTransition "Saddle_Ply_432_9b8c" "Saddle_Ply_448_9a9b" = 'True
  ValidTransition "Saddle_Ply_416_8a7a" "Saddle_Ply_432_9b8c" = 'True
  ValidTransition "Saddle_Ply_384_S*2i" "Saddle_Ply_416_8a7a" = 'True
  ValidTransition "Saddle_Ply_368_8a8b" "Saddle_Ply_384_S*2i" = 'True
  ValidTransition "Saddle_Ply_360_7a8a" "Saddle_Ply_368_8a8b" = 'True
  ValidTransition "Saddle_Ply_352_9b8a" "Saddle_Ply_360_7a8a" = 'True
  ValidTransition "Saddle_Ply_320_R*2i" "Saddle_Ply_352_9b8a" = 'True
  ValidTransition "Saddle_Ply_304_7a7b" "Saddle_Ply_320_R*2i" = 'True
  ValidTransition "Saddle_Ply_288_8b8a" "Saddle_Ply_304_7a7b" = 'True
  ValidTransition "Saddle_Ply_256_7a8a" "Saddle_Ply_288_8b8a" = 'True
  ValidTransition "Saddle_Ply_248_7a7b" "Saddle_Ply_256_7a8a" = 'True
  ValidTransition "Saddle_Ply_224_S*3i" "Saddle_Ply_248_7a7b" = 'True
  ValidTransition "Saddle_Ply_216_4c5c" "Saddle_Ply_224_S*3i" = 'True
  ValidTransition "Saddle_Ply_208_6a6b" "Saddle_Ply_216_4c5c" = 'True
  ValidTransition "Saddle_Ply_192_5c5b" "Saddle_Ply_208_6a6b" = 'True
  ValidTransition "Saddle_Ply_160_6d6c" "Saddle_Ply_192_5c5b" = 'True
  ValidTransition "Saddle_Ply_152_3b2b" "Saddle_Ply_160_6d6c" = 'True
  ValidTransition "Saddle_Ply_128_B*1i" "Saddle_Ply_152_3b2b" = 'True
  ValidTransition "Saddle_Ply_96_5i2i+" "Saddle_Ply_128_B*1i" = 'True
  ValidTransition "Saddle_Ply_64_6h6i+" "Saddle_Ply_96_5i2i+" = 'True
  ValidTransition "Saddle_Ply_32_7a6a" "Saddle_Ply_64_6h6i+" = 'True
  ValidTransition "Start" "Saddle_Ply_32_7a6a" = 'True
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
    (("Saddle_Ply_504_8a8b", "Saddle_Ply_512_B*2i"), ["L*7b","8b9a","7b7a+","B*2i","1i2i","S*3i","2i1i","B*2i"]),
    (("Saddle_Ply_496_9b9a", "Saddle_Ply_504_8a8b"), ["B*9b","9a9b","B*9a","9b9a","S*8a","9a8a","S*8b","8a8b"]),
    (("Saddle_Ply_480_3i2h+", "Saddle_Ply_496_9b9a"), ["1h2h","R*3i","2i1h","3i1i+","1h1i","S*1h","2h1h","9b9a","R*8a","9a8a","S*9a","8a9a","S*9b","9a9b","R*9a","9b9a"]),
    (("Saddle_Ply_456_8a8b", "Saddle_Ply_480_3i2h+"), ["S*8a","8b8a","S*8b","8a8b","R*9b","8b9b","2i1i","L*1h","2h1h","S*2i","1i2i","B*3i","2i1i","3i1g+","1h1g","R*1h","1g1h","S*2i","1i2i","S*3i","2i1i","B*2i","1i2i","3i2h+"]),
    (("Saddle_Ply_448_9a9b", "Saddle_Ply_456_8a8b"), ["S*8b","9b8b","S*7b","8b9a","7b8a+","9a8a","R*8b","8a8b"]),
    (("Saddle_Ply_432_9b8c", "Saddle_Ply_448_9a9b"), ["S*8b","8c8b","B*7b","S*2i","1i2i","S*2h","1h2h","8b9b","7b8c+","9b8c","B*7c","8c9b","7c9a+","9b9a","L*9b","9a9b"]),
    (("Saddle_Ply_416_8a7a", "Saddle_Ply_432_9b8c"), ["S*7b","7a8b","7b8c+","8b8c","B*8b","8c8b","S*7b","B*1h","2i1h","S*2i","1i2i","B*1i","2i1i","8b9b","7b8c+","9b8c"]),
    (("Saddle_Ply_384_S*2i", "Saddle_Ply_416_8a7a"), ["1i2i","S*3i","2i1i","R*1h","1i1h","3i2h+","2g2h","B*1i","1h1i","B*1h","2h1h","S*2i","1i2i","L*2h","1h2h","S*1i","2i1i","R*2i","2h2i","N*2g","1i1h","2g1i+","1h1i","N*1f","1g1f","P*4c","B*7a","8a7a","L*7b","7a8a","7b7a+","8a7a"]),
    (("Saddle_Ply_368_8a8b", "Saddle_Ply_384_S*2i"), ["N*8c","8b8c","S*8b","8c8b","S*9b","8b9b","P*8b","3f2g","2h2g","9b8b","R*8a","8b8a","N*4e","4d4e","1h1i","S*2i"]),
    (("Saddle_Ply_360_7a8a", "Saddle_Ply_368_8a8b"), ["R*7a","8a7a","S*7b","7a8b","7b8a+","8b8a","L*8b","8a8b"]),
    (("Saddle_Ply_352_9b8a", "Saddle_Ply_360_7a8a"), ["B*7a","8a7a","B*7b","R*3i","2i1h","3i3f+","7b8a+","7a8a"]),
    (("Saddle_Ply_320_R*2i", "Saddle_Ply_352_9b8a"), ["3i2i","R*1i","2i1i","S*1h","2h1h","S*2i","1i2i","G*1i","2i1i","N*7f","B*9b","8b9b","G*7b","7f8h+","7b7c","B*2i","1i2i","L*2h","1h2h","P*4d","4e4d","5c4d","7c6c","P*4h","S*8b","9b8b","R*9b","8b9b","P*8b","4h4i+","8b8a+","9b8a"]),
    (("Saddle_Ply_304_7a7b", "Saddle_Ply_320_R*2i"), ["S*8b","7b8b","9f9e","4f3h+","3g3f","3h2h","1h2h","B*1h","2h1h","B*2i","1i2i","S*2h","1h2h","S*3i","2i3i","R*2i"]),
    (("Saddle_Ply_288_8b8a", "Saddle_Ply_304_7a7b"), ["P*8b","8a8b","L*8c","8b8c","B*8b","8c8b","B*9b","8b9b","R*8b","9b8b","S*8a","8b8a","S*7a","8a7a","R*7b","7a7b"]),
    (("Saddle_Ply_256_7a8a", "Saddle_Ply_288_8b8a"), ["L*8b","8a8b","2i1i","9d8f","5g5f","5e5f","8i7g","8f7h+","4g4f","7h7g","4f4e","G*2i","1i2i","B*1i","2i1i","S*2i","1i2i","S*1i","2i1i","B*2i","1i2i","R*1i","2i1i","R*2i","1i2i","S*1i","2i1i","L*1e","1f1e","N*4f","G*8a","8b8a"]),
    (("Saddle_Ply_248_7a7b", "Saddle_Ply_256_7a8a"), ["B*7a","7b7a","G*6a","7a6a","S*7a","6a7a","B*8a","7a8a"]),
    (("Saddle_Ply_224_S*3i", "Saddle_Ply_248_7a7b"), ["2h3i","S*2h","1h2h","R*1i","2i1i","R*1h","2h1h","B*2i","1i2i","S*1i","2i1i","G*2i","1i2i","L*2h","3i2h","N*9d","S*7a","6a7a","S*8a","7a8a","R*7a","8a7a","R*7b","7a7b"]),
    (("Saddle_Ply_216_4c5c", "Saddle_Ply_224_S*3i"), ["L*6b","5b6b","G*6a","6b6a","9i9g","B*2i","1i2i","S*3i"]),
    (("Saddle_Ply_208_6a6b", "Saddle_Ply_216_4c5c"), ["S*5b","6b5b","S*5a","5b5a","R*5b","5a5b","R*5c","4c5c"]),
    (("Saddle_Ply_192_5c5b", "Saddle_Ply_208_6a6b"), ["1h1i","G*1h","1i1h","R*1i","1h1i","B*2i","3i2i","S*1h","2i1h","5b6a","B*6b","6a6b","B*6a","6b6a","S*6b","6a6b"]),
    (("Saddle_Ply_160_6d6c", "Saddle_Ply_192_5c5b"), ["P*6d","6c6d","4h4i","B*2i","1i2i","G*1i","2i1i","S*2i","1i2i","B*1i","2i1i","R*2i","1i2i","R*3i","4i3i","S*1i","2i1i","L*1h","1i1h","P*6c","B*5c","4c5c","S*4c","5b4c","S*5b","6a5b","G*5e","6d5e","R*5a","S*1i","5a5b+","5c5b"]),
    (("Saddle_Ply_152_3b2b", "Saddle_Ply_160_6d6c"), ["L*3b","2b3b","R*4b","3b4b","R*4c","4b4c","P*6c","6d6c"]),
    (("Saddle_Ply_128_B*1i", "Saddle_Ply_152_3b2b"), ["1h1i","B*2i","1i2i","S*3i","2h3i","G*1i","2i1i","L*1h","1i1h","R*2h","3i2h","R*1i","1h1i","2c1b","S*1a","1b1a","B*2a","1a2a","B*3a","2a3a","S*3b","3a3b","G*2b","3b2b"]),
    (("Saddle_Ply_96_5i2i+", "Saddle_Ply_128_B*1i"), ["3h2i","N*1g","2h1g","L*2h","1g2h","N*1g","1i1g","P*4e","7g6h","6i6h","L*5b","4a5b","S*4a","5a4a","B*5a","4a5a","R*4a","5a4a","R*3a","4a3a","G*3b","3a3b","B*2b","3b2b","N*2d","2c2d","N*2c","2b2c","2i1i","S*1h","1i1h","B*1i"]),
    (("Saddle_Ply_64_6h6i+", "Saddle_Ply_96_5i2i+"), ["R*7a","P*6h","7a6a+","5b6a","G*5b","6b5b","B*5d","5c5d","S*3a","4a3a","B*4a","3a4a","R*5c","5b5c","P*6d","5c6d","8h7g","R*4i","3i4i","G*4h","3h4h","B*3i","4i3i","B*3h","4h3h","S*4i","3i4i","P*4h","3h4h","R*5i","4i3h","5i2i+"]),
    (("Saddle_Ply_32_7a6a", "Saddle_Ply_64_6h6i+"), ["P*7b","6c7b","4i3i","6b6g+","P*6c","6g5h","6i5h","7b6c","P*6b","6a6b","R*6a","5b6a","5h4h","R*3h","4h3h","R*4i","3i4i","B*5i","4i5i","G*6i","5i6i","B*5i","6i5i","L*5h","5i5h","P*4h","5h4h","P*6h","L*5b","6c5b","4h3i","6h6i+"]),
    (("Start", "Saddle_Ply_32_7a6a"), ["2h5h","3a4b","3i2h","8b6b","8g8f","7a7b","9g9f","3c3d","1g1f","2b7g","8h7g","7c7d","7g1a+","8a7c","1a2a","8c8d","2a4c","4b4c","L*5b","4c5b","B*7a","6a7a","N*5e","B*8h","7i8h","P*4h","5e6c+","4h4i+","5i4i","7b6c","B*6a","7a6a"])
  ]
