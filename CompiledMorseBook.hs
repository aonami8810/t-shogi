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

-- | Node ID: Minimum_Gote_Victory (Type: Minimum, Potential: 10000.0)
-- | Node ID: Saddle_Furibisha_Mino (Type: Saddle, Potential: 68.02261555670782)
-- | Node ID: Saddle_Kakugawari_Clash (Type: Saddle, Potential: -0.7058608834278957)
-- | Node ID: Saddle_Mino_Attack (Type: Saddle, Potential: -3320.4802059049844)
-- | Node ID: Saddle_Yagura_Climax (Type: Saddle, Potential: -1644.5524228857562)
-- | Node ID: Saddle_Yagura_Fortress (Type: Saddle, Potential: 43.54056860248669)
-- | Node ID: Start (Type: Saddle, Potential: 0.0)

-- ============================================================================
-- 2. 勾配流（V-Path）の型レベル二項遷移関係の定義（Type Family）
--    アクティブ・ラーニングで収束が確認されたパスのみが 'True と判定されます。
-- ============================================================================
type family ValidTransition (from :: Symbol) (to :: Symbol) :: Bool where
  ValidTransition "Saddle_Mino_Attack" "Minimum_Gote_Victory" = 'True
  ValidTransition "Start" "Saddle_Kakugawari_Clash" = 'True
  ValidTransition "Saddle_Furibisha_Mino" "Saddle_Mino_Attack" = 'True
  ValidTransition "Start" "Saddle_Furibisha_Mino" = 'True
  ValidTransition "Saddle_Yagura_Fortress" "Saddle_Yagura_Climax" = 'True
  ValidTransition "Start" "Saddle_Yagura_Fortress" = 'True
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
    (("Saddle_Mino_Attack", "Minimum_Gote_Victory"), ["5e5d","5c5d","R*5g","5c5b+","5g5h","5i6i","5h6h","6i7i"]),
    (("Start", "Saddle_Kakugawari_Clash"), ["7g7f","3c3d","8h2b+","3a2b","2g2f","8c8d","2f2e","2b3c"]),
    (("Saddle_Furibisha_Mino", "Saddle_Mino_Attack"), ["8h2b+","3a2b","5f5c+","4b5c","B*5b","5c5b"]),
    (("Start", "Saddle_Furibisha_Mino"), ["7g7f","3c3d","2h5h","5a5b","5h5f","4a3b","3i4h","5b4b"]),
    (("Saddle_Yagura_Fortress", "Saddle_Yagura_Climax"), ["8h2b+","3a2b","B*4a","4b4a","2h5h","B*5i"]),
    (("Start", "Saddle_Yagura_Fortress"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","6i7h","4a3b","5i6i","5a4b"])
  ]
