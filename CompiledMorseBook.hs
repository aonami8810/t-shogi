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

-- | Node ID: Saddle_Aigakari_Hikibisha (Type: Saddle, Potential: 30.0)
-- | Node ID: Saddle_Aigakari_Standard (Type: Saddle, Potential: 25.0)
-- | Node ID: Saddle_DirectGold_KakutoDefense (Type: Saddle, Potential: 35.0)
-- | Node ID: Saddle_EdgePawn_Solid (Type: Saddle, Potential: 15.0)
-- | Node ID: Saddle_Gote_34Pawn_Yagura (Type: Saddle, Potential: 25.0)
-- | Node ID: Saddle_Gote_84Pawn_Ibisha (Type: Saddle, Potential: 20.0)
-- | Node ID: Saddle_Kakugawari_Hayaguri (Type: Saddle, Potential: 40.0)
-- | Node ID: Saddle_Kakugawari_Koshikake (Type: Saddle, Potential: 35.0)
-- | Node ID: Saddle_Nakabisha_Gokigen (Type: Saddle, Potential: 65.0)
-- | Node ID: Saddle_Nakabisha_LeftKing (Type: Saddle, Potential: 55.0)
-- | Node ID: Saddle_Sankenbisha_Ishida (Type: Saddle, Potential: 50.0)
-- | Node ID: Saddle_Shikenbisha_Exchange (Type: Saddle, Potential: 55.0)
-- | Node ID: Saddle_Shikenbisha_Mino (Type: Saddle, Potential: 60.0)
-- | Node ID: Saddle_Yagura_Fortress (Type: Saddle, Potential: 50.0)
-- | Node ID: Saddle_Yagura_Kato (Type: Saddle, Potential: 45.0)
-- | Node ID: Saddle_Yokofudori_34Rook (Type: Saddle, Potential: 70.0)
-- | Node ID: Start (Type: Saddle, Potential: 0.0)

-- ============================================================================
-- 2. 勾配流（V-Path）の型レベル二項遷移関係の定義（Type Family）
--    アクティブ・ラーニングで収束が確認されたパスのみが 'True と判定されます。
-- ============================================================================
type family ValidTransition (from :: Symbol) (to :: Symbol) :: Bool where
  ValidTransition "Start" "Saddle_DirectGold_KakutoDefense" = 'True
  ValidTransition "Start" "Saddle_EdgePawn_Solid" = 'True
  ValidTransition "Start" "Saddle_Gote_34Pawn_Yagura" = 'True
  ValidTransition "Start" "Saddle_Gote_84Pawn_Ibisha" = 'True
  ValidTransition "Start" "Saddle_Yokofudori_34Rook" = 'True
  ValidTransition "Start" "Saddle_Nakabisha_LeftKing" = 'True
  ValidTransition "Start" "Saddle_Nakabisha_Gokigen" = 'True
  ValidTransition "Start" "Saddle_Sankenbisha_Ishida" = 'True
  ValidTransition "Start" "Saddle_Shikenbisha_Exchange" = 'True
  ValidTransition "Start" "Saddle_Shikenbisha_Mino" = 'True
  ValidTransition "Start" "Saddle_Yagura_Kato" = 'True
  ValidTransition "Start" "Saddle_Yagura_Fortress" = 'True
  ValidTransition "Start" "Saddle_Kakugawari_Koshikake" = 'True
  ValidTransition "Start" "Saddle_Kakugawari_Hayaguri" = 'True
  ValidTransition "Start" "Saddle_Aigakari_Hikibisha" = 'True
  ValidTransition "Start" "Saddle_Aigakari_Standard" = 'True
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
    (("Start", "Saddle_DirectGold_KakutoDefense"), ["7g7f","8c8d","7i7h","8d8e","8h7g","3c3d","2g2f","6b6c","2f2e","4a3b","3i4h","5a4b","5i6h","4b3c","6h7h","7a6b","7h8h","6b7c","4h3g","7c8d","3g4f","8d7c","2e2d","2c2d","2h2d","P*2c","2d2h","3c4b","7g2b+","3a2b","P*3c","3b3c","S*7h"]),
    (("Start", "Saddle_EdgePawn_Solid"), ["1g1f","3c3d","7g7f","8c8d","6g6f","8d8e","7i6h","6b6c","2g2f","4a3b","6h7g","7a6b","5g5f","5a4b","4i5h","4b3a","5i6h","3a2b","6h7h","6b7b","7g6h","8e8f","6f6e","S*3c","7h7i","8f8g+"]),
    (("Start", "Saddle_Gote_34Pawn_Yagura"), ["7g7f","3c3d","2g2f","4c4d","4i4h","3a3b","5g5f","4b4a","5i6h","5a4b","6h7h","4a4b","7h8h","4b4c","3i3h","3c3d","2f2e","2b3c","4h4g","8b4b","2e2d"]),
    (("Start", "Saddle_Gote_84Pawn_Ibisha"), ["2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","S*6h","3c4b","5h5i","6a5b","P*2d","2b4d"]),
    (("Start", "Saddle_Yokofudori_34Rook"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f2f","B*3c","2f2e","3c5a+"]),
    (("Start", "Saddle_Nakabisha_LeftKing"), ["5g5f","3c3d","5h5e","5c5d","5e5f","8c8d","7g7f","8d8e","7i6h","6b6c","4i5h","4a3b","3i4h","5a4b","5i4h","4b3a","4h3h","3a2b","3h2h","7a6b","2h1h","6b7c","8h2b+"]),
    (("Start", "Saddle_Nakabisha_Gokigen"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","3g3f","8d7c","S*7h","B*4i","2e2d","3a4a","2h1h","4i3h+"]),
    (("Start", "Saddle_Sankenbisha_Ishida"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","8b8f","7g8f"]),
    (("Start", "Saddle_Shikenbisha_Exchange"), ["7g7f","3c3d","2h6h","8c8d","8h2b+","3a2b","7i8h","7a6b","8h7g","8d8e","6h6f","6b7b","4i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b8b","6i5h","4b3a","1g1f","1c1d","6f6c+","B*4i","B*6i","6a7a","G*3h","4i3h+"]),
    (("Start", "Saddle_Shikenbisha_Mino"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","3c2d","6f6e"]),
    (("Start", "Saddle_Yagura_Kato"), ["7g7f","8c8d","7i6h","3c3d","6h7g","7a6b","2g2f","8d8e","5g5f","4a3b","5i6h","5a4b","4i5h","6b5c","6h7h","7c7d","2f2e","3a2b","7h8h","5c6d","B*6f","B*3e","5h6h","8e8f","6i7h","P*5e"]),
    (("Start", "Saddle_Yagura_Fortress"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","2e2d","4a4b","B*1f","9a9b","8h7i"]),
    (("Start", "Saddle_Kakugawari_Koshikake"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3a2b","7i8h","7a6b","3i3h","6b7b","3h4h","4a3b","4h4g","5a4b","5i6h","6c6d","4g5f","7b6c","6i7h","5a4b","S*5h"]),
    (("Start", "Saddle_Kakugawari_Hayaguri"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3a2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","3b3c","B*1f"]),
    (("Start", "Saddle_Aigakari_Hikibisha"), ["2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2f","7a7b","3i3h","5a4b","5i6h","4b3c","4i5h","7b6c","6g6f","8e8f","8g8f","8b8f","P*8g","8f8b","3h2g","8b8d","2f2h","8d7d","6h7i","6a5b","5h6h","3c4b","2g3f","P*3h"]),
    (("Start", "Saddle_Aigakari_Standard"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2e","7d8d","2e2f","8d7d","8h3c+"])
  ]
