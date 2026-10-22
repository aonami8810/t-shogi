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
    (("Start", "Saddle_DirectGold_KakutoDefense"), ["7g7f","8c8d","7i7h","8d8e","8h7g","3c3d","2g2f","7a6b","2f2e","4a3b","3i4h","5a4b","5i6h","4b3c","3g3f","7c7d","4h3g","6b7c","3g4f","7c8d","6h7i","8d7c","2e2d","2c2d","2h2d","P*2c","2d2h","7c8d","6g6f","8d7c"]),
    (("Start", "Saddle_EdgePawn_Solid"), ["1g1f","3c3d","7g7f","8c8d","6g6f","8d8e","7i6h","7a6b","2g2f","4a3b","6h7g","7c7d","5g5f","5a4b","4i5h","6b7c","5i6h","7c8d","6h7h","2b3c","2f2e","3a2b","3i4h","5c5d"]),
    (("Start", "Saddle_Gote_34Pawn_Yagura"), ["7g7f","3c3d","2g2f","4c4d","4i4h","3a3b","5g5f","4a4b","7i6h","5a5b","6h7g","4b4c","5i6h","7a6b","6h7h","6c6d","2f2e","2b3c","4g4f","8b4b","6g6f","6b6c","3i3h","5c5d"]),
    (("Start", "Saddle_Gote_84Pawn_Ibisha"), ["2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","6c6d","6g6f","7b6c","6h7i","3c3d","7g7f","1c1d","1g1f","6c5d"]),
    (("Start", "Saddle_Yokofudori_34Rook"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8d","3d2d","P*2c","2d2h","P*8g","7h8g","8d8b","6g6f","4a4b","3i3h","5a5b"]),
    (("Start", "Saddle_Nakabisha_LeftKing"), ["5g5f","3c3d","2h5h","5c5d","7g7f","8c8d","7i6h","8d8e","3i3h","7a6b","5i4h","4a3b","4h3i","5a4b","3i2h","7c7d","4i4h","6b7c","2g2f","2b3c","6g6f","4c4d","1g1f","1c1d"]),
    (("Start", "Saddle_Nakabisha_Gokigen"), ["5g5f","3c3d","2h5h","8c8d","7g7f","8d8e","7i6h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3i","7c7d","3i2h","6b7c","4i4h","2b3c","2g2f","7c6d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","3g3f","6d5e"]),
    (("Start", "Saddle_Sankenbisha_Ishida"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","7a6b","3i3h","4a3b","5i4h","5a4b","4h3i","7c7d","3i2h","6b7c","1g1f","1c1d","7f7e","2b3c","7h7f","5c5d","4i4h","9c9d"]),
    (("Start", "Saddle_Shikenbisha_Exchange"), ["7g7f","3c3d","2h6h","8c8d","8h2b+","3a2b","7i8h","7a6b","8h7g","8d8e","6h6f","6c6d","3i3h","4a3b","5i4h","5a4b","4h3i","6b7b","3i2h","7b6c","4i4h","2b3c","1g1f","1c1d","2h1h","6c5d"]),
    (("Start", "Saddle_Shikenbisha_Mino"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","7a6b","3i3h","7c7d","5i4h","4a3b","4h3i","5a4b","3i2h","6b7c","4i4h","7c8d","6h7h","8b7b","1g1f","1c1d","2g2f","5c5d","2h1h","9c9d"]),
    (("Start", "Saddle_Yagura_Kato"), ["7g7f","8c8d","7i6h","3c3d","6h7g","7a6b","2g2f","8d8e","5g5f","4a3b","5i6h","5a4b","4i5h","6c6d","6g6f","6b6c","2f2e","7c7d","3i4h","2b3c","1g1f","1c1d"]),
    (("Start", "Saddle_Yagura_Fortress"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","6i7h","4b3c","5i6h","5c5d","2g2f","6b5c","2f2e","8d8e","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b"]),
    (("Start", "Saddle_Kakugawari_Koshikake"), ["7g7f","8c8d","2g2f","8d8e","2f2e","3c3d","6i7h","2b3c","8h2b+","3c2b","3i3h","7a6b","3h4h","6b7b","4g4f","6c6d","4h4g","7b6c","4g5f","5a4b","5i6h","3a3b","3g3f","4a5b"]),
    (("Start", "Saddle_Kakugawari_Hayaguri"), ["7g7f","8c8d","2g2f","8d8e","2f2e","3c3d","6i7h","2b3c","8h2b+","3c2b","3i4h","7a6b","3g3f","6b7b","4h3g","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","6d6e","2e2d","2c2d","2h2d","P*2c","2d2h","7b7c"]),
    (("Start", "Saddle_Aigakari_Hikibisha"), ["2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","6c6d","6g6f","7b6c","7g7f","3c3d","1g1f","1c1d","3g3f","6c5d"]),
    (("Start", "Saddle_Aigakari_Standard"), ["2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","3c3d","7g7f","6c6d","3g3f","7b6c","3h3g","8b8d","2h2f","8d7d","6g6f","7d8d","6h7i","5c5d"])
  ]
