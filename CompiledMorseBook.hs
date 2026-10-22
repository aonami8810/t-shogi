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

-- | Node ID: Aigakari_StratifiedEquilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly03_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly04_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly05_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly05_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly06_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly07_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly07_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly08_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly09_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly09_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly10_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly10_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly11_2h2e (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly11_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly12_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly12_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly13_2d1d (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly13_2d2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly14_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly14_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly15_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly15_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly16_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly16_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly17_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly17_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly18_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly18_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly19_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly19_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly20_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly20_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly21_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly21_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly22_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly22_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly23_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly23_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly24_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly24_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly25_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly25_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly27_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly27_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly29_6g6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly29_7h7g (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly31_2f2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly31_2f6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly33_2e3e (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly33_2e5e (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly35_2f2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly35_2f6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Early3eBlitz_Melee (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly03_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly04_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly05_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly05_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly06_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly07_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly07_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly08_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly09_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly09_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly10_1a1d (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly10_1a1e (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly11_8h2b+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly11_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly12_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly12_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly13_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly13_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly14_1a1c (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly14_2b1c (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly15_1c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly15_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly16_2b1a (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly16_2b1c (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly17_1b2b (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly17_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly18_2e1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly18_2e3g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly19_1a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly19_1i1h (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly20_1g2g (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly20_1g2h (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly21_2a1a (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly21_2a3a (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly22_2c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly22_3a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly23_2a1a (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly23_2a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly24_2c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly24_3c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly25_2h3h (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly25_2h4h (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly26_2a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly26_2a3b (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly27_2h3h (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly27_2h4h (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly28_2b1a (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly28_3c4b (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly29_2h3h (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly29_2h4h (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly30_3c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly30_3c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly31_1h1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly31_1h1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly32_1d2c (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly32_1d2e (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly33_1h1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly33_1h1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly34_1e2d (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly34_1e2f (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly35_1h1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly35_2a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly36_1f1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Early3eBlitz_Melee_DevPly36_1f1g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly03_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly04_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly05_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly05_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly06_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly07_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly07_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly08_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly09_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly09_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly10_1a1d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly10_1a1e (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly11_8h2b+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly11_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly12_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly12_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly13_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly13_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly14_1a1c (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly14_2b1c (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly15_1c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly15_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly16_2b1a (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly16_2b1c (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly17_1b2b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly17_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly18_2e1g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly18_2e3g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly19_1a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly19_1i1h (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly20_1g2g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly20_3c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly21_2a1a (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly21_2a3a (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly22_2c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly22_3a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly23_2a1a (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly23_2a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly24_2c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly24_3c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly25_2h2g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly25_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly26_2a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly26_2a3b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly27_2h2f (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly27_2h2g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly28_2b1a (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly28_3c4b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly29_2h2f (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly29_2h2g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly30_3c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly30_3c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly31_1h1f (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly31_1h1g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly32_1d2c (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly32_1d2e (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly33_1h1f (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly33_1h1g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly34_1e2d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly34_1e2f (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly35_1h1g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly35_2a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly36_1f1g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyClimbingSilver_Melee_DevPly36_1f1g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly03_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly04_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly05_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly05_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly06_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly07_7f8e (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly07_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly08_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly09_7f8e (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly09_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly10_1a1d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly10_1a1e (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly11_7f8e (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly11_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly12_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly12_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly13_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly13_7f8e (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly14_1a1c (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly14_2b1c (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly15_1c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly15_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly16_2b1a (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly16_2b1c (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly17_1b2b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly17_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly18_2e1g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly18_2e3g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly19_1a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly19_1i1h (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly20_1g2g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly20_1g2h (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly21_2a1a (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly21_2a3a (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly22_2c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly22_3a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly23_2a1a (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly23_2a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly24_2c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly24_3c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly25_2h3h (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly25_2h4h (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly26_2a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly26_2a3b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly27_2h3h (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly27_2h4h (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly28_2b1a (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly28_2c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly29_2h3h (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly29_2h4h (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly30_3c6f (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly30_3c7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly31_1h1f (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly31_1h1g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly32_1d2e (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly32_3c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly33_1h1f (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly33_1h1g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly34_1e2d (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly34_1e2f (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly35_1h1g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly35_2b1a (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly36_1f1g (Type: Saddle, Potential: 0.0)
-- | Node ID: EarlyIshidaMelee_DevPly36_1f1g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly03_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly04_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly05_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly05_8h5e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly06_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly07_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly07_8h5e (Type: Saddle, Potential: 50.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly08_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly09_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly09_8h5e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly10_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly10_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly11_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly11_8h5e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly12_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly12_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly13_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly13_8h5e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly14_4d3e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly15_4f3e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly15_4f5e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly16_4d3e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly17_2h2e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly17_4f3e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly18_4d3e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly19_2d1d (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly19_2d2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly20_4d3e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly21_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly21_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly22_4d3e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly23_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly23_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly24_4d3e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly25_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly25_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly26_4d3e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly27_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly27_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly28_4d3e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly29_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly29_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly30_4d3e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly31_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly31_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly32_2b4d (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly33_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly33_2h2e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly34_5e3e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly34_5e4e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly35_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly35_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly36_1a1b (Type: Saddle, Potential: 50.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly36_1a1c (Type: Saddle, Potential: 50.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly37_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly37_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly38_2b3a (Type: Saddle, Potential: 50.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly38_S*1e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly39_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly39_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly40_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly40_2b3a (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly41_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly41_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly42_1a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly42_3d3e (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly43_2a1b (Type: Saddle, Potential: 50.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly43_2a2b (Type: Saddle, Potential: 50.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly44_1a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly44_1a1c (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly45_2c1c (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly45_2c3c (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly46_1a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly46_3a4b (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly47_P*2b (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly47_P*2c (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly48_5b4b (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_IronDefense_Equilibrium_DevPly48_5b5a (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_TokinAssault_Equilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_TokinAssault_Equilibrium_DevPly09_5g5f (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_TokinAssault_Equilibrium_DevPly11_4h5g (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_TokinAssault_Equilibrium_DevPly12_5g4h (Type: Saddle, Potential: 0.0)
-- | Node ID: GoteGokigen_TokinAssault_Equilibrium_DevPly12_5g6h (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly03_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly04_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly05_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly05_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly06_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly07_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly07_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly08_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly09_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly09_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly10_3b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly10_5a4b (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly11_B*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly11_B*1d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly12_5a5b (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly12_5a6b (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly13_B*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly13_B*1d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly14_5a5b (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly14_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly15_B*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly15_B*1d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly16_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly16_B*1h (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly17_B*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly17_B*1d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly18_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly18_B*1h (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly19_P*3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly19_P*3e (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly20_4b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly20_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly21_P*3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly21_P*3e (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly22_4b3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly22_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly23_P*3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly23_P*3e (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly24_4b3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly24_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly25_3e2d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly25_3e4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly26_4b3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly26_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly27_2h2e (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly27_3e4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly28_4b3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly28_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly29_2d1d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly29_2d2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly30_4b3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly30_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly31_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly31_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly32_4b3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly32_S*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly33_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly33_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly34_4b3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly34_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly35_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly35_2h2d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly36_1a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly36_1c1d (Type: Saddle, Potential: 0.0)
-- | Node ID: MetaIdeal_001_AtamaKin_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_002_ShiriKin_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_003_BellySilver_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_004_IkkenRyu_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_005_TsurushiKei_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_006_BishopDive_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_007_TatatakiPawn_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_008_AtamaKin_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_009_ShiriKin_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_010_BellySilver_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_011_IkkenRyu_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_012_TsurushiKei_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_013_BishopDive_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_014_TatatakiPawn_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_015_AtamaKin_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_016_ShiriKin_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_017_BellySilver_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_018_IkkenRyu_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_019_TsurushiKei_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_020_BishopDive_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_021_TatatakiPawn_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_022_AtamaKin_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_023_ShiriKin_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_024_BellySilver_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_025_IkkenRyu_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_026_TsurushiKei_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_027_BishopDive_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_028_TatatakiPawn_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_029_AtamaKin_71 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_030_ShiriKin_71 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_031_BellySilver_71 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_032_IkkenRyu_71 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_033_TsurushiKei_71 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_034_BishopDive_71 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_035_TatatakiPawn_71 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_036_AtamaKin_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_037_ShiriKin_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_038_BellySilver_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_039_IkkenRyu_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_040_TsurushiKei_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_041_BishopDive_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_042_TatatakiPawn_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_043_AtamaKin_81 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_044_ShiriKin_81 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_045_BellySilver_81 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_046_IkkenRyu_81 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_047_TsurushiKei_81 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_048_BishopDive_81 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_049_TatatakiPawn_81 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_050_AtamaKin_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_051_ShiriKin_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_052_BellySilver_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_053_IkkenRyu_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_054_TsurushiKei_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_055_BishopDive_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_056_TatatakiPawn_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_057_AtamaKin_91 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_058_ShiriKin_91 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_059_BellySilver_91 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_060_IkkenRyu_91 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_061_TsurushiKei_91 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_062_BishopDive_91 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_063_TatatakiPawn_91 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_064_MiyakoCenter_55 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_065_HeadSupportTsumero_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_066_TarekomaPawnTsumero_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_067_DragonEncroach_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_068_HorseAnchor_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_069_PincerBrinkmate_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_070_KnightForkTsumero_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_071_SilverForkTsumero_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_072_HeadSupportTsumero_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_073_TarekomaPawnTsumero_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_074_DragonEncroach_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_075_HorseAnchor_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_076_PincerBrinkmate_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_077_KnightForkTsumero_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_078_SilverForkTsumero_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_079_HeadSupportTsumero_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_080_TarekomaPawnTsumero_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_081_DragonEncroach_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_082_HorseAnchor_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_083_PincerBrinkmate_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_084_KnightForkTsumero_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_085_SilverForkTsumero_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_086_HeadSupportTsumero_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_087_TarekomaPawnTsumero_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_088_DragonEncroach_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_089_HorseAnchor_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_090_PincerBrinkmate_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_091_KnightForkTsumero_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_092_SilverForkTsumero_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_093_HeadSupportTsumero_71 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_094_TarekomaPawnTsumero_71 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_095_DragonEncroach_71 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_096_HorseAnchor_71 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_097_PincerBrinkmate_71 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_098_KnightForkTsumero_71 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_099_SilverForkTsumero_71 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_100_HeadSupportTsumero_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_101_TarekomaPawnTsumero_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_102_DragonEncroach_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_103_HorseAnchor_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_104_PincerBrinkmate_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_105_KnightForkTsumero_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_106_SilverForkTsumero_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_107_HeadSupportTsumero_81 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_108_TarekomaPawnTsumero_81 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_109_DragonEncroach_81 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_110_HorseAnchor_81 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_111_PincerBrinkmate_81 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_112_KnightForkTsumero_81 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_113_SilverForkTsumero_81 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_114_HeadSupportTsumero_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_115_TarekomaPawnTsumero_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_116_DragonEncroach_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_117_HorseAnchor_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_118_PincerBrinkmate_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_119_KnightForkTsumero_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_120_SilverForkTsumero_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_121_HeadSupportTsumero_91 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_122_TarekomaPawnTsumero_91 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_123_DragonEncroach_91 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_124_HorseAnchor_91 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_125_PincerBrinkmate_91 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_126_KnightForkTsumero_91 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_127_SilverForkTsumero_91 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_128_EscapeCounter_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_129_DragonInfiltration_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_130_DragonInfiltration_12 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_131_DragonInfiltration_13 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_132_DragonInfiltration_14 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_133_DragonInfiltration_15 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_134_DragonInfiltration_16 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_135_DragonInfiltration_17 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_136_DragonInfiltration_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_137_DragonInfiltration_22 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_138_DragonInfiltration_23 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_139_DragonInfiltration_24 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_140_DragonInfiltration_25 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_141_DragonInfiltration_26 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_142_DragonInfiltration_27 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_143_DragonInfiltration_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_144_DragonInfiltration_32 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_145_DragonInfiltration_33 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_146_DragonInfiltration_34 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_147_DragonInfiltration_35 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_148_DragonInfiltration_36 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_149_DragonInfiltration_37 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_150_DragonInfiltration_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_151_DragonInfiltration_42 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_152_DragonInfiltration_43 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_153_DragonInfiltration_44 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_154_DragonInfiltration_45 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_155_DragonInfiltration_46 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_156_DragonInfiltration_47 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_157_DragonInfiltration_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_158_DragonInfiltration_52 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_159_DragonInfiltration_53 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_160_DragonInfiltration_54 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_161_DragonInfiltration_55 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_162_DragonInfiltration_56 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_163_DragonInfiltration_57 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_164_DragonInfiltration_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_165_DragonInfiltration_62 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_166_DragonInfiltration_63 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_167_DragonInfiltration_64 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_168_DragonInfiltration_65 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_169_DragonInfiltration_66 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_170_DragonInfiltration_67 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_171_DragonInfiltration_71 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_172_DragonInfiltration_72 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_173_DragonInfiltration_73 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_174_DragonInfiltration_74 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_175_DragonInfiltration_75 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_176_DragonInfiltration_76 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_177_DragonInfiltration_77 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_178_DragonInfiltration_81 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_179_DragonInfiltration_82 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_180_DragonInfiltration_83 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_181_DragonInfiltration_84 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_182_DragonInfiltration_85 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_183_DragonInfiltration_86 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_184_DragonInfiltration_87 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_185_DragonInfiltration_91 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_186_DragonInfiltration_92 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_187_DragonInfiltration_93 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_188_DragonInfiltration_94 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_189_DragonInfiltration_95 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_190_DragonInfiltration_96 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_191_DragonInfiltration_97 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_192_DragonPenetration_Center (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_193_KnightForkTactics_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_194_KnightForkTactics_12 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_195_KnightForkTactics_13 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_196_KnightForkTactics_14 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_197_KnightForkTactics_15 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_198_KnightForkTactics_16 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_199_KnightForkTactics_17 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_200_KnightForkTactics_18 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_201_KnightForkTactics_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_202_KnightForkTactics_22 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_203_KnightForkTactics_23 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_204_KnightForkTactics_24 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_205_KnightForkTactics_25 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_206_KnightForkTactics_26 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_207_KnightForkTactics_27 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_208_KnightForkTactics_28 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_209_KnightForkTactics_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_210_KnightForkTactics_32 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_211_KnightForkTactics_33 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_212_KnightForkTactics_34 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_213_KnightForkTactics_35 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_214_KnightForkTactics_36 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_215_KnightForkTactics_37 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_216_KnightForkTactics_38 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_217_KnightForkTactics_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_218_KnightForkTactics_42 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_219_KnightForkTactics_43 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_220_KnightForkTactics_44 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_221_KnightForkTactics_45 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_222_KnightForkTactics_46 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_223_KnightForkTactics_47 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_224_KnightForkTactics_48 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_225_KnightForkTactics_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_226_KnightForkTactics_52 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_227_KnightForkTactics_53 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_228_KnightForkTactics_54 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_229_KnightForkTactics_55 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_230_KnightForkTactics_56 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_231_KnightForkTactics_57 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_232_KnightForkTactics_58 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_233_KnightForkTactics_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_234_KnightForkTactics_62 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_235_KnightForkTactics_63 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_236_KnightForkTactics_64 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_237_KnightForkTactics_65 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_238_KnightForkTactics_66 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_239_KnightForkTactics_67 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_240_KnightForkTactics_68 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_241_KnightForkTactics_71 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_242_KnightForkTactics_72 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_243_KnightForkTactics_73 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_244_KnightForkTactics_74 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_245_KnightForkTactics_75 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_246_KnightForkTactics_76 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_247_KnightForkTactics_77 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_248_KnightForkTactics_78 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_249_KnightForkTactics_81 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_250_KnightForkTactics_82 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_251_KnightForkTactics_83 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_252_KnightForkTactics_84 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_253_KnightForkTactics_85 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_254_KnightForkTactics_86 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_255_KnightForkTactics_87 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_256_KnightForkTactics_88 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_257_GuardStripping_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_258_GuardStripping_12 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_259_GuardStripping_13 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_260_GuardStripping_14 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_261_GuardStripping_15 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_262_GuardStripping_16 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_263_GuardStripping_17 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_264_GuardStripping_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_265_GuardStripping_22 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_266_GuardStripping_23 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_267_GuardStripping_24 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_268_GuardStripping_25 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_269_GuardStripping_26 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_270_GuardStripping_27 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_271_GuardStripping_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_272_GuardStripping_32 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_273_GuardStripping_33 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_274_GuardStripping_34 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_275_GuardStripping_35 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_276_GuardStripping_36 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_277_GuardStripping_37 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_278_GuardStripping_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_279_GuardStripping_42 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_280_GuardStripping_43 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_281_GuardStripping_44 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_282_GuardStripping_45 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_283_GuardStripping_46 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_284_GuardStripping_47 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_285_GuardStripping_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_286_GuardStripping_52 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_287_GuardStripping_53 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_288_GuardStripping_54 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_289_GuardStripping_55 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_290_GuardStripping_56 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_291_GuardStripping_57 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_292_GuardStripping_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_293_GuardStripping_62 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_294_GuardStripping_63 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_295_GuardStripping_64 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_296_GuardStripping_65 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_297_GuardStripping_66 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_298_GuardStripping_67 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_299_GuardStripping_71 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_300_GuardStripping_72 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_301_GuardStripping_73 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_302_GuardStripping_74 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_303_GuardStripping_75 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_304_GuardStripping_76 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_305_GuardStripping_77 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_306_GuardStripping_81 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_307_GuardStripping_82 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_308_GuardStripping_83 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_309_GuardStripping_84 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_310_GuardStripping_85 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_311_GuardStripping_86 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_312_GuardStripping_87 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_313_GuardStripping_91 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_314_GuardStripping_92 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_315_GuardStripping_93 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_316_GuardStripping_94 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_317_GuardStripping_95 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_318_GuardStripping_96 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_319_GuardStripping_97 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_320_GuardStripping_Center (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_321_YaguraDestruction_1 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_322_YaguraDestruction_2 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_323_YaguraDestruction_3 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_324_YaguraDestruction_4 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_325_YaguraDestruction_5 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_326_YaguraDestruction_6 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_327_YaguraDestruction_7 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_328_YaguraDestruction_8 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_329_MinoDestruction_1 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_330_MinoDestruction_2 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_331_MinoDestruction_3 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_332_MinoDestruction_4 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_333_MinoDestruction_5 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_334_MinoDestruction_6 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_335_MinoDestruction_7 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_336_MinoDestruction_8 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_337_AnagumaDestruction_1 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_338_AnagumaDestruction_2 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_339_AnagumaDestruction_3 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_340_AnagumaDestruction_4 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_341_AnagumaDestruction_5 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_342_AnagumaDestruction_6 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_343_AnagumaDestruction_7 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_344_AnagumaDestruction_8 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_345_FunagakoiDestruction_1 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_346_FunagakoiDestruction_2 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_347_FunagakoiDestruction_3 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_348_FunagakoiDestruction_4 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_349_FunagakoiDestruction_5 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_350_FunagakoiDestruction_6 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_351_FunagakoiDestruction_7 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_352_FunagakoiDestruction_8 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_353_GangiDestruction_1 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_354_GangiDestruction_2 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_355_GangiDestruction_3 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_356_GangiDestruction_4 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_357_GangiDestruction_5 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_358_GangiDestruction_6 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_359_GangiDestruction_7 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_360_GangiDestruction_8 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_361_ElmoDestruction_1 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_362_ElmoDestruction_2 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_363_ElmoDestruction_3 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_364_ElmoDestruction_4 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_365_ElmoDestruction_5 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_366_ElmoDestruction_6 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_367_ElmoDestruction_7 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_368_ElmoDestruction_8 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_369_SittingKingDestruction_1 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_370_SittingKingDestruction_2 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_371_SittingKingDestruction_3 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_372_SittingKingDestruction_4 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_373_SittingKingDestruction_5 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_374_SittingKingDestruction_6 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_375_SittingKingDestruction_7 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_376_SittingKingDestruction_8 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_377_RightKingDestruction_1 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_378_RightKingDestruction_2 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_379_RightKingDestruction_3 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_380_RightKingDestruction_4 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_381_RightKingDestruction_5 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_382_RightKingDestruction_6 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_383_RightKingDestruction_7 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_384_RightKingDestruction_8 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_385_MaterialDomination_1 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_386_MaterialDomination_2 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_387_MaterialDomination_3 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_388_MaterialDomination_4 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_389_MaterialDomination_5 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_390_MaterialDomination_6 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_391_MaterialDomination_7 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_392_MaterialDomination_8 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_393_MaterialDomination_9 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_394_MaterialDomination_10 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_395_MaterialDomination_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_396_MaterialDomination_12 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_397_MaterialDomination_13 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_398_MaterialDomination_14 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_399_MaterialDomination_15 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_400_MaterialDomination_16 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_401_MaterialDomination_17 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_402_MaterialDomination_18 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_403_MaterialDomination_19 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_404_MaterialDomination_20 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_405_MaterialDomination_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_406_MaterialDomination_22 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_407_MaterialDomination_23 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_408_MaterialDomination_24 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_409_MaterialDomination_25 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_410_MaterialDomination_26 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_411_MaterialDomination_27 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_412_MaterialDomination_28 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_413_MaterialDomination_29 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_414_MaterialDomination_30 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_415_MaterialDomination_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_416_MaterialDomination_32 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_417_MaterialDomination_33 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_418_MaterialDomination_34 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_419_MaterialDomination_35 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_420_MaterialDomination_36 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_421_MaterialDomination_37 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_422_MaterialDomination_38 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_423_MaterialDomination_39 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_424_MaterialDomination_40 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_425_MaterialDomination_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_426_MaterialDomination_42 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_427_MaterialDomination_43 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_428_MaterialDomination_44 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_429_MaterialDomination_45 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_430_MaterialDomination_46 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_431_MaterialDomination_47 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_432_MaterialDomination_48 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_433_MaterialDomination_49 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_434_MaterialDomination_50 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_435_MaterialDomination_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_436_MaterialDomination_52 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_437_MaterialDomination_53 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_438_MaterialDomination_54 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_439_MaterialDomination_55 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_440_MaterialDomination_56 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_441_MaterialDomination_57 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_442_MaterialDomination_58 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_443_MaterialDomination_59 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_444_MaterialDomination_60 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_445_MaterialDomination_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_446_MaterialDomination_62 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_447_MaterialDomination_63 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_448_MaterialDomination_64 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_449_MorseGradientSink_1 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_450_MorseGradientSink_2 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_451_MorseGradientSink_3 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_452_MorseGradientSink_4 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_453_MorseGradientSink_5 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_454_MorseGradientSink_6 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_455_MorseGradientSink_7 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_456_MorseGradientSink_8 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_457_MorseGradientSink_9 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_458_MorseGradientSink_10 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_459_MorseGradientSink_11 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_460_MorseGradientSink_12 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_461_MorseGradientSink_13 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_462_MorseGradientSink_14 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_463_MorseGradientSink_15 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_464_MorseGradientSink_16 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_465_MorseGradientSink_17 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_466_MorseGradientSink_18 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_467_MorseGradientSink_19 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_468_MorseGradientSink_20 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_469_MorseGradientSink_21 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_470_MorseGradientSink_22 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_471_MorseGradientSink_23 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_472_MorseGradientSink_24 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_473_MorseGradientSink_25 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_474_MorseGradientSink_26 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_475_MorseGradientSink_27 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_476_MorseGradientSink_28 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_477_MorseGradientSink_29 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_478_MorseGradientSink_30 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_479_MorseGradientSink_31 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_480_MorseGradientSink_32 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_481_MorseGradientSink_33 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_482_MorseGradientSink_34 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_483_MorseGradientSink_35 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_484_MorseGradientSink_36 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_485_MorseGradientSink_37 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_486_MorseGradientSink_38 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_487_MorseGradientSink_39 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_488_MorseGradientSink_40 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_489_MorseGradientSink_41 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_490_MorseGradientSink_42 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_491_MorseGradientSink_43 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_492_MorseGradientSink_44 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_493_MorseGradientSink_45 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_494_MorseGradientSink_46 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_495_MorseGradientSink_47 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_496_MorseGradientSink_48 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_497_MorseGradientSink_49 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_498_MorseGradientSink_50 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_499_MorseGradientSink_51 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_500_MorseGradientSink_52 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_501_MorseGradientSink_53 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_502_MorseGradientSink_54 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_503_MorseGradientSink_55 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_504_MorseGradientSink_56 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_505_MorseGradientSink_57 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_506_MorseGradientSink_58 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_507_MorseGradientSink_59 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_508_MorseGradientSink_60 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_509_MorseGradientSink_61 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_510_MorseGradientSink_62 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_511_MorseGradientSink_63 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MetaIdeal_512_MorseGradientSink_64 (Type: Minimum, Potential: 9999.0)
-- | Node ID: MigiShikenBlitz_Melee (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly03_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly04_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly05_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly07_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly09_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly10_1a1d (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly10_1a1e (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly13_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly14_1a1c (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly14_2b1c (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly15_1c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly15_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly16_2b1a (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly16_2b1c (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly17_1b2b (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly17_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly18_2e1g (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly18_2e3g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly19_1a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly19_1i1h (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly20_1g2h (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly20_3c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly21_2a1a (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly21_2a3a (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly22_2c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly22_3a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly23_2a1a (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly23_2a3a (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly25_2a1a (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly25_2a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly26_3c1a (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly26_3c1e (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly27_3a3b (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly27_3a4a (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly28_4b1e (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly28_4b2d (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly29_3a3b (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly29_3a4a (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly30_N*1a (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly30_N*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly31_3a3b (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly31_3a4a (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly32_4b2d (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly32_4b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly33_2c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly33_2c3c (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly34_3g2h (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly34_3g4h (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly35_3a4a (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly35_3i2h (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly36_3h2i (Type: Saddle, Potential: 0.0)
-- | Node ID: MigiShikenBlitz_Melee_DevPly36_3h3i (Type: Saddle, Potential: 0.0)
-- | Node ID: QEGTB_K59_G*5b_DTM0 (Type: Minimum, Potential: 9999.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly03_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly04_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly05_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly05_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly06_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly07_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly07_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly08_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly09_7g4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly09_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly10_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly10_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly11_7g4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly11_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly12_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly12_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly13_7g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly13_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly14_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly14_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly15_7g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly15_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly16_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly16_4b3a (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly17_7g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly17_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly18_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly18_4b3a (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly19_7g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly19_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly20_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly20_4b3a (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly21_7g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly21_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly22_B*1e (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly22_B*1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly23_7f7d (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly23_7f8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly24_B*1e (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly24_B*1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly25_7f7c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly25_7f7d (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly26_B*1e (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly26_B*1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly27_7g3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly27_7g4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly28_B*1e (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly28_B*1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly03_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly04_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly05_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly05_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly06_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly07_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly07_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly08_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly09_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly09_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly10_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly10_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly11_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly11_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly12_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly12_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly13_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly13_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly14_5a4b (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly14_5a5b (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly15_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly15_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly16_5a5b (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly16_5a6b (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly17_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly17_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly18_5a5b (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly18_8b8f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly19_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly19_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly20_2b3a (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly20_4b3a (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly21_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly21_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly22_4b3c (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly22_4b4a (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly23_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly23_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly24_8b8f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly24_8b8g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly25_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly25_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly26_8b8f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly26_8b8g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly27_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly27_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly28_8b8f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly28_8b8g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly29_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly29_B*1c (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly30_8b8f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly30_8b8g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly31_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly31_B*1c (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly32_8d8f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly32_8d8g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly33_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly33_B*1c (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly34_7d7e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly34_B*1e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly35_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly35_B*1c (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly36_1a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly36_1a1c (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly37_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly37_B*1c (Type: Saddle, Potential: 50.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly38_8e7e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly38_8e8f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly39_B*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly39_B*1c (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly40_B*1e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly40_B*1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly41_5e3e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly41_5e4e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly43_5c4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly43_5c4d+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly45_5e3e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly45_5e4e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly46_4d3e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteKakuexchange_IronMino_Equilibrium_DevPly46_B*1e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly03_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly04_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly05_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly05_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly06_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly07_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly07_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly08_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly09_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly09_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly22_6c5d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly24_6c5d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly24_8b8e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly26_6c5d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly26_8f9f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly28_6c5d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly28_8b8f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly30_6c5d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly30_8b8f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly32_6c5d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly32_8b8f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly34_6c5d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly34_8b8f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly35_8h7g (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly35_8i7g (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly36_6c5d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly36_6e5g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly38_2c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly38_4a5b (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly39_N*1c (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly39_N*1e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly40_2c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly40_4a5b (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly41_4f4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly41_4f4e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly42_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly42_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly43_4f4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly43_4f6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly44_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_IronMino_Equilibrium_DevPly44_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly03_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly04_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly05_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly05_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly06_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly07_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly07_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly10_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly10_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly11_5d5c (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly11_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly12_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly12_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly13_5c5b (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly13_5c6b (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly15_5h5c (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly15_5h5c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly16_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly16_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly17_5h5c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly17_5h5e (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly18_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: SenteNakabisha_TokinAssault_Equilibrium_DevPly18_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly03_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly09_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly11_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly13_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly15_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly15_G*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly16_4b3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly16_4b4a (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly17_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly17_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly18_4b3a (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly18_4b3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly19_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly19_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly20_4b3a (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly20_4b3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly21_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly21_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly22_4b3a (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly22_4b3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly23_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly23_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly24_4b3a (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly24_4b3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly25_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly25_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly26_4b3a (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly26_4b3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly27_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly27_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly28_4b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly28_4b4a (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly29_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly29_S*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly30_B*1e (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly30_B*1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly31_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly31_S*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly32_3a2b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly32_3c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly33_6i5i (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly33_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly34_3a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly34_3c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly35_7g5i (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly35_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly36_1a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly36_1a1c (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly37_7g5i (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly37_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly38_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly38_2d1e (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly03_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly04_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly05_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly05_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly06_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly07_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly07_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly08_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly09_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly09_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly10_1a1d (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly10_1a1e (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly11_8h2b+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly11_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly12_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly12_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly13_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly13_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly14_1a1c (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly14_2b1c (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly15_1c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly15_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly16_2b1a (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly16_2b1c (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly17_1b2b (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly17_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly18_2e1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly18_2e3g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly19_1a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly19_1i1h (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly20_1g2h (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly20_3c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly21_2a1a (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly21_2a3a (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly22_2c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly22_3a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly23_2a1a (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly23_2a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly24_2c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly24_3c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly25_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly25_3a4a (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly26_2a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly26_2a3b (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly27_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly27_3a2a (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly28_2b1a (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly28_3c4b (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly29_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly29_3a2a (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly30_3c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly30_3c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly31_1h1f (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly31_1h1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly32_1d2c (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly32_1d2e (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly33_1h1f (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly33_1h1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly34_1e2d (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly34_1e2f (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly35_1h1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly35_2a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly36_1f1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SodebishaBlitz_Melee_DevPly36_1f1g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Start (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly03_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly04_5a4b (Type: Saddle, Potential: 50.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly04_5a5b (Type: Saddle, Potential: 50.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly05_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly05_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly06_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly06_B*1h (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly07_4e3d (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly07_4e5d (Type: Saddle, Potential: 50.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly08_B*1e (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly08_B*1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly09_4e3d (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly09_4e5d (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly10_1a1d (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly10_1a1e (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly11_4e5d (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly12_B*1d (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly12_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly13_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly13_6c5b (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly15_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly15_G*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly17_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly18_1a1c (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly18_1a1d (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly20_1a1c (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly20_1a1d (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly21_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly21_G*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly22_1a1c (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly22_2e1g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly23_1c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly23_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly24_2e1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly24_2e3g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly25_1b2b (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly25_1i1h (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly26_1g2h (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly26_4b1b (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly27_1a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly27_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly28_1h1g (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly28_1h2h (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly29_2a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly29_2a3a (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly31_2a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly31_3a4a (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly32_1i1h (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly32_4b3b (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly33_1a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly33_3a4a (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly34_2i2h (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly34_3c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly35_2a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly35_3a4a (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly36_2c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: SujichigaiBishop_Melee_DevPly36_3c2d (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly04_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly05_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly05_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly06_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly07_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly07_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly08_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly09_1f1e (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly09_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly10_1a1b (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly10_1a1c (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly11_1i1f (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly11_1i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly12_5a5b (Type: Saddle, Potential: 50.0)
-- | Node ID: UreshinoMelee_DevPly12_5a6b (Type: Saddle, Potential: 50.0)
-- | Node ID: UreshinoMelee_DevPly13_1i1f (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly13_1i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly14_3c2e (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly14_5a4b (Type: Saddle, Potential: 50.0)
-- | Node ID: UreshinoMelee_DevPly15_1i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly15_2h1h (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly16_1g2g (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly16_1g2h (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly17_2h1h (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly17_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly18_1h2h (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly18_1h2i (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly19_2h1h (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly19_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly20_1i1h (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly20_3c2e (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly21_2h1h (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly21_2h4h (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly22_2i2h (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly22_3c2e (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly23_2c1b (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly23_2c3b (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly24_3c2e (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly24_3i2i (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly25_2b1b (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly25_2b3a (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly26_3b2a (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly26_3c2e (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly27_2a2b (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly27_2h1h (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly28_3c2e (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly28_3i2i (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly29_2h3h (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly29_2h4h (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly30_3c2e (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly30_3i4i (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly31_2i2e (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly31_2i2f (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly32_1e2f (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly32_3c2e (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly33_2h3h (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly33_2h4h (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly34_1f2g (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly34_1f2g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly35_2i2e (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly35_2i2g (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly36_1g1h (Type: Saddle, Potential: 0.0)
-- | Node ID: UreshinoMelee_DevPly36_1g2g (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly03_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly10_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly10_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly12_2b5e (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly12_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly14_2b5e (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly14_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly19_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly19_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly21_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly21_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly23_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly23_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly25_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly25_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly27_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly27_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly28_8b8e (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly30_8f7f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly30_8f9f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly31_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly31_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly32_8b8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly33_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly33_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly34_8b8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly35_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly35_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly36_8b8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly37_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly37_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly38_8b8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly39_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly39_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly40_8b8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly03_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly04_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly05_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly05_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly06_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly07_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly07_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly08_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly09_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly09_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly10_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly10_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly11_2h2e (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly11_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly12_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly12_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly13_2d1d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly13_2d2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly14_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly14_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly15_2d1d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly15_2d2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly16_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly16_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly17_3d1d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly17_3d4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly18_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly18_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly19_3d1d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly19_3d4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly20_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly20_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly21_3f3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly21_3f3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly22_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly22_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly23_1i1h (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly23_3d4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly24_1g2h (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly24_1g3i (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly25_1i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly25_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly26_1f3h (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly26_1f4i (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly27_2i1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly27_3e2e (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly29_3c2b (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly29_3c2d+ (Type: Saddle, Potential: 0.0)

-- ============================================================================
-- 2. 勾配流（V-Path）の型レベル二項遷移関係の定義（Type Family）
--    アクティブ・ラーニングで収束が確認されたパスのみが 'True と判定されます。
-- ============================================================================
type family ValidTransition (from :: Symbol) (to :: Symbol) :: Bool where
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly35_2f6f" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly35_2f2d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly33_2e3e" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly33_2e5e" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly31_2f6f" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly31_2f2d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly29_6g6f" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly29_7h7g" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly27_2h2d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly27_2h2c+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly25_2h2d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly25_2h2c+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly24_2b6f" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly24_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly23_2h2d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly23_2h2c+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly22_2b6f" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly22_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly21_2h2d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly21_2h2c+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly20_2b6f" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly20_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly19_2h2d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly19_2h2c+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly18_2b6f" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly18_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly17_2h2d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly17_2h2c+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly16_2b6f" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly16_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly15_2h2d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly15_2h2c+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly14_2b6f" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly14_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly13_2d1d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly13_2d2c+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly12_2b6f" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly12_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly11_2h2e" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly11_8h4d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly10_2b6f" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly10_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly09_8h4d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly09_8h3c+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly08_2b6f" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly08_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly07_8h4d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly06_2b6f" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly06_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly05_8h4d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly04_2b6f" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly04_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly36_1a1b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly36_1c1d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly35_2h2d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly35_2h2c+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly34_4b3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly34_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly33_2h2d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly33_2h2c+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly32_4b3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly32_S*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly31_2h2d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly31_2h2c+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly30_4b3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly30_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly29_2d1d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly29_2d2c+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly28_4b3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly28_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly27_2h2e" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly27_3e4d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly26_4b3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly26_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly25_3e2d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly25_3e4d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly24_4b3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly24_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly23_P*3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly23_P*3e" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly22_4b3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly22_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly21_P*3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly21_P*3e" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly20_4b3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly20_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly19_P*3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly19_P*3e" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly18_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly18_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly17_B*1b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly17_B*1d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly16_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly16_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly15_B*1b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly15_B*1d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly14_5a5b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly14_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly13_B*1b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly13_B*1d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly12_5a5b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly12_5a6b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly11_B*1b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly11_B*1d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly10_3b3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly10_5a4b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly09_8h4d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly09_8h3c+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly08_2b6f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly08_2b7g+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly07_8h4d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly06_2b6f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly06_2b7g+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly05_8h4d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly04_2b6f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly04_2b7g+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly40_8b8f" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly39_B*1b" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly39_B*1d" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly38_8b8f" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly37_B*1b" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly37_B*1d" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly36_8b8f" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly35_B*1b" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly35_B*1d" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly34_8b8f" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly33_B*1b" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly33_B*1d" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly32_8b8f" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly31_B*1b" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly31_B*1d" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly30_8f7f" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly30_8f9f" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly28_8b8e" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly27_B*1b" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly27_B*1d" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly25_7g8f" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly25_B*1b" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly23_7g8f" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly23_B*1b" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly21_7g8f" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly21_B*1b" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly19_B*1b" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly19_B*1d" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly14_2b5e" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly14_2b6f" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly12_2b5e" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly12_2b6f" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly10_2b5e" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly10_2b6f" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly08_2b6f" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly38_2a3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly38_2d1e" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly37_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly37_7g5i" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly36_1a1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly36_1a1c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly35_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly35_7g5i" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly34_3a2b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly34_3c2d" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly33_6i5i" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly33_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly32_3a2b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly32_3c2d" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly31_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly31_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly30_B*1e" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly30_B*1g" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly29_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly29_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly28_4b3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly28_4b4a" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly27_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly27_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly26_4b3a" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly26_4b3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly25_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly25_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly24_4b3a" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly24_4b3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly23_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly23_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly22_4b3a" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly22_4b3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly21_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly21_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly20_4b3a" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly20_4b3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly19_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly19_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly18_4b3a" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly18_4b3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly17_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly17_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly16_4b3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly16_4b4a" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly15_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly15_G*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly13_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly11_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly09_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly29_3c2b" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly29_3c2d+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly27_2i1g" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly27_3e2e" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly26_1f3h" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly26_1f4i" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly25_1i1g" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly25_2i1g" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly24_1g2h" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly24_1g3i" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly23_1i1h" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly23_3d4d" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly22_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly22_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly21_3f3c+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly21_3f3c" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly20_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly20_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly19_3d4d" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly19_3d1d" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly18_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly18_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly17_3d4d" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly17_3d1d" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly16_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly16_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly15_2d1d" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly15_2d2c+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly14_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly14_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly13_2d1d" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly13_2d2c+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly12_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly12_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly11_2h2e" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly11_8h4d" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly10_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly10_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly09_8h4d" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly09_8h3c+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly08_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly08_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly07_8h4d" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly06_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly06_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly05_8h4d" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly04_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly04_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly28_B*1e" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly28_B*1g" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly27_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly27_7g3c+" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly26_B*1e" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly26_B*1g" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly25_7f7d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly25_7f7c+" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly24_B*1e" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly24_B*1g" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly23_7f8f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly23_7f7d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly22_B*1e" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly22_B*1g" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly21_7g8f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly21_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly20_2a3c" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly20_4b3a" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly19_7g8f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly19_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly18_2a3c" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly18_4b3a" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly17_7g8f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly17_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly16_2a3c" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly16_4b3a" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly15_7g8f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly15_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly14_2a3c" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly14_2b6f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly13_7g8f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly13_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly12_2a3c" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly12_2b6f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly11_7g8f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly11_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly10_2b6f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly10_2b7g+" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly09_7g8f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly09_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly08_2b6f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly08_2b7g+" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly07_8h4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly06_2b6f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly06_2b7g+" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly05_8h4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly04_2b6f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly04_2b7g+" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly44_2b5e" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly44_2b6f" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly43_4f6f" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly43_4f4d" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly42_2b6f" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly42_2b7g+" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly41_4f4e" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly41_4f4d" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly40_2c2d" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly40_4a5b" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly39_N*1c" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly39_N*1e" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly38_2c2d" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly38_4a5b" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly36_6c5d" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly36_6e5g+" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly35_8h7g" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly35_8i7g" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly34_6c5d" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly34_8b8f" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly32_6c5d" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly32_8b8f" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly30_6c5d" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly30_8b8f" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly28_6c5d" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly28_8b8f" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly26_6c5d" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly26_8f9f" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly24_6c5d" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly24_8b8e" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly22_6c5d" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly09_8h4d" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly09_8h3c+" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly08_2b5e" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly08_2b6f" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly07_8h4d" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly06_2b5e" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly06_2b6f" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly05_8h4d" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly04_2b5e" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly04_2b6f" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "SenteNakabisha_IronMino_Equilibrium" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly48_5b4b" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly48_5b5a" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly47_P*2b" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly47_P*2c" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly46_1a1b" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly46_3a4b" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly45_2c1c" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly45_2c3c" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly44_1a1b" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly44_1a1c" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly43_2a2b" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly43_2a1b" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly42_1a1b" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly42_3d3e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly41_2h2d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly41_2h2c+" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly40_2a3c" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly40_2b3a" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly39_2h2d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly39_2h2c+" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly38_2b3a" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly38_S*1e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly37_2h2d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly37_2h2c+" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly36_1a1b" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly36_1a1c" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly35_2h2d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly35_2h2c+" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly34_5e4e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly34_5e3e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly33_2h2e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly33_2h2d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly32_2b4d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly31_2h2d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly31_2h2c+" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly30_4d3e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly29_2h2d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly29_2h2c+" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly28_4d3e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly27_2h2d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly27_2h2c+" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly26_4d3e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly25_2h2d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly25_2h2c+" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly24_4d3e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly23_2h2d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly23_2h2c+" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly22_4d3e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly21_2h2d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly21_2h2c+" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly20_4d3e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly19_2d1d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly19_2d2c+" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly18_4d3e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly17_2h2e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly17_4f3e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly16_4d3e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly15_4f3e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly15_4f5e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly14_4d3e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly13_8h5e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly13_8h4d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly12_2b6f" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly12_2b7g+" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly11_8h5e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly11_8h4d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly10_2b6f" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly10_2b7g+" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly09_8h5e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly09_8h4d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly08_2b6f" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly08_2b7g+" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly07_8h5e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly07_8h4d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly06_2b6f" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly06_2b7g+" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly05_8h5e" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly05_8h4d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly04_2b6f" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly04_2b7g+" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "GoteGokigen_IronDefense_Equilibrium" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly46_4d3e" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly46_B*1e" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly45_5e4e" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly45_5e3e" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly43_5c4d+" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly43_5c4d" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly41_5e4e" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly41_5e3e" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly40_B*1e" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly40_B*1g" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly39_B*1b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly39_B*1c" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly38_8e7e" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly38_8e8f" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly37_B*1b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly37_B*1c" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly36_1a1b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly36_1a1c" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly35_B*1b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly35_B*1c" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly34_7d7e" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly34_B*1e" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly33_B*1b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly33_B*1c" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly32_8d8f" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly32_8d8g+" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly31_B*1b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly31_B*1c" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly30_8b8f" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly30_8b8g+" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly29_B*1b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly29_B*1c" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly28_8b8f" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly28_8b8g+" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly27_B*1b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly27_B*1d" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly26_8b8f" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly26_8b8g+" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly25_B*1b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly25_B*1d" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly24_8b8f" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly24_8b8g+" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly23_B*1b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly23_B*1d" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly22_4b3c" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly22_4b4a" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly21_B*1b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly21_B*1d" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly20_2b3a" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly20_4b3a" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly19_B*1b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly19_B*1d" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly18_5a5b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly18_8b8f" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly17_B*1b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly17_B*1d" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly16_5a5b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly16_5a6b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly15_B*1b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly15_B*1d" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly14_5a4b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly14_5a5b" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly13_8h4d" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly13_8h3c+" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly12_2b5e" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly12_2b6f" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly11_8h4d" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly11_8h3c+" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly10_2b5e" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly10_2b6f" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly09_8h4d" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly09_8h3c+" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly08_2b5e" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly08_2b6f" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly07_8h4d" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly06_2b5e" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly06_2b6f" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly05_8h4d" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly04_2b5e" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly04_2b6f" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "SenteKakuexchange_IronMino_Equilibrium" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly18_2a3c" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly18_2b6f" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly17_5h5e" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly17_5h5c+" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly16_2a3c" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly16_2b6f" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly15_5h5c+" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly15_5h5c" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly13_5c5b" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly13_5c6b" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly12_2b6f" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly12_2b7g+" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly11_5d5c" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly11_8h4d" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly10_2b6f" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly10_2b7g+" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly07_8h4d" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly06_2b5e" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly06_2b6f" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly05_8h4d" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly04_2b5e" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly04_2b6f" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "SenteNakabisha_TokinAssault_Equilibrium" = 'True
  ValidTransition "Start" "GoteGokigen_TokinAssault_Equilibrium_DevPly12_5g4h" = 'True
  ValidTransition "Start" "GoteGokigen_TokinAssault_Equilibrium_DevPly12_5g6h" = 'True
  ValidTransition "Start" "GoteGokigen_TokinAssault_Equilibrium_DevPly11_4h5g" = 'True
  ValidTransition "Start" "GoteGokigen_TokinAssault_Equilibrium_DevPly09_5g5f" = 'True
  ValidTransition "Start" "GoteGokigen_TokinAssault_Equilibrium" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly36_2c2d" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly36_3c2d" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly35_2a2b" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly35_3a4a" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly34_2i2h" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly34_3c2b" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly33_1a1b" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly33_3a4a" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly32_1i1h" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly32_4b3b" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly31_2a2b" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly31_3a4a" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly29_2a3a" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly29_2a2b" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly28_1h2h" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly28_1h1g" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly27_1a1b" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly27_2i1g" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly26_1g2h" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly26_4b1b" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly25_1b2b" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly25_1i1h" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly24_2e1g" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly24_2e3g+" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly23_1c2b" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly23_2i1g" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly22_1a1c" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly22_2e1g+" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly21_2i1g" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly21_G*1b" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly20_1a1c" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly20_1a1d" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly18_1a1c" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly18_1a1d" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly17_2i1g" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly15_2i1g" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly15_G*1b" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly13_2i1g" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly13_6c5b" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly12_B*1d" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly12_B*1f" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly11_4e5d" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly10_1a1d" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly10_1a1e" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly09_4e3d" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly09_4e5d" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly08_B*1e" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly08_B*1g" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly07_4e3d" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly07_4e5d" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly06_B*1f" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly06_B*1h" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly05_B*1b" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly05_B*1d" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly04_5a4b" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly04_5a5b" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly03_8h4d" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "SujichigaiBishop_Melee" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly36_1f1g+" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly36_1f1g" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly35_1h1g" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly35_2a2b" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly34_1e2f" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly34_1e2d" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly33_1h1g" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly33_1h1f" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly32_1d2e" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly32_1d2c" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly31_1h1g" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly31_1h1f" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly30_3c2b" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly30_3c2d" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly29_2h3h" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly29_2h4h" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly28_2b1a" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly28_3c4b" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly27_2h3h" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly27_2h4h" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly26_2a1b" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly26_2a3b" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly25_2h3h" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly25_2h4h" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly24_2c2d" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly24_3c2b" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly23_2a1a" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly23_2a2b" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly22_2c2d" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly22_3a2b" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly21_2a1a" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly21_2a3a" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly20_1g2h" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly20_1g2g" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly19_1a1b" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly19_1i1h" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly18_2e1g" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly18_2e3g+" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly17_1b2b" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly17_2i1g" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly16_2b1a" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly16_2b1c" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly15_1c2b" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly15_2i1g" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly14_1a1c" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly14_2b1c" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly13_2i1g" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly13_8h4d" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly12_2b6f" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly12_2b7g+" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly11_8h4d" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly11_8h2b+" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly10_1a1d" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly10_1a1e" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly09_8h4d" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly09_8h3c+" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly08_2b6f" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly08_2b7g+" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly07_8h4d" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly06_2b6f" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly06_2b7g+" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly05_8h4d" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly04_2b6f" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly04_2b7g+" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly03_8h4d" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "Early3eBlitz_Melee" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly36_1f1g+" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly36_1f1g" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly35_1h1g" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly35_2a2b" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly34_1e2f" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly34_1e2d" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly33_1h1g" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly33_1h1f" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly32_1d2e" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly32_1d2c" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly31_1h1g" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly31_1h1f" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly30_3c2b" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly30_3c2d" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly29_2h2g" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly29_2h2f" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly28_2b1a" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly28_3c4b" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly27_2h2g" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly27_2h2f" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly26_2a1b" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly26_2a3b" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly25_2h2g" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly25_2i1g" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly24_2c2d" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly24_3c2b" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly23_2a1a" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly23_2a2b" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly22_2c2d" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly22_3a2b" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly21_2a1a" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly21_2a3a" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly20_1g2g" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly20_3c2b" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly19_1a1b" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly19_1i1h" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly18_2e1g" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly18_2e3g+" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly17_1b2b" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly17_2i1g" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly16_2b1a" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly16_2b1c" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly15_1c2b" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly15_2i1g" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly14_1a1c" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly14_2b1c" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly13_2i1g" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly13_8h4d" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly12_2b6f" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly12_2b7g+" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly11_8h4d" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly11_8h2b+" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly10_1a1d" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly10_1a1e" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly09_8h4d" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly09_8h3c+" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly08_2b6f" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly08_2b7g+" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly07_8h4d" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly06_2b6f" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly06_2b7g+" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly05_8h4d" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly04_2b6f" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly04_2b7g+" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly03_8h4d" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "EarlyClimbingSilver_Melee" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly36_3h2i" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly36_3h3i" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly35_3a4a" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly35_3i2h" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly34_3g2h" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly34_3g4h" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly33_2c3c" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly33_2c2d" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly32_4b3c" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly32_4b2d" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly31_3a4a" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly31_3a3b" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly30_N*1a" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly30_N*1b" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly29_3a4a" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly29_3a3b" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly28_4b2d" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly28_4b1e" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly27_3a4a" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly27_3a3b" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly26_3c1a" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly26_3c1e" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly25_2a1a" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly25_2a2b" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly23_2a1a" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly23_2a3a" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly22_2c2d" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly22_3a2b" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly21_2a1a" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly21_2a3a" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly20_1g2h" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly20_3c2b" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly19_1a1b" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly19_1i1h" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly18_2e1g" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly18_2e3g+" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly17_1b2b" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly17_2i1g" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly16_2b1a" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly16_2b1c" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly15_1c2b" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly15_2i1g" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly14_1a1c" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly14_2b1c" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly13_2i1g" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly10_1a1d" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly10_1a1e" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly09_8h4d" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly07_8h4d" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly05_8h4d" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly04_2b6f" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly04_2b7g+" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly03_8h4d" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "MigiShikenBlitz_Melee" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly36_1f1g+" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly36_1f1g" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly35_1h1g" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly35_2a2b" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly34_1e2f" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly34_1e2d" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly33_1h1g" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly33_1h1f" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly32_1d2e" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly32_1d2c" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly31_1h1g" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly31_1h1f" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly30_3c2b" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly30_3c2d" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly29_2i1g" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly29_3a2a" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly28_2b1a" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly28_3c4b" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly27_2i1g" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly27_3a2a" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly26_2a1b" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly26_2a3b" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly25_2i1g" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly25_3a4a" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly24_2c2d" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly24_3c2b" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly23_2a1a" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly23_2a2b" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly22_2c2d" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly22_3a2b" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly21_2a1a" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly21_2a3a" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly20_1g2h" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly20_3c2b" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly19_1a1b" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly19_1i1h" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly18_2e1g" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly18_2e3g+" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly17_1b2b" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly17_2i1g" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly16_2b1a" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly16_2b1c" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly15_1c2b" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly15_2i1g" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly14_1a1c" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly14_2b1c" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly13_2i1g" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly13_8h4d" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly12_2b6f" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly12_2b7g+" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly11_8h4d" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly11_8h2b+" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly10_1a1d" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly10_1a1e" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly09_8h4d" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly09_8h3c+" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly08_2b6f" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly08_2b7g+" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly07_8h4d" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly06_2b6f" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly06_2b7g+" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly05_8h4d" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly04_2b6f" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly04_2b7g+" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly03_8h4d" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "SodebishaBlitz_Melee" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly36_1f1g+" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly36_1f1g" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly35_1h1g" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly35_2b1a" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly34_1e2f" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly34_1e2d" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly33_1h1g" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly33_1h1f" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly32_1d2e" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly32_3c2b" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly31_1h1g" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly31_1h1f" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly30_3c6f" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly30_3c7g+" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly29_2h3h" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly29_2h4h" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly28_2b1a" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly28_2c2d" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly27_2h3h" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly27_2h4h" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly26_2a1b" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly26_2a3b" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly25_2h3h" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly25_2h4h" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly24_2c2d" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly24_3c2b" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly23_2a1a" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly23_2a2b" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly22_2c2d" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly22_3a2b" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly21_2a1a" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly21_2a3a" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly20_1g2h" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly20_1g2g" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly19_1a1b" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly19_1i1h" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly18_2e1g" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly18_2e3g+" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly17_1b2b" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly17_2i1g" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly16_2b1a" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly16_2b1c" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly15_1c2b" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly15_2i1g" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly14_1a1c" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly14_2b1c" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly13_2i1g" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly13_7f8e" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly12_2b6f" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly12_2b7g+" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly11_7f8e" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly11_8h4d" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly10_1a1d" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly10_1a1e" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly09_7f8e" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly09_8h4d" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly08_2b6f" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly08_2b7g+" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly07_7f8e" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly07_8h4d" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly06_2b6f" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly06_2b7g+" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly05_8h4d" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly04_2b6f" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly04_2b7g+" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly03_8h4d" = 'True
  ValidTransition "Start" "EarlyIshidaMelee_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "EarlyIshidaMelee" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly36_1g1h" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly36_1g2g" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly35_2i2g" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly35_2i2e" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly34_1f2g+" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly34_1f2g" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly33_2h3h" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly33_2h4h" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly32_1e2f" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly32_3c2e" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly31_2i2f" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly31_2i2e" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly30_3c2e" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly30_3i4i" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly29_2h3h" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly29_2h4h" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly28_3c2e" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly28_3i2i" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly27_2a2b" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly27_2h1h" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly26_3b2a" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly26_3c2e" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly25_2b3a" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly25_2b1b" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly24_3c2e" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly24_3i2i" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly23_2c1b" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly23_2c3b" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly22_2i2h" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly22_3c2e" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly21_2h1h" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly21_2h4h" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly20_1i1h" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly20_3c2e" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly19_2h1h" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly19_2i1g" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly18_1h2i" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly18_1h2h" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly17_2h1h" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly17_2i1g" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly16_1g2h" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly16_1g2g" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly15_1i1g" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly15_2h1h" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly14_3c2e" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly14_5a4b" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly13_1i1g" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly13_1i1f" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly12_5a5b" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly12_5a6b" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly11_1i1g" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly11_1i1f" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly10_1a1b" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly10_1a1c" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly09_1f1e" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly09_8h4d" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly08_2b6f" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly08_2b7g+" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly07_8h4d" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly06_2b6f" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly06_2b7g+" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly05_8h4d" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly04_2b6f" = 'True
  ValidTransition "Start" "UreshinoMelee_DevPly04_2b7g+" = 'True
  ValidTransition "Start" "UreshinoMelee" = 'True
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
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly35_2f6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2e","7d8d","2e2f","8d7d","2f6f","7d8d","6f6c+","P*6f","6c6a","8d5d","6a4a","3b4a"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly35_2f2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2e","7d8d","2e2f","8d7d","2f2d","3c2d","8h2b+","7d8d","2b3b","4a3b","2g1f","8d5d"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly33_2e3e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2e","7d8d","2e3e","P*6f","8h6f","4c4d","6f8d","3d3e","R*3b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly33_2e5e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2e","7d8d","2e5e","8d5d","5e5d","P*6f","R*3b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly31_2f6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f6f","7d8d","6f6c+","P*6f","6c6a","8d5d","6a4a","3b4a"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly31_2f2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2d","3c2d","8h2b+","7d8d","2b3b","4a3b","2g1f","8d5d"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly29_6g6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","6g6f","8d5d","6f6e","P*6f","8h6f","4c4d","S*2d","2c2d"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly29_7h7g"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","7h7g","8d5d","S*4d","4c4d","2g1f","5d8d","2h2f","8d5d"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly27_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","2h2d","3c2d","8h2b+","8b8d","2b3b","4a3b","S*1e","2d1e"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly27_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","2h2c+","3c2c","8h2b+","2c2b","P*2b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly25_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","2h2d","3c2d","8h2b+","8b8d","2b3b","4a3b","S*3e","3d3e"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly25_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","2h2c+","3c2c","8h2b+","2c2b","P*2b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly24_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","2b6f","P*4b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly24_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","2b7g+","P*4b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly23_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","2h2d","2b8h+","P*4b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly23_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","2h2c+","3b2c","P*4b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly22_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","2b6f","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly22_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","2b7g+","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly21_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","2h2d","2b8h+","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly21_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","2h2c+","3b2c","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly20_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","2b6f","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly20_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","2b7g+","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly19_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","2h2d","2b8h+","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly19_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","2h2c+","3b2c","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly18_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","2b6f","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly18_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","2b7g+","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly17_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","2h2d","2b8h+","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly17_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","2h2c+","3b2c","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly16_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","2b6f","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly16_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","2b7g+","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly15_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","2h2d","2b8h+","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly15_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","2h2c+","3b2c","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly14_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","2b6f","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly14_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","2b7g+","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly13_2d1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d1d","2b8h+","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly13_2d2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2c+","3b2c","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly12_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","2b6f","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly12_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","2b7g+","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly11_2h2e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2e","2b8h+","2e2f","8h7h","6i7h","8b8d","B*6b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly11_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7h7g","8b8d","B*6b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly10_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly10_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2b7g+","7h7g","8b8d","B*6b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly09_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7h7g","8b8d","B*6b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly09_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h3c+","3b3c","2h2f","8b8d","2f5f","8d5d","5f5d","5c5d","R*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly08_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly08_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2b7g+","7h7g","8b8d","B*6b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly07_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","8b8d","B*6b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly07_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h3c+","2b3c","2h2f","3c9i+","2f5f","9i8i","5f5c+","8b5b","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly06_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly06_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly05_8h4d"), ["7g7f","3c3d","2g2f","8c8d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly05_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly04_2b6f"), ["7g7f","3c3d","2g2f","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly04_2b7g+"), ["7g7f","3c3d","2g2f","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Aigakari_StratifiedEquilibrium"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2e","7d8d","2e2f","8d7d","8h3c+"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly36_1a1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","3b3c","B*1f","1a1b","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly36_1c1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","3b3c","B*1f","1c1d","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly35_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","3b3c","2h2d","2c2d","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly35_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","3b3c","2h2c+","2b2c","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly34_4b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","4b3c","2h2f","8b8d","2f5f","8e8f","5f2f","8f8g+","8h8g"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly34_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","B*1f","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly33_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","2h2d","2c2d","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly33_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","2h2c+","2b2c","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly32_4b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","4b3c","2h2f","8b8d","2f5f","8e8f","5f2f","8f8g+","8h8g"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly32_S*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*1f","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly31_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","2h2d","2c2d","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly31_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","2h2c+","2b2c","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly30_4b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","4b3c","3e2d","2c2d","2h2f","8b8d","2f5f","8e8f","5f2f"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly30_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","B*1f","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly29_2d1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d1d","1c1d","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly29_2d2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2c+","2b2c","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly28_4b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","4b3c","2d2b+","3b2b","3e2d","3c2d","S*3e","3d3e","S*1e"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly28_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","B*1f","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly27_2h2e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2e","2d2e","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly27_3e4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","3e4d","4c4d","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly26_4b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","4b3c","2d2c+","2b2c","2h2c+","3b2c","3e2d","2c2d","S*2b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly26_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","B*1f","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly25_3e2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","3e2d","2c2d","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly25_3e4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","3e4d","4c4d","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly24_4b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","4b3c","3e2d","2c2d","2e2d","8b8d","2d2c+","2b2c","2h2c+"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly24_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","B*1f","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly23_P*3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","P*3c","8b8d","3c3b+","4b3b","2h2f","8e8f","2e2d","8f8g+"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly23_P*3e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","P*3e","8b8d","2h2f","8e8f","2e2d","8f8g+","8h8g","3d3e","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly22_4b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","4b3c","4f4e","8b8d","4e3d","3c3d","2h2f","8e8f","2f5f"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly22_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","B*1f","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly21_P*3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","P*3c","8b8d","3c3b+","4b3b","2h2f","8e8f","2f5f","8f8g+"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly21_P*3e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","P*3e","8b8d","2h2f","8e8f","2f5f","8f8g+","8h8g","3d3e","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly20_4b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","4b3c","2h2f","8b8d","2f5f","8d5d","5f5d","5c5d","R*3b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly20_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","B*1f","P*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly19_P*3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","P*3c","8b8d","3c3b+","4b3b","2h2f","8d5d","2f5f","5d5f"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly19_P*3e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","P*3e","8b8d","2h2f","8d5d","2f5f","5d5f","5g5f","3d3e","R*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly18_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","B*1f","S*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly18_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","B*1h","S*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly17_B*1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","B*1b","1a1b","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly17_B*1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","B*1d","1c1d","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly16_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","B*1f","B*6b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly16_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","B*1h","B*6b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly15_B*1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","B*1b","1a1b","2h2f","8b8d","2f5f","8d5d","5f5d","5c5d","R*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly15_B*1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","B*1d","1c1d","2h2f","8b8d","2f5f","8d5d","5f5d","5c5d","R*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly14_5a5b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","5a5b","2h2f","8b8d","2f5f","8d5d","5f5d","5c5d","R*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly14_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","B*1f","B*6b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly13_B*1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","B*1b","1a1b","2h2f","8b8d","2f5f","8d5d","5f5d","5c5d","R*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly13_B*1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","B*1d","1c1d","2h2f","8b8d","2f5f","8d5d","5f5d","5c5d","R*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly12_5a5b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","5a5b","2h2f","8b8d","2f5f","8d5d","5f5d","5c5d","R*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly12_5a6b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","5a6b","2h2f","8b8d","2f5f","8d5d","5f5d","5c5d","R*6b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly11_B*1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","B*1b","1a1b","2h2f","8b8d","2f5f","8d5d","5f5d","5c5d","R*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly11_B*1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","B*1d","1c1d","2h2f","8b8d","2f5f","8d5d","5f5d","5c5d","R*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly10_3b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b3c","B*6b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly10_5a4b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","5a4b","2b3b","4a3b","2h2f","8b8d","2f5f","8d5d","5f5d"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly09_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7h7g","8b8d","B*6b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly09_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h3c+","3b3c","2h2f","8b8d","2f5f","8d5d","5f5d","5c5d","R*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly08_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly08_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2b7g+","7h7g","8b8d","B*6b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly07_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","8b8d","B*6b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly07_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h3c+","2b3c","2h2f","3c9i+","2f5f","9i8i","5f5c+","8b5b","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly06_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly06_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly05_8h4d"), ["7g7f","3c3d","2g2f","8c8d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly05_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly04_2b6f"), ["7g7f","3c3d","2g2f","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly04_2b7g+"), ["7g7f","3c3d","2g2f","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","3b3c","B*1f","8b8d","P*4b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly40_8b8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","2e2d","4a4b","B*1f","8b8f","7g8f","2c2d","R*3b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly39_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","2e2d","4a4b","B*1b","1a1b","2d2c+","3b2c","2h2c+","3c2c","S*1d","1c1d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly39_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","2e2d","4a4b","B*1d","1c1d","2d2c+","3b2c","2h2c+","3c2c","S*1b","1a1b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly38_8b8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","2e2d","8b8f","7g8f","2c2d","R*3b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly37_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","B*1b","1a1b","2h2f","8b8d","5f5e","5d5e","2e2d","2c2d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly37_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","B*1d","1c1d","2h2f","8b8d","5f5e","5d5e","2e2d","2c2d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly36_8b8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","8b8f","7g8f","3d3e","R*3b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly35_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","B*1b","1a1b","2h2f","8b8d","5f5e","5d5e","2e2d","2c2d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly35_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","B*1d","1c1d","2h2f","8b8d","5f5e","5d5e","2e2d","2c2d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly34_8b8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","8b8f","7g8f","3d3e","R*3b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly33_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","B*1b","1a1b","2h2f","8b8d","5f5e","5d5e","2e2d","2c2d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly33_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","B*1d","1c1d","2h2f","8b8d","5f5e","5d5e","2e2d","2c2d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly32_8b8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","8b8f","7g8f","3d3e","R*3b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly31_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","B*1b","1a1b","2h2f","8b8d","5f5e","5d5e","2e2d","2c2d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly31_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","B*1d","1c1d","2h2f","8b8d","5f5e","5d5e","2e2d","2c2d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly30_8f7f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f7f","7g7f","3d3e","R*3b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly30_8f9f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f9f","9g9f","3d3e","R*3b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly28_8b8e"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8e","8f8e","3d3e","R*3b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly27_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","B*1b","1a1b","2h2f","8f8g+","8h7i","8g7g","8i7g","8b8d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly27_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","B*1d","1c1d","2h2f","8f8g+","8h7i","8g7g","8i7g","8b8d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly25_7g8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","7g8f","8e8f","5f5e","8f8g+","8h7i","8b8d","5e5d","S*6h"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly25_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","B*1b","1a1b","5f5e","8b8d","5e5d","8e8f","5d5c+","8f8g+"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly23_7g8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","7g8f","8e8f","5f5e","8f8g+","8h7i","8b8d","5e5d","S*6h"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly23_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","B*1b","1a1b","5f5e","8b8d","5e5d","8e8f","5d5c+","8f8g+"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly21_7g8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7g8f","8e8f","5f5e","8f8g+","7h6g","5d5e","8i7g","8g7g"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly21_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","B*1b","1a1b","5f5e","8b8d","5e5d","8e8f","5d5c+","6b5c"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly19_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","B*1b","1a1b","5f5e","5d5e","7f7e","3d3e","2g2f","5e5f"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly19_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","B*1d","1c1d","5f5e","5d5e","7f7e","3d3e","2g2f","5e5f"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly14_2b5e"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","2b5e","5f5e","3d3e","5e5d","5c5d","B*1e","2a3c","1e3c+"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly14_2b6f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","2b6f","7g6f","3d3e","6f5e","2a3c","5e6d","P*6g","P*4b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly12_2b5e"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","2b5e","5f5e","3d3e","5e5d","5c5d","B*1e","2a3c","1e3c+"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly12_2b6f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","2b6f","7g6f","3d3e","6f5e","2a3c","5e6d","5c5d","P*4b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly10_2b5e"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","2b5e","5f5e","2a3c","B*6b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly10_2b6f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","2b6f","7g6f","2a3c","B*6b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly08_2b6f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","2b6f","7g6f","8b5b","B*6b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly03_8h4d"), ["7g7f","8c8d","8h4d","4c4d","2h5h","8b5b","8i7g","5c5d","2g2f","5d5e"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly03_8h3c+"), ["7g7f","8c8d","8h3c+","2b3c","P*5b"]),
    (("Start", "Yagura_StratifiedEquilibrium"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","2e2d","4a4b","B*1f","2b3c","8h7i"," *1b","2d2c+","3b2c","P*1b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly38_2a3c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","3c2d","6f6e","2a3c","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly38_2d1e"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","3c2d","6f6e","2d1e","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly37_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","3c2d","7g8f","8e8f","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly37_7g5i"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","3c2d","7g5i","6i7h","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly36_1a1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","1a1b","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly36_1a1c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","1a1c","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly35_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","7g8f","8e8f","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly35_7g5i"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","7g5i","6i7h","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly34_3a2b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","3a2b","2e2d","4i4g+","2d2c+","3b2c","P*2b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly34_3c2d"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","3c2d","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly33_6i5i"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","6i5i","4i5i+","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly33_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","7g8f","8e8f","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly32_3a2b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","3a2b","2e2d","3c2d","6f6e","2a3c","7g3c+","2d3c","S*1c"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly32_3c2d"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","3c2d","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly31_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","7g8f","8e8f","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly31_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","S*1b","1a1b","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly30_B*1e"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","B*1e","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly30_B*1g"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","B*1g","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly29_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","7g8f","8e8f","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly29_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","S*1b","1a1b","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly28_4b3c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3c","2f2e","R*2f","S*2g","2f2g+","2h2g","3d3e","R*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly28_4b4a"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b4a","G*4b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly27_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","7g8f","8e8f","S*3a","2b3a","G*3c","2a3c","2g2f","8f8g+"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly27_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","S*1b","1a1b","G*3c","2b3c","2g2f","S*1g","2i1g","3c4d"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly26_4b3a"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","4b3a","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly26_4b3c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","4b3c","1f1e","1d1e","1i1e","1a1e","2i1g","1e1g+","2h1g"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly25_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","7g8f","8e8f","S*3a","2b3a","G*3c","2a3c","2g2f","8f8g+"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly25_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","S*1b","1a1b","G*3c","2b3c","2g2f","S*2g","2h2g","3d3e"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly24_4b3a"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","4b3a","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly24_4b3c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","4b3c","6f6e","4c4d","S*2d","2c2d","G*2c","2b2c","2g2f"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly23_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","7g8f","8e8f","S*3a","2b3a","G*3c","2a3c","2g2f","8f8g+"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly23_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","S*1b","1a1b","G*3c","2b3c","2g2f","S*2g","1h2g","3d3e"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly22_4b3a"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","4b3a","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly22_4b3c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","4b3c","6f6e","4c4d","S*2d","2c2d","G*2c","2b2c","2g2f"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly21_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","7g8f","8e8f","S*3a","2b3a","G*3c","2a3c","2g2f","8f8g+"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly21_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","S*1b","1a1b","G*3c","2b3c","2g2f","S*2g","1h2g","3d3e"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly20_4b3a"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","4b3a","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly20_4b3c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","4b3c","6f6e","4c4d","S*2d","2c2d","G*2c","2b2c","2g2f"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly19_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","7g8f","8e8f","S*3a","2b3a","G*3c","2a3c","2g2f","8b8d"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly19_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","S*1b","1a1b","G*3c","2b3c","2g2f","8b8d","2f2e","8d5d"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly18_4b3a"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","4b3a","G*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly18_4b3c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","4b3c","6f6e","4c4d","S*2d","2c2d","G*2c","2b2c","2g2f"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly17_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","7g8f","8e8f","S*3a","2b3a","G*3c","2a3c","2g2f","8b8d"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly17_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","S*1b","1a1b","G*3c","2b3c","2g2f","8b8d","2f2e","8d5d"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly16_4b3c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","4b3c","6f6e","4c4d","S*2d","2c2d","G*2c","3b2c","2g2f"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly16_4b4a"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","4b4a","G*4b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly15_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","7g8f","8e8f","G*3c","2b3c","7f7e","8b8d","8i7g","8d5d"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly15_G*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","G*1b","1a1b","7f7e","8b8d","2g2f","8d5d","3h2g","5d8d"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly13_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","7g8f","8e8f","8g8f","8b8d","P*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly11_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","7g8f","8e8f","8g8f","8b8d","P*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly09_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","7g8f","8e8f","8g8f","8b8d","P*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","3c2d","6f6e","6i7h","G*3b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly29_3c2b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f2f","B*3c","2f2e","3c2b","2e3e","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly29_3c2d+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f2f","B*3c","2f2e","3c2d+","2e2d","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly27_2i1g"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f2f","2i1g","2f3e","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly27_3e2e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f2f","3e2e","2f2e","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly26_1f3h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f3h","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly26_1f4i"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f4i","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly25_1i1g"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","1i1g","1f4i","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly25_2i1g"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","2i1g","1f4i","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly24_1g2h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g2h","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly24_1g3i"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g3i","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly23_1i1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","1i1h","1g3i","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly23_3d4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d4d","1g4d","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly22_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2b6f","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly22_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2b7g+","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly21_3f3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3c+","3b3c","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly21_3f3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3c","3b3c","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly20_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","2b6f","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly20_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","2b7g+","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly19_3d4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d4d","2b4d","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly19_3d1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d1d","1c1d","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly18_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","2b6f","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly18_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","2b7g+","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly17_3d4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","3d4d","2b4d","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly17_3d1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","3d1d","1c1d","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly16_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","2b6f","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly16_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","2b7g+","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly15_2d1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d1d","2b8h+","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly15_2d2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d2c+","3b2c","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly14_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","2b6f","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly14_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","2b7g+","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly13_2d1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","2d1d","2b8h+","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly13_2d2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","2d2c+","3b2c","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly12_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","2b6f","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly12_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","2b7g+","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly11_2h2e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2e","2b8h+","2e2f","8h7h","6i7h","8b8d","B*6b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly11_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7h7g","8b8d","B*6b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly10_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly10_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2b7g+","7h7g","8b8d","B*6b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly09_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7h7g","8b8d","B*6b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly09_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h3c+","3b3c","2h2f","8b8d","2f5f","8d5d","5f5d","5c5d","R*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly08_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly08_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2b7g+","7h7g","8b8d","B*6b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly07_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","8b8d","B*6b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly07_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h3c+","2b3c","2h2f","3c9i+","2f5f","9i8i","5f5c+","8b5b","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly06_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly06_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly05_8h4d"), ["7g7f","3c3d","2g2f","8c8d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly05_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly04_2b6f"), ["7g7f","3c3d","2g2f","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly04_2b7g+"), ["7g7f","3c3d","2g2f","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f2f","B*3c","2f2e","3c5a+"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly28_B*1e"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","8b8f","7g8f","B*1e","S*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly28_B*1g"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","8b8f","7g8f","B*1g","S*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly27_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","8b8f","7g4d","3c4d","S*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly27_7g3c+"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","8b8f","7g3c+","2a3c","S*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly26_B*1e"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","B*1e","S*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly26_B*1g"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","B*1g","S*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly25_7f7d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f7d","7c7d","S*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly25_7f7c+"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f7c+","6b7c","S*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly24_B*1e"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","B*1e","S*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly24_B*1g"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","B*1g","S*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly23_7f8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","7f8f","8e8f","S*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly23_7f7d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","7f7d","7c7d","S*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly22_B*1e"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","B*1e","P*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly22_B*1g"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","B*1g","P*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly21_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7g8f","8e8f","S*3a","4b3a","2g2f","8b8d","2f2e","8d5d"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly21_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7g4d","3c4d","S*3a","4b3a","2g2f","8b8d","2f2e","8d5d"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly20_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2a3c","7g3c+","2b3c","N*5d","5c5d","S*3a","4b3a","2g2f"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly20_4b3a"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","4b3a","S*2b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly19_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","7g8f","8e8f","S*3a","2b3a","2g2f","8b8d","2f2e","8d5d"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly19_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","7g4d","4c4d","S*3a","2b3a","2g2f","8b8d","2f2e","8d5d"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly18_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","2a3c","7g3c+","2b3c","N*5d","5c5d","S*3a","4b3a","2g2f"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly18_4b3a"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","4b3a","S*2b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly17_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","7g8f","8e8f","S*3a","2b3a","2g2f","8b8d","2f2e","8d5d"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly17_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","7g4d","4c4d","S*3a","2b3a","2g2f","8b8d","2f2e","8d5d"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly16_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","2a3c","7g3c+","2b3c","N*5d","5c5d","S*3a","4b3a","2g2f"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly16_4b3a"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","4b3a","S*2b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly15_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","7g8f","8e8f","S*3a","2b3a","2g2f","8b8d","2f2e","8d5d"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly15_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","7g4d","4c4d","S*3a","2b3a","2g2f","8b8d","2f2e","8d5d"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly14_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","2a3c","7g3c+","2b3c","N*5d","5c5d","S*5a","6a5a","2g2f"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly14_2b6f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","2b6f","7g6f","8b5b","6f1a+","2a3c","1a3c","3b3c","N*5d"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly13_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","7g8f","8e8f","S*3c","2b3c","8i7g","3c7g+","6h7g","8b8d"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly13_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","7g4d","2b4d","S*3c","4d3c","8i7g","3c7g+","6h7g","8b8d"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly12_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","2a3c","S*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly12_2b6f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","2b6f","S*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly11_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","7g8f","8e8f","8i7g","2b7g+","6h7g","8b8d","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly11_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","7g4d","2b4d","8i7g","4d7g+","6h7g","8b8d","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly10_2b6f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","2b6f","7g6f","8b5b","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly10_2b7g+"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","2b7g+","6h7g","8b8d","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly09_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","7g8f","8e8f","8i7g","2b7g+","6h7g","8b8d","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly09_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","7g4d","2b4d","8i7g","4d7g+","6h7g","8b8d","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly08_2b6f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","2b6f","7g6f","8b5b","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly08_2b7g+"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","2b7g+","6h7g","8b8d","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly07_8h4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h4d","2b4d","8i7g","4d7g+","6h7g","8b8d","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly07_8h3c+"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h3c+","2b3c","8i7g","3c7g+","6h7g","8b8d","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly06_2b6f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly06_2b7g+"), ["7g7f","3c3d","7i6h","8c8d","2h7h","2b7g+","6h7g","8b5b","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly05_8h4d"), ["7g7f","3c3d","7i6h","8c8d","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","6h7g","8b5b","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly05_8h3c+"), ["7g7f","3c3d","7i6h","8c8d","8h3c+","2b3c","2h5h","3c9i+","8i7g","9i7g","6h7g","8b5b","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly04_2b6f"), ["7g7f","3c3d","7i6h","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly04_2b7g+"), ["7g7f","3c3d","7i6h","2b7g+","6h7g","8b5b","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","8b8f","7g8f","3c4d","S*4b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly44_2b5e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","7h6h","6e7g+","6h7g","8b8d","5f4f","6c5d","5e5d","6d6e","7g6f","2b5e","S*2b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly44_2b6f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","7h6h","6e7g+","6h7g","8b8d","5f4f","6c5d","5e5d","6d6e","7g6f","2b6f","S*2b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly43_4f6f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","7h6h","6e7g+","6h7g","8b8d","5f4f","6c5d","5e5d","6d6e","4f6f","2b6f","S*2b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly43_4f4d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","7h6h","6e7g+","6h7g","8b8d","5f4f","6c5d","5e5d","6d6e","4f4d","2b4d","S*2b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly42_2b6f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","7h6h","6e7g+","6h7g","8b8d","5f4f","6c5d","5e5d","2b6f","S*2b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly42_2b7g+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","7h6h","6e7g+","6h7g","8b8d","5f4f","6c5d","5e5d","2b7g+","S*2b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly41_4f4e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","7h6h","6e7g+","6h7g","8b8d","5f4f","6c5d","4f4e","5d4e","N*2c"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly41_4f4d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","7h6h","6e7g+","6h7g","8b8d","5f4f","6c5d","4f4d","2b4d","N*2c"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly40_2c2d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","7h6h","6e7g+","6h7g","8b8d","5f4f","2c2d","N*2c"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly40_4a5b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","7h6h","6e7g+","6h7g","8b8d","5f4f","4a5b","N*2c"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly39_N*1c"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","7h6h","6e7g+","6h7g","8b8d","N*1c","2a1c","5f2f","6c5d","5e5d","2b7g+","S*2b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly39_N*1e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","7h6h","6e7g+","6h7g","8b8d","N*1e","1d1e","5f2f","6c5d","5e5d","2b7g+","S*2b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly38_2c2d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","7h6h","6e7g+","6h7g","2c2d","N*2c"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly38_4a5b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","7h6h","6e7g+","6h7g","4a5b","N*2c"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly36_6c5d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","7h6h","6c5d","5e5d","2b8h+","S*2b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly36_6e5g+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","7h6h","6e5g+","6h5g","8b8d","N*2c"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly35_8h7g"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","8h7g","6e7g+","7h7g","8b8d","N*2c"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly35_8i7g"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","8i7g","6e5g+","5f5g","8b8d","N*2c"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly34_6c5d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","6c5d","5e5d","2b8h+","S*2b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly34_8b8f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","8b8f","8g8f","6c5d","R*3b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly32_6c5d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","6c5d","5e5d","2b8h+","S*2b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly32_8b8f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","8b8f","8g8f","6c5d","R*3b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly30_6c5d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","6c5d","5e5d","2b8h+","S*2b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly30_8b8f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8b8f","8g8f","6c5d","R*3b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly28_6c5d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","6c5d","5e5d","2b8h+","S*2b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly28_8b8f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","8b8f","8g8f","6c5d","R*3b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly26_6c5d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","6c5d","8g8f","5d6e","R*3b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly26_8f9f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f9f","9g9f","6c5d","R*3b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly24_6c5d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","6c5d","P*3b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly24_8b8e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8e","P*3b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly22_6c5d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","6c5d","5e5d","2b8h+","S*2b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly09_8h4d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","8h4d","2b4d","8i7g","4d7g+","7h7g","8b8d","B*6b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly09_8h3c+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","8h3c+","2b3c","8i7g","3c7g+","7h7g","8b8d","B*6b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly08_2b5e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","2b5e","8h5e","8b8d","B*6b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly08_2b6f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly07_8h4d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","8h4d","2b4d","7i8h","4d8h+","5h8h","8b8d","B*6b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly07_8h3c+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","8h3c+","2b3c","7i8h","3c8h+","5h8h","8b8d","B*6b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly06_2b5e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","2b5e","8h5e","8b5b","B*6b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly06_2b6f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly05_8h4d"), ["7g7f","3c3d","5g5f","8c8d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly05_8h3c+"), ["7g7f","3c3d","5g5f","8c8d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly04_2b5e"), ["7g7f","3c3d","5g5f","2b5e","8h5e","8b5b","B*6b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly04_2b6f"), ["7g7f","3c3d","5g5f","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SenteNakabisha_IronMino_Equilibrium"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","4a3b","5f5e","5a4b","3i3h","7a6b","5i4h","6c6d","4h3i","3a4a","3i2h","4b3a","4i3i","6b6c","6i6h","8e8f","8g8f","8b8f","P*8g","8f8b","5h5f","7c7d","6h5h","8a7c","1g1f","1c1d","5h5g","7c6e","7h6h","6e7g+","6h7g","8b8d","5f4f","6c5d","5e5d","6d6e","7g6f","2b6f","S*2b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly48_5b4b"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","3b4b","2h2a+","2b3a","2a2c","4b3b","2c3b","3a3b","G*4a","5b4b","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly48_5b5a"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","3b4b","2h2a+","2b3a","2a2c","4b3b","2c3b","3a3b","G*4a","5b5a","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly47_P*2b"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","3b4b","2h2a+","2b3a","2a2c","4b3b","2c3b","3a3b","P*2b","S*5i","6h5i","5d5e","N*7d","7c7d","S*7a","8b7a","G*7b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly47_P*2c"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","3b4b","2h2a+","2b3a","2a2c","4b3b","2c3b","3a3b","P*2c","S*5i","6h5i","5d5e","N*7d","7c7d","S*7a","8b7a","G*7b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly46_1a1b"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","3b4b","2h2a+","2b3a","2a2c","4b3b","2c3b","1a1b","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly46_3a4b"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","3b4b","2h2a+","2b3a","2a2c","4b3b","2c3b","3a4b","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly45_2c1c"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","3b4b","2h2a+","2b3a","2a2c","4b3b","2c1c","1a1c","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly45_2c3c"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","3b4b","2h2a+","2b3a","2a2c","4b3b","2c3c","3b3c","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly44_1a1b"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","3b4b","2h2a+","2b3a","2a2c","1a1b","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly44_1a1c"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","3b4b","2h2a+","2b3a","2a2c","1a1c","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly43_2a2b"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","3b4b","2h2a+","2b3a","2a2b","3a2b","N*7d","7c7d","S*7a","8b7a","5f5e","N*5f"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly43_2a1b"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","3b4b","2h2a+","2b3a","2a1b","1a1b","N*7d","7c7d","S*7a","8b7a","5f5e","N*5f"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly42_1a1b"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","3b4b","2h2a+","1a1b","2a8a","8b8a","S*7b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly42_3d3e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","3b4b","2h2a+","3d3e","2a8a","8b8a","S*7b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly41_2h2d"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","3b4b","2h2d","2c2d","S*7a","8b7a","5f5e","S*5i","6h5i","S*5h","S*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly41_2h2c+"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","3b4b","2h2c+","2b2c","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly40_2a3c"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","2a3c","4b5b","S*5i","R*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly40_2b3a"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","2b3a","4b5b","S*5i","R*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly39_2h2d"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","2h2d","2c2d","S*7a","8b9b","7a8b+","7b8b","5c7a+","S*5i"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly39_2h2c+"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","2h2c+","2b2c","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly38_2b3a"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","2b3a","5c3a+","5a3a","2h2f","S*5i","6h5i","B*9e","8i7g"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly38_S*1e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","S*1e","1f1e","5a5c","2h2f","5d5e","5f5e","B*9e","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly37_2h2d"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","2h2d","2c2d","S*7a","8b7a","B*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly37_2h2c+"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","2h2c+","2b2c","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly36_1a1b"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","1a1b","2b3b","3a3b","2h2f","S*5i","6h5i","5d5e","S*7a"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly36_1a1c"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","1a1c","2b3b","3a3b","2h2f","S*5i","6h5i","5d5e","S*7a"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly35_2h2d"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","2h2d","2b8h+","7i8h","2c2d","S*7a","8b7a","B*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly35_2h2c+"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","2h2c+","2b8h+","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly34_5e4e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e4e","8h2b+","3a2b","3g4e","S*5i","R*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly34_5e3e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e3e","8h2b+","3a2b","3f3e","S*5i","R*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly33_2h2e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","2h2e","5e2e","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly33_2h2d"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","2h2d","2c2d","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly32_2b4d"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","2b4d","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly31_2h2d"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","2h2d","5e4f","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly31_2h2c+"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","2h2c+","3b2c","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly30_4d3e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d3e","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly29_2h2d"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2h2d","2c2d","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly29_2h2c+"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2h2c+","3b2c","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly28_4d3e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","4d3e","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly27_2h2d"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","2h2d","2c2d","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly27_2h2c+"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","2h2c+","3b2c","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly26_4d3e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","4d3e","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly25_2h2d"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","2h2d","2c2d","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly25_2h2c+"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","2h2c+","3b2c","P*8b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly24_4d3e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","4d3e","P*7b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly23_2h2d"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","2h2d","2c2d","P*7b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly23_2h2c+"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","2h2c+","3b2c","P*7b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly22_4d3e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","4d3e","P*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly21_2h2d"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","2h2d","2c2d","P*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly21_2h2c+"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","2h2c+","3b2c","P*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly20_4d3e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","4d3e","P*5b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly19_2d1d"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d1d","1c1d","P*5b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly19_2d2c+"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2c+","3b2c","P*5b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly18_4d3e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","4d3e","P*5b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly17_2h2e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2e","2d2e","8h4d","2b4d","S*4b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly17_4f3e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","4f3e","4d3e","8h2b+","3a2b","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly16_4d3e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","4d3e","8h2b+","3a2b","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly15_4f3e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","4f3e","4d3e","8h2b+","3a2b","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly15_4f5e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","4f5e","4d5e","8h5e","2b5e","S*4b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly14_4d3e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4d3e","8h2b+","3a2b","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly13_8h5e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","8h5e","4d5e","2h2f","B*9e","7i6h","9e6h+","6i6h","S*4h","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly13_8h4d"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","8h4d","2b4d","S*4b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly12_2b6f"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","2b6f","8h6f","5c4d","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly12_2b7g+"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","2b7g+","8h7g","5c4d","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly11_8h5e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","8h5e","2b5e","2h2f","5e1i+","3h3g","1i3g","2i3g","S*4h","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly11_8h4d"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","8h4d","5c4d","2h2f","4d5e","3h2g","B*9e","7i6h","9e6h+"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly10_2b6f"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","2b6f","8h6f","3a2b","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly10_2b7g+"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","2b7g+","8h7g","3a2b","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly09_8h5e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","8h5e","2b5e","2h2f","5e1i+","3i2h","1i2h","2f2h","S*4h","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly09_8h4d"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly08_2b6f"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","2b6f","8h6f","3a2b","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly08_2b7g+"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","2b7g+","8h7g","3a2b","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly07_8h5e"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","8h5e","2b5e","2h2f","5e9i+","2f5f","9i8i","5f2f","8i7i"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly07_8h4d"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly06_2b6f"), ["7g7f","3c3d","2g2f","5c5d","2f2e","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly06_2b7g+"), ["7g7f","3c3d","2g2f","5c5d","2f2e","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly05_8h5e"), ["7g7f","3c3d","2g2f","5c5d","8h5e","2b5e","2h5h","5e9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly05_8h4d"), ["7g7f","3c3d","2g2f","5c5d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly04_2b6f"), ["7g7f","3c3d","2g2f","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly04_2b7g+"), ["7g7f","3c3d","2g2f","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "GoteGokigen_IronDefense_Equilibrium"), ["7g7f","3c3d","2g2f","5c5d","2f2e","8b5b","3g3f","7a6b","3i3h","6b5c","3h3g","5c4d","3g4f","4a3b","2e2d","2c2d","2h2d","P*2c","2d2h","5a6b","6i7h","6b7b","5i6h","7b8b","4i4h","6a7b","1g1f","1c1d","2i3g","4d5e","4f5e","5b5e","P*5f","5e5a","8h2b+","3a2b","B*5c","5a5b","5c4b+","3b4b","2h2a+","2b3a","2a2c","4b3b","2c3b","3a3b","G*4a","3b4a","P*8b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly46_4d3e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","8d8e","3g3f","8e5e","5g5e","5c4d","B*5c","3b4b","5c4b+","3a4b","G*5a","4d3e","R*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly46_B*1e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","8d8e","3g3f","8e5e","5g5e","5c4d","B*5c","3b4b","5c4b+","3a4b","G*5a","B*1e","R*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly45_5e4e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","8d8e","3g3f","8e5e","5g5e","5c4d","B*5c","3b4b","5c4b+","3a4b","5e4e","4d4e","R*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly45_5e3e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","8d8e","3g3f","8e5e","5g5e","5c4d","B*5c","3b4b","5c4b+","3a4b","5e3e","3d3e","R*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly43_5c4d+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","8d8e","3g3f","8e5e","5g5e","5c4d","B*5c","3b4b","5c4d+","4c4d","S*2b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly43_5c4d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","8d8e","3g3f","8e5e","5g5e","5c4d","B*5c","3b4b","5c4d","5d5e","S*2b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly41_5e4e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","8d8e","3g3f","8e5e","5g5e","5c4d","5e4e","4d4e","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly41_5e3e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","8d8e","3g3f","8e5e","5g5e","5c4d","5e3e","3d3e","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly40_B*1e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","8d8e","3g3f","8e5e","5g5e","B*1e","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly40_B*1g"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","8d8e","3g3f","8e5e","5g5e","B*1g","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly39_B*1b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","8d8e","3g3f","8e5e","B*1b","5e5g+","4h5g","1a1b","R*3b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly39_B*1c"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","8d8e","3g3f","8e5e","B*1c","5e5g+","1c5g+","5c4d","R*3b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly38_8e7e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","8d8e","3g3f","8e7e","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly38_8e8f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","8d8e","3g3f","8e8f","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly37_B*1b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","8d8e","B*1b","1a1b","5g5f","8e8d","5f2f","5d5e","7f7e","8d5d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly37_B*1c"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","8d8e","B*1c","2b1c","5g5f","8e8d","5f2f","5d5e","7f7e","8d5d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly36_1a1b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","1a1b","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly36_1a1c"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","1a1c","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly35_B*1b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","B*1b","1a1b","5g5f","5d5e","5f2f","8d5d","7f7e","5d8d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly35_B*1c"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","B*1c","2b1c","5g5f","5d5e","5f2f","8d5d","7f7e","5d8d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly34_7d7e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d7e","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly34_B*1e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","B*1e","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly33_B*1b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","B*1b","1a1b","5g5f","7d8d","5f2f","5d5e","7f7e","8d5d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly33_B*1c"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","B*1c","2b1c","5g5f","7d8d","5f2f","5d5e","7f7e","8d5d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly32_8d8f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d8f","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly32_8d8g+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d8g+","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly31_B*1b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","B*1b","1a1b","5h5f","5d5e","5f2f","8d5d","7f7e","5d8d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly31_B*1c"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","B*1c","2b1c","5h5f","5d5e","5f2f","8d5d","7f7e","5d8d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly30_8b8f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8f","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly30_8b8g+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8g+","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly29_B*1b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","B*1b","1a1b","5f5e","8b8d","5h5f","5d5e","5f2f","8d5d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly29_B*1c"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","B*1c","2b1c","5f5e","8b8d","5h5f","5d5e","5f2f","8d5d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly28_8b8f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","8b8f","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly28_8b8g+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","8b8g+","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly27_B*1b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","B*1b","1a1b","5f5e","8b8d","5h5f","5d5e","5f2f","8d5d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly27_B*1d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","B*1d","1c1d","5f5e","8b8d","5h5f","5d5e","5f2f","8d5d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly26_8b8f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","8b8f","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly26_8b8g+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","8b8g+","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly25_B*1b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","B*1b","1a1b","5f5e","8b8d","5h5f","5d5e","5f2f","8d5d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly25_B*1d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","B*1d","1c1d","5f5e","8b8d","5h5f","5d5e","5f2f","8d5d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly24_8b8f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","8b8f","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly24_8b8g+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","8b8g+","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly23_B*1b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","B*1b","1a1b","2g2f","8b8d","3h2g","8d5d","2g1f","5d8d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly23_B*1d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","B*1d","1c1d","2g2f","8b8d","3h2g","8d5d","2g1f","5d8d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly22_4b3c"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3c","5f5e","8b8d","5h5f","5c5d","5f2f","5d5e","B*1e"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly22_4b4a"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b4a","B*5b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly21_B*1b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","B*1b","1a1b","5f5e","8b8d","5h5f","5c5d","5f2f","5d5e"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly21_B*1d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","B*1d","1c1d","5f5e","8b8d","5h5f","5c5d","5f2f","5d5e"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly20_2b3a"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","2b3a","B*1e","2a3c","1e3c+","4b3c","N*3c"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly20_4b3a"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4b3a","B*4b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly19_B*1b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","B*1b","1a1b","5f5e","8b8d","5h5f","5c5d","5f2f","5d5e"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly19_B*1d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","B*1d","1c1d","5f5e","8b8d","5h5f","5c5d","5f2f","5d5e"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly18_5a5b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a5b","5f5e","8b8d","5h5f","5c5d","5f2f","5d5e","7f7e"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly18_8b8f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","8b8f","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly17_B*1b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","B*1b","1a1b","8i7g","8b8d","2g2f","8d5d","5f5e","5d8d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly17_B*1d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","B*1d","1c1d","8i7g","8b8d","2g2f","8d5d","5f5e","5d8d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly16_5a5b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","5a5b","8i7g","8b8d","2g2f","8d5d","5f5e","5d8d","5h5f"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly16_5a6b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","5a6b","8i7g","8b8d","2g2f","8d5d","5f5e","5d8d","5h5f"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly15_B*1b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","B*1b","1a1b","8i7g","8b8d","2g2f","8d5d","5f5e","5d8d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly15_B*1d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","B*1d","1c1d","8i7g","8b8d","2g2f","8d5d","5f5e","5d8d"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly14_5a4b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","5a4b","2b3a","4a3a","S*3c","2a3c","8i7g","8b8d","2g2f"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly14_5a5b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","5a5b","2b3a","4a3a","S*4a","3a4a","8i7g","8b8d","2g2f"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly13_8h4d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h4d","2b4d","8i7g","4d7g+","7h7g","8b8d","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly13_8h3c+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h3c+","2b3c","8i7g","3c7g+","7h7g","8b8d","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly12_2b5e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","2b5e","8h5e","8f8d","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly12_2b6f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","2b6f","8h6f","2a3c","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly11_8h4d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","8h4d","2b4d","P*5b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly11_8h3c+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","8h3c+","2b3c","P*5b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly10_2b5e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","2b5e","P*5b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly10_2b6f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","2b6f","P*5b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly09_8h4d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8h4d","2b4d","8i7g","4d7g+","7h7g","8b8d","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly09_8h3c+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8h3c+","2b3c","8i7g","3c7g+","7h7g","8b8d","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly08_2b5e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","2b5e","8h5e","8b8d","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly08_2b6f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly07_8h4d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","8h4d","2b4d","7i8h","4d8h+","5h8h","8b8d","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly07_8h3c+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","8h3c+","2b3c","7i8h","3c8h+","5h8h","8b8d","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly06_2b5e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","2b5e","8h5e","8b5b","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly06_2b6f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly05_8h4d"), ["7g7f","3c3d","5g5f","8c8d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly05_8h3c+"), ["7g7f","3c3d","5g5f","8c8d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly04_2b5e"), ["7g7f","3c3d","5g5f","2b5e","8h5e","8b5b","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly04_2b6f"), ["7g7f","3c3d","5g5f","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SenteKakuexchange_IronMino_Equilibrium"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","7i7h","8e8f","8g8f","8b8f","P*8g","8f8b","8h2b+","3a2b","6i6h","7a6b","5i4h","5a4b","3i3h","4a3b","4h3i","4b3a","3i2h","5c5d","4i3i","6b5c","1g1f","1c1d","5f5e","8b8d","5h5g","8d7d","6h5h","7d8d","5h4h","8d8e","3g3f","8e5e","5g5e","5c4d","B*5c","3b4b","5c4b+","3a4b","G*5a","4d5e","R*4b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly18_2a3c"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","5f5e","6c6d","5e5d","7a6b","5d5c+","4a3b","5c4b","5a4b","5i4h","8e8f","8g8f","2a3c","P*4b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly18_2b6f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","5f5e","6c6d","5e5d","7a6b","5d5c+","4a3b","5c4b","5a4b","5i4h","8e8f","8g8f","2b6f","P*4b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly17_5h5e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","5f5e","6c6d","5e5d","7a6b","5d5c+","4a3b","5c4b","5a4b","5i4h","8e8f","5h5e","2b5e","P*4b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly17_5h5c+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","5f5e","6c6d","5e5d","7a6b","5d5c+","4a3b","5c4b","5a4b","5i4h","8e8f","5h5c+","6b5c","P*4b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly16_2a3c"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","5f5e","6c6d","5e5d","7a6b","5d5c+","4a3b","5c4b","5a4b","5i4h","2a3c","P*4b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly16_2b6f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","5f5e","6c6d","5e5d","7a6b","5d5c+","4a3b","5c4b","5a4b","5i4h","2b6f","P*4b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly15_5h5c+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","5f5e","6c6d","5e5d","7a6b","5d5c+","4a3b","5c4b","5a4b","5h5c+","6b5c","P*4b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly15_5h5c"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","5f5e","6c6d","5e5d","7a6b","5d5c+","4a3b","5c4b","5a4b","5h5c","2b8h+","P*4b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly13_5c5b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","5f5e","6c6d","5e5d","7a6b","5d5c+","4a3b","5c5b","6a5b","P*5b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly13_5c6b"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","5f5e","6c6d","5e5d","7a6b","5d5c+","4a3b","5c6b","5a6b","P*6b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly12_2b6f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","5f5e","6c6d","5e5d","7a6b","5d5c+","2b6f","P*5b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly12_2b7g+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","5f5e","6c6d","5e5d","7a6b","5d5c+","2b7g+","P*5b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly11_5d5c"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","5f5e","6c6d","5e5d","7a6b","5d5c","2b8h+","P*5b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly11_8h4d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","5f5e","6c6d","5e5d","7a6b","8h4d","2b4d","5d5c+","6b5c","P*5b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly10_2b6f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","5f5e","6c6d","5e5d","2b6f","8h6f","5c5d","B*6b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly10_2b7g+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","5f5e","6c6d","5e5d","2b7g+","8h7g","8b8d","B*6b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly07_8h4d"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","8h4d","2b4d","7i8h","4d8h+","5h8h","8b8d","B*6b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly07_8h3c+"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","8h3c+","2b3c","7i8h","3c8h+","5h8h","8b8d","B*6b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly06_2b5e"), ["7g7f","3c3d","5g5f","8c8d","2h5h","2b5e","8h5e","8b5b","B*6b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly06_2b6f"), ["7g7f","3c3d","5g5f","8c8d","2h5h","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly05_8h4d"), ["7g7f","3c3d","5g5f","8c8d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly05_8h3c+"), ["7g7f","3c3d","5g5f","8c8d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly04_2b5e"), ["7g7f","3c3d","5g5f","2b5e","8h5e","8b5b","B*6b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly04_2b6f"), ["7g7f","3c3d","5g5f","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SenteNakabisha_TokinAssault_Equilibrium"), ["7g7f","3c3d","5g5f","8c8d","2h5h","8d8e","5f5e","6c6d","5e5d","7a6b","5d5c+","4a3b","5c4b","5a4b","5i4h","8e8f","8g8f","2b8h+","P*4b"]),
    (("Start", "GoteGokigen_TokinAssault_Equilibrium_DevPly12_5g4h"), ["2g2f","5c5d","2f2e","8b5b","3g3f","5d5e","3i4h","5e5f","6i7h","5f5g+","4i3h","5g4h","5i4h","5b5d","P*5b"]),
    (("Start", "GoteGokigen_TokinAssault_Equilibrium_DevPly12_5g6h"), ["2g2f","5c5d","2f2e","8b5b","3g3f","5d5e","3i4h","5e5f","6i7h","5f5g+","4i3h","5g6h","5i6h","5b5d","P*5b"]),
    (("Start", "GoteGokigen_TokinAssault_Equilibrium_DevPly11_4h5g"), ["2g2f","5c5d","2f2e","8b5b","3g3f","5d5e","3i4h","5e5f","6i7h","5f5g+","4h5g","5b5g+","P*5b"]),
    (("Start", "GoteGokigen_TokinAssault_Equilibrium_DevPly09_5g5f"), ["2g2f","5c5d","2f2e","8b5b","3g3f","5d5e","3i4h","5e5f","5g5f","5b5d","P*5b"]),
    (("Start", "GoteGokigen_TokinAssault_Equilibrium"), ["2g2f","5c5d","2f2e","8b5b","3g3f","5d5e","3i4h","5e5f","6i7h","5f5g+","4i3h","5g5h"]),
    (("Start", "SujichigaiBishop_Melee_DevPly36_2c2d"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a2a","1h1i","G*3a","4a5a","2a1a","1i2i","1a2a","2i3i","2a1a","2c2d","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly36_3c2d"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a2a","1h1i","G*3a","4a5a","2a1a","1i2i","1a2a","2i3i","2a1a","3c2d","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly35_2a2b"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a2a","1h1i","G*3a","4a5a","2a1a","1i2i","1a2a","2i3i","2a2b","3i4i","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly35_3a4a"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a2a","1h1i","G*3a","4a5a","2a1a","1i2i","1a2a","2i3i","3a4a","4b4a","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly34_2i2h"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a2a","1h1i","G*3a","4a5a","2a1a","1i2i","1a2a","2i2h","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly34_3c2b"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a2a","1h1i","G*3a","4a5a","2a1a","1i2i","1a2a","3c2b","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly33_1a1b"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a2a","1h1i","G*3a","4a5a","2a1a","1i2i","1a1b","2i3i","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly33_3a4a"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a2a","1h1i","G*3a","4a5a","2a1a","1i2i","3a4a","4b4a","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly32_1i1h"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a2a","1h1i","G*3a","4a5a","2a1a","1i1h","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly32_4b3b"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a2a","1h1i","G*3a","4a5a","2a1a","4b3b","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly31_2a2b"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a2a","1h1i","G*3a","4a5a","2a2b","3c2b","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly31_3a4a"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a2a","1h1i","G*3a","4a5a","3a4a","4b4a","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly29_2a3a"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a2a","1h1i","2a3a","4a3a","G*3b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly29_2a2b"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a2a","1h1i","2a2b","3c2b","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly28_1h2h"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a2a","1h2h","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly28_1h1g"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a2a","1h1g","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly27_1a1b"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a1b","1h2i","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly27_2i1g"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","2i1g","1h1g","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly26_1g2h"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g2h","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly26_4b1b"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","4b1b","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly25_1b2b"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b2b","3c2b","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly25_1i1h"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1i1h","1g1h","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly24_2e1g"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly24_2e3g+"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e3g+","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly23_1c2b"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c2b","3c2b","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly23_2i1g"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","2i1g","1b1c","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly22_1a1c"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1c","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly22_2e1g+"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","2e1g+","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly21_2i1g"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","2i1g","2e1g+","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly21_G*1b"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","G*1b","1a1b","P*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly20_1a1c"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","1a1c","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly20_1a1d"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","1a1d","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly18_1a1c"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","1a1c","P*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly18_1a1d"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","1a1d","P*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly17_2i1g"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","2i1g","2e1g+","P*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly15_2i1g"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","2i1g","2e1g+","G*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly15_G*1b"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*1b","1a1b","P*4b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly13_2i1g"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","2i1g","2e1g+","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly13_6c5b"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c5b","8b5b","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly12_B*1d"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","B*1d","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly12_B*1f"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","B*1f","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly11_4e5d"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e5d","5c5d","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly10_1a1d"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","1a1d","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly10_1a1e"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","1a1e","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly09_4e3d"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","4e3d","8b5b","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly09_4e5d"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","4e5d","5c5d","2h5h","8b5b","8i7g","2a3c","1f1e","8c8d","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly08_B*1e"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","B*1e","1f1e","8b5b","B*6b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly08_B*1g"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","B*1g","2i1g","8b5b","B*6b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly07_4e3d"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","4e3d","8b5b","P*5b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly07_4e5d"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","4e5d","5c5d","2h5h","8b5b","8i7g","2a3c","2g2f","8c8d"]),
    (("Start", "SujichigaiBishop_Melee_DevPly06_B*1f"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","B*1f","1g1f","8b5b","B*6b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly06_B*1h"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","B*1h","1i1h","8b5b","B*6b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly05_B*1b"), ["7g7f","3c3d","8h2b+","3a2b","B*1b","1a1b","2h5h","8b5b","8i7g","2a3c","2g2f","8c8d"]),
    (("Start", "SujichigaiBishop_Melee_DevPly05_B*1d"), ["7g7f","3c3d","8h2b+","3a2b","B*1d","1c1d","2h5h","8b5b","8i7g","2a3c","2g2f","8c8d"]),
    (("Start", "SujichigaiBishop_Melee_DevPly04_5a4b"), ["7g7f","3c3d","8h2b+","5a4b","2b3a","4a3a","S*3c","2a3c","2h5h","S*4h","3i4h"]),
    (("Start", "SujichigaiBishop_Melee_DevPly04_5a5b"), ["7g7f","3c3d","8h2b+","5a5b","2b3a","4a3a","S*4a","3a4a","2h5h","S*4h","3i4h"]),
    (("Start", "SujichigaiBishop_Melee_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SujichigaiBishop_Melee_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SujichigaiBishop_Melee"), ["7g7f","3c3d","8h2b+","3a2b","B*4e","1c1d","1g1f","1d1e","1f1e","2a1c","4e6c+","1c2e","6c4a","5a4a","G*4b","8b4b","1e1d","G*5h","2h5h","2b3c","1d1c+","1a1b","1c1b","2e1g+","1b1a","1g1h","1a2a","1h1i","G*3a","4a5a","2a1a","1i2i","1a2a","2i3i","2a1a","3i4i","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly36_1f1g+"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e1f","2a1a","1f1g+","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly36_1f1g"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e1f","2a1a","1f1g","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly35_1h1g"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e1f","1h1g","3c8h+","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly35_2a2b"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e1f","2a2b","3c8h+","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly34_1e2f"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e2f","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly34_1e2d"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e2d","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly33_1h1g"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","1h1g","3c8h+","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly33_1h1f"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","1h1f","3c8h+","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly32_1d2e"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d2e","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly32_1d2c"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d2c","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly31_1h1g"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","1h1g","3c8h+","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly31_1h1f"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","1h1f","3c8h+","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly30_3c2b"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","3c2b","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly30_3c2d"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","3c2d","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly29_2h3h"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2h3h","3c8h+","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly29_2h4h"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2h4h","3c8h+","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly28_2b1a"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1a","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly28_3c4b"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","3c4b","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly27_2h3h"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2h3h","3c8h+","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly27_2h4h"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2h4h","3c8h+","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly26_2a1b"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a1b","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly26_2a3b"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a3b","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly25_2h3h"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2h3h","3c8h+","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly25_2h4h"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2h4h","3c8h+","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly24_2c2d"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","2c2d","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly24_3c2b"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3c2b","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly23_2a1a"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a1a","3c8h+","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly23_2a2b"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a2b","3c8h+","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly22_2c2d"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","2c2d","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly22_3a2b"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a2b","N*4c"]),
    (("Start", "Early3eBlitz_Melee_DevPly21_2a1a"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","2a1a","3c8h+","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly21_2a3a"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","2a3a","3c8h+","S*4b"]),
    (("Start", "Early3eBlitz_Melee_DevPly20_1g2h"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g2h","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly20_1g2g"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g2g","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly19_1a1b"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a1b","3c8h+","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly19_1i1h"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1i1h","3c8h+","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly18_2e1g"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly18_2e3g+"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e3g+","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly17_1b2b"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b2b","3c8h+","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly17_2i1g"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","2i1g","3c8h+","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly16_2b1a"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b1a","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly16_2b1c"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b1c","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly15_1c2b"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c2b","3a2b","B*6b"]),
    (("Start", "Early3eBlitz_Melee_DevPly15_2i1g"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","2i1g","2b8h+","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly14_1a1c"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1c","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly14_2b1c"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","2b1c","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly13_2i1g"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","2i1g","2b8h+","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly13_8h4d"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","8h4d","2b4d","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly12_2b6f"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","2b6f","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly12_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","2b7g+","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly11_8h4d"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","8h4d","2b4d","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly11_8h2b+"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","8h2b+","3a2b","B*6b"]),
    (("Start", "Early3eBlitz_Melee_DevPly10_1a1d"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","1a1d","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly10_1a1e"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","1a1e","P*5b"]),
    (("Start", "Early3eBlitz_Melee_DevPly09_8h4d"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Early3eBlitz_Melee_DevPly09_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Early3eBlitz_Melee_DevPly08_2b6f"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "Early3eBlitz_Melee_DevPly08_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "Early3eBlitz_Melee_DevPly07_8h4d"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Early3eBlitz_Melee_DevPly07_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Early3eBlitz_Melee_DevPly06_2b6f"), ["7g7f","3c3d","2g2f","8c8d","3g3f","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "Early3eBlitz_Melee_DevPly06_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","3g3f","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "Early3eBlitz_Melee_DevPly05_8h4d"), ["7g7f","3c3d","2g2f","8c8d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Early3eBlitz_Melee_DevPly05_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Early3eBlitz_Melee_DevPly04_2b6f"), ["7g7f","3c3d","2g2f","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "Early3eBlitz_Melee_DevPly04_2b7g+"), ["7g7f","3c3d","2g2f","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "Early3eBlitz_Melee_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Early3eBlitz_Melee_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "Early3eBlitz_Melee"), ["7g7f","3c3d","2g2f","8c8d","3g3f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e1f","2a1a","3c8h+","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly36_1f1g+"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e1f","2a1a","1f1g+","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly36_1f1g"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e1f","2a1a","1f1g","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly35_1h1g"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e1f","1h1g","3c8h+","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly35_2a2b"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e1f","2a2b","3c8h+","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly34_1e2f"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e2f","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly34_1e2d"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e2d","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly33_1h1g"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","1h1g","3c8h+","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly33_1h1f"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","1h1f","3c8h+","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly32_1d2e"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d2e","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly32_1d2c"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d2c","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly31_1h1g"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","1h1g","3c8h+","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly31_1h1f"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","1h1f","3c8h+","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly30_3c2b"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","3c2b","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly30_3c2d"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","3c2d","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly29_2h2g"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2h2g","3c8h+","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly29_2h2f"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2h2f","3c8h+","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly28_2b1a"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1a","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly28_3c4b"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","3c4b","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly27_2h2g"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2h2g","3c8h+","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly27_2h2f"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2h2f","3c8h+","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly26_2a1b"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a1b","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly26_2a3b"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a3b","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly25_2h2g"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2h2g","3c8h+","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly25_2i1g"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2i1g","3c8h+","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly24_2c2d"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","2c2d","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly24_3c2b"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3c2b","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly23_2a1a"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a1a","3c8h+","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly23_2a2b"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a2b","3c8h+","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly22_2c2d"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","2c2d","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly22_3a2b"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a2b","N*4c"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly21_2a1a"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","2a1a","3c8h+","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly21_2a3a"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","2a3a","3c8h+","S*4b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly20_1g2g"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g2g","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly20_3c2b"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","3c2b","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly19_1a1b"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a1b","3c8h+","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly19_1i1h"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1i1h","3c8h+","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly18_2e1g"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly18_2e3g+"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e3g+","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly17_1b2b"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b2b","3c8h+","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly17_2i1g"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","2i1g","3c8h+","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly16_2b1a"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b1a","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly16_2b1c"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b1c","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly15_1c2b"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c2b","3a2b","B*6b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly15_2i1g"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","2i1g","2b8h+","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly14_1a1c"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1c","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly14_2b1c"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","2b1c","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly13_2i1g"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","2i1g","2b8h+","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly13_8h4d"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","8h4d","2b4d","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly12_2b6f"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","2b6f","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly12_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","2b7g+","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly11_8h4d"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","8h4d","2b4d","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly11_8h2b+"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","8h2b+","3a2b","B*6b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly10_1a1d"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","1a1d","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly10_1a1e"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","1a1e","P*5b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly09_8h4d"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","8h4d","2b4d","3h2g","4d9i+","2h5h","9i8i","2g3f","8i7i"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly09_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","8h3c+","2b3c","3h2g","3c9i+","2h5h","9i8i","2g3f","8i7i"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly08_2b6f"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly08_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly07_8h4d"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","8h4d","2b4d","3h2g","4d9i+","2h5h","9i8i","2g3f","8i7i"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly07_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","8h3c+","2b3c","3h2g","3c9i+","2h5h","9i8i","2g3f","8i7i"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly06_2b6f"), ["7g7f","3c3d","2g2f","8c8d","3i3h","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly06_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","3i3h","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly05_8h4d"), ["7g7f","3c3d","2g2f","8c8d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly05_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly04_2b6f"), ["7g7f","3c3d","2g2f","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly04_2b7g+"), ["7g7f","3c3d","2g2f","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "EarlyClimbingSilver_Melee_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "EarlyClimbingSilver_Melee"), ["7g7f","3c3d","2g2f","8c8d","3i3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e1f","2a1a","3c8h+","N*4c"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly36_3h2i"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","3d3e","2e2d","3e3f","2d2c+","3f3g+","2c2b","3g3h","2b2a","3h2i","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly36_3h3i"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","3d3e","2e2d","3e3f","2d2c+","3f3g+","2c2b","3g3h","2b2a","3h3i","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly35_3a4a"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","3d3e","2e2d","3e3f","2d2c+","3f3g+","2c2b","3g3h","3a4a","5a4a","G*4b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly35_3i2h"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","3d3e","2e2d","3e3f","2d2c+","3f3g+","2c2b","3g3h","3i2h","3h4h","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly34_3g2h"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","3d3e","2e2d","3e3f","2d2c+","3f3g+","2c2b","3g2h","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly34_3g4h"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","3d3e","2e2d","3e3f","2d2c+","3f3g+","2c2b","3g4h","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly33_2c3c"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","3d3e","2e2d","3e3f","2d2c+","3f3g+","2c3c","3g4h","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly33_2c2d"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","3d3e","2e2d","3e3f","2d2c+","3f3g+","2c2d","3g4h","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly32_4b3c"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","3d3e","2e2d","3e3f","2d2c+","4b3c","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly32_4b2d"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","3d3e","2e2d","3e3f","2d2c+","4b2d","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly31_3a4a"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","3d3e","2e2d","3e3f","3a4a","5a4a","G*4b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly31_3a3b"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","3d3e","2e2d","3e3f","3a3b","4a3b","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly30_N*1a"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","3d3e","2e2d","N*1a","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly30_N*1b"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","3d3e","2e2d","N*1b","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly29_3a4a"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","3d3e","3a4a","5a4a","G*4b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly29_3a3b"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","3d3e","3a3b","4a3b","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly28_4b2d"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","4b2d","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly28_4b1e"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","4b1e","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly27_3a4a"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","3a4a","5a4a","G*4b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly27_3a3b"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","3a3b","4a3b","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly26_3c1a"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c1a","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly26_3c1e"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c1e","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly25_2a1a"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a1a","3c1a","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly25_2a2b"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a2b","3c2b","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly23_2a1a"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a1a","3c1a","N*4c"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly23_2a3a"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","4a3a","N*4c"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly22_2c2d"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","2c2d","N*4c"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly22_3a2b"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a2b","N*4c"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly21_2a1a"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","2a1a","3c1a","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly21_2a3a"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","2a3a","4a3a","S*4b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly20_1g2h"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g2h","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly20_3c2b"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","3c2b","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly19_1a1b"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a1b","8b5b","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly19_1i1h"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1i1h","3c1a","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly18_2e1g"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly18_2e3g+"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e3g+","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly17_1b2b"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b2b","3a2b","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly17_2i1g"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","2i1g","2e1g+","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly16_2b1a"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b1a","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly16_2b1c"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b1c","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly15_1c2b"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c2b","3a2b","B*6b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly15_2i1g"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","2i1g","2b1c","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly14_1a1c"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1c","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly14_2b1c"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","2b1c","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly13_2i1g"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","2i1g","2e1g+","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly10_1a1d"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","1a1d","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly10_1a1e"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","1a1e","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly09_8h4d"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","8h4d","2b4d","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly07_8h4d"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","8h4d","2b4d","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly05_8h4d"), ["7g7f","3c3d","2g2f","4c4d","8h4d","2b4d","P*5b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly04_2b6f"), ["7g7f","3c3d","2g2f","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly04_2b7g+"), ["7g7f","3c3d","2g2f","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "MigiShikenBlitz_Melee_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "MigiShikenBlitz_Melee"), ["7g7f","3c3d","2g2f","4c4d","2h4h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","N*4c","3b4c","2a3a","3c4b","2f2e","3d3e","2e2d","3e3f","2d2c+","3f3g+","2c2b","3g3h","2b2a","3h4h","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly36_1f1g+"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e1f","2a1a","1f1g+","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly36_1f1g"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e1f","2a1a","1f1g","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly35_1h1g"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e1f","1h1g","3c8h+","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly35_2a2b"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e1f","2a2b","3c8h+","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly34_1e2f"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e2f","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly34_1e2d"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e2d","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly33_1h1g"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","1h1g","3c8h+","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly33_1h1f"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","1h1f","3c8h+","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly32_1d2e"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d2e","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly32_1d2c"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d2c","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly31_1h1g"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","1h1g","3c8h+","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly31_1h1f"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","1h1f","3c8h+","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly30_3c2b"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","3c2b","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly30_3c2d"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","3c2d","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly29_2i1g"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2i1g","3c8h+","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly29_3a2a"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","3a2a","3c8h+","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly28_2b1a"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1a","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly28_3c4b"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","3c4b","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly27_2i1g"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2i1g","3c8h+","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly27_3a2a"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","3a2a","3c8h+","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly26_2a1b"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a1b","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly26_2a3b"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a3b","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly25_2i1g"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2i1g","3c8h+","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly25_3a4a"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","3a4a","5a4a","G*4b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly24_2c2d"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","2c2d","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly24_3c2b"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3c2b","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly23_2a1a"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a1a","3c8h+","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly23_2a2b"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a2b","3c8h+","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly22_2c2d"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","2c2d","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly22_3a2b"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a2b","N*4c"]),
    (("Start", "SodebishaBlitz_Melee_DevPly21_2a1a"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","2a1a","3c8h+","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly21_2a3a"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","2a3a","3c8h+","S*4b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly20_1g2h"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g2h","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly20_3c2b"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","3c2b","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly19_1a1b"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a1b","3c8h+","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly19_1i1h"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1i1h","3c8h+","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly18_2e1g"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly18_2e3g+"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e3g+","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly17_1b2b"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b2b","3c8h+","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly17_2i1g"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","2i1g","3c8h+","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly16_2b1a"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b1a","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly16_2b1c"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b1c","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly15_1c2b"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c2b","3a2b","B*6b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly15_2i1g"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","2i1g","2b8h+","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly14_1a1c"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1c","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly14_2b1c"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","2b1c","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly13_2i1g"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","2i1g","2b8h+","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly13_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","8h4d","2b4d","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly12_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","2b6f","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly12_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","2b7g+","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly11_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","8h4d","2b4d","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly11_8h2b+"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","8h2b+","3a2b","B*6b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly10_1a1d"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","1a1d","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly10_1a1e"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","1a1e","P*5b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly09_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","8h4d","2b4d","7i8h","4d8h+","3h8h","S*4h","B*6b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly09_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","8h3c+","2b3c","7i8h","3c8h+","3h8h","S*4h","B*6b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly08_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly08_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly07_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","8h4d","2b4d","7i8h","4d8h+","3h8h","S*4h","B*6b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly07_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","8h3c+","2b3c","7i8h","3c8h+","3h8h","S*4h","B*6b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly06_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2h3h","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly06_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2h3h","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly05_8h4d"), ["7g7f","3c3d","2g2f","8c8d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly05_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly04_2b6f"), ["7g7f","3c3d","2g2f","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly04_2b7g+"), ["7g7f","3c3d","2g2f","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SodebishaBlitz_Melee_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "SodebishaBlitz_Melee"), ["7g7f","3c3d","2g2f","8c8d","2h3h","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2f2e","2a2b","2e2d","2b1c","2d2c+","1c1d","2c2b","1d1e","2b2a","1e1f","2a1a","3c8h+","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly36_1f1g+"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1c","2e2d","1c1d","2d2c+","1d1e","2c2b","1e1f","2b2a","1f1g+","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly36_1f1g"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1c","2e2d","1c1d","2d2c+","1d1e","2c2b","1e1f","2b2a","1f1g","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly35_1h1g"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1c","2e2d","1c1d","2d2c+","1d1e","2c2b","1e1f","1h1g","3c8h+","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly35_2b1a"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1c","2e2d","1c1d","2d2c+","1d1e","2c2b","1e1f","2b1a","3c8h+","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly34_1e2f"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1c","2e2d","1c1d","2d2c+","1d1e","2c2b","1e2f","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly34_1e2d"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1c","2e2d","1c1d","2d2c+","1d1e","2c2b","1e2d","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly33_1h1g"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1c","2e2d","1c1d","2d2c+","1d1e","1h1g","3c8h+","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly33_1h1f"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1c","2e2d","1c1d","2d2c+","1d1e","1h1f","3c8h+","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly32_1d2e"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1c","2e2d","1c1d","2d2c+","1d2e","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly32_3c2b"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1c","2e2d","1c1d","2d2c+","3c2b","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly31_1h1g"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1c","2e2d","1c1d","1h1g","3c8h+","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly31_1h1f"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1c","2e2d","1c1d","1h1f","3c8h+","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly30_3c6f"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1c","2e2d","3c6f","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly30_3c7g+"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1c","2e2d","3c7g+","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly29_2h3h"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1c","2h3h","3c8h+","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly29_2h4h"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1c","2h4h","3c8h+","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly28_2b1a"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1a","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly28_2c2d"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2c2d","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly27_2h3h"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2h3h","3c8h+","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly27_2h4h"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2h4h","3c8h+","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly26_2a1b"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a1b","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly26_2a3b"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a3b","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly25_2h3h"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2h3h","3c8h+","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly25_2h4h"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2h4h","3c8h+","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly24_2c2d"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","2c2d","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly24_3c2b"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3c2b","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly23_2a1a"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a1a","3c8h+","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly23_2a2b"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a2b","3c8h+","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly22_2c2d"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","2c2d","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly22_3a2b"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a2b","N*4c"]),
    (("Start", "EarlyIshidaMelee_DevPly21_2a1a"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","2a1a","3c8h+","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly21_2a3a"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","2a3a","3c8h+","S*4b"]),
    (("Start", "EarlyIshidaMelee_DevPly20_1g2h"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g2h","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly20_1g2g"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g2g","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly19_1a1b"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a1b","3c8h+","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly19_1i1h"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1i1h","3c8h+","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly18_2e1g"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly18_2e3g+"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e3g+","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly17_1b2b"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b2b","3c8h+","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly17_2i1g"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","2i1g","3c8h+","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly16_2b1a"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b1a","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly16_2b1c"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b1c","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly15_1c2b"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c2b","3a2b","B*6b"]),
    (("Start", "EarlyIshidaMelee_DevPly15_2i1g"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","2i1g","2b8h+","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly14_1a1c"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1c","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly14_2b1c"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","2b1c","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly13_2i1g"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","2i1g","2b8h+","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly13_7f8e"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","7f8e","2b8h+","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly12_2b6f"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","2b6f","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly12_2b7g+"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","2b7g+","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly11_7f8e"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","7f8e","2b8h+","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly11_8h4d"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","8h4d","2b4d","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly10_1a1d"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","1a1d","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly10_1a1e"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","1a1e","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly09_7f8e"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","7f8e","2b8h+","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly09_8h4d"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","8h4d","2b4d","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly08_2b6f"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","2b6f","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly08_2b7g+"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","2b7g+","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly07_7f8e"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","7f8e","2b8h+","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly07_8h4d"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","8h4d","2b4d","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly06_2b6f"), ["7g7f","3c3d","7i7h","8c8d","7h7f","2b6f","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly06_2b7g+"), ["7g7f","3c3d","7i7h","8c8d","7h7f","2b7g+","P*5b"]),
    (("Start", "EarlyIshidaMelee_DevPly05_8h4d"), ["7g7f","3c3d","7i7h","8c8d","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7h7g","8b5b","B*6b"]),
    (("Start", "EarlyIshidaMelee_DevPly05_8h3c+"), ["7g7f","3c3d","7i7h","8c8d","8h3c+","2b3c","2h5h","3c9i+","8i7g","9i7g","7h7g","8b5b","B*6b"]),
    (("Start", "EarlyIshidaMelee_DevPly04_2b6f"), ["7g7f","3c3d","7i7h","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "EarlyIshidaMelee_DevPly04_2b7g+"), ["7g7f","3c3d","7i7h","2b7g+","7h7g","8b5b","B*6b"]),
    (("Start", "EarlyIshidaMelee_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "EarlyIshidaMelee_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2h5h","3c9i+","7i8h","9i8h","5h8h","S*4h","B*6b"]),
    (("Start", "EarlyIshidaMelee"), ["7g7f","3c3d","7i7h","8c8d","7h7f","1c1d","1g1f","1d1e","1f1e","2a1c","1e1d","1c2e","1d1c+","1a1b","1c1b","2b3c","1b1a","2e1g+","1a2a","1g1h","1i1h","3a3b","2a3a","3b2a","2g2f","2a2b","2f2e","2b1c","2e2d","1c1d","2d2c+","1d1e","2c2b","1e1f","2b2a","3c8h+","N*4c"]),
    (("Start", "UreshinoMelee_DevPly36_1g1h"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","2c1d","2h2i","1d1e","2i2h","1e1f","2h2i","1f1g+","2i2h","1g1h","B*6b"]),
    (("Start", "UreshinoMelee_DevPly36_1g2g"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","2c1d","2h2i","1d1e","2i2h","1e1f","2h2i","1f1g+","2i2h","1g2g","B*6b"]),
    (("Start", "UreshinoMelee_DevPly35_2i2g"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","2c1d","2h2i","1d1e","2i2h","1e1f","2h2i","1f1g+","2i2g","1g2g","B*6b"]),
    (("Start", "UreshinoMelee_DevPly35_2i2e"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","2c1d","2h2i","1d1e","2i2h","1e1f","2h2i","1f1g+","2i2e","3c2e","B*6b"]),
    (("Start", "UreshinoMelee_DevPly34_1f2g+"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","2c1d","2h2i","1d1e","2i2h","1e1f","2h2i","1f2g+","B*6b"]),
    (("Start", "UreshinoMelee_DevPly34_1f2g"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","2c1d","2h2i","1d1e","2i2h","1e1f","2h2i","1f2g","B*6b"]),
    (("Start", "UreshinoMelee_DevPly33_2h3h"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","2c1d","2h2i","1d1e","2i2h","1e1f","2h3h","3i3h","B*6b"]),
    (("Start", "UreshinoMelee_DevPly33_2h4h"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","2c1d","2h2i","1d1e","2i2h","1e1f","2h4h","3i4i","B*6b"]),
    (("Start", "UreshinoMelee_DevPly32_1e2f"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","2c1d","2h2i","1d1e","2i2h","1e2f","B*6b"]),
    (("Start", "UreshinoMelee_DevPly32_3c2e"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","2c1d","2h2i","1d1e","2i2h","3c2e","B*6b"]),
    (("Start", "UreshinoMelee_DevPly31_2i2f"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","2c1d","2h2i","1d1e","2i2f","1e2f","B*6b"]),
    (("Start", "UreshinoMelee_DevPly31_2i2e"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","2c1d","2h2i","1d1e","2i2e","3c2e","B*6b"]),
    (("Start", "UreshinoMelee_DevPly30_3c2e"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","2c1d","2h2i","3c2e","B*6b"]),
    (("Start", "UreshinoMelee_DevPly30_3i4i"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","2c1d","2h2i","3i4i","B*6b"]),
    (("Start", "UreshinoMelee_DevPly29_2h3h"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","2c1d","2h3h","3i3h","B*6b"]),
    (("Start", "UreshinoMelee_DevPly29_2h4h"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","2c1d","2h4h","3i4i","B*6b"]),
    (("Start", "UreshinoMelee_DevPly28_3c2e"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","3c2e","B*6b"]),
    (("Start", "UreshinoMelee_DevPly28_3i2i"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","3i2i","B*6b"]),
    (("Start", "UreshinoMelee_DevPly27_2a2b"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a2b","3i4i","B*6b"]),
    (("Start", "UreshinoMelee_DevPly27_2h1h"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2h1h","3i4i","B*6b"]),
    (("Start", "UreshinoMelee_DevPly26_3b2a"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2a","B*6b"]),
    (("Start", "UreshinoMelee_DevPly26_3c2e"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3c2e","B*6b"]),
    (("Start", "UreshinoMelee_DevPly25_2b3a"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b3a","3i4i","B*6b"]),
    (("Start", "UreshinoMelee_DevPly25_2b1b"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b1b","3i4i","B*6b"]),
    (("Start", "UreshinoMelee_DevPly24_3c2e"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3c2e","B*6b"]),
    (("Start", "UreshinoMelee_DevPly24_3i2i"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3i2i","B*6b"]),
    (("Start", "UreshinoMelee_DevPly23_2c1b"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c1b","3i4i","B*6b"]),
    (("Start", "UreshinoMelee_DevPly23_2c3b"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c3b","3a3b","B*6b"]),
    (("Start", "UreshinoMelee_DevPly22_2i2h"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i2h","B*6b"]),
    (("Start", "UreshinoMelee_DevPly22_3c2e"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","3c2e","B*6b"]),
    (("Start", "UreshinoMelee_DevPly21_2h1h"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2h1h","2i3i","B*6b"]),
    (("Start", "UreshinoMelee_DevPly21_2h4h"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2h4h","2i3i","B*6b"]),
    (("Start", "UreshinoMelee_DevPly20_1i1h"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i1h","B*6b"]),
    (("Start", "UreshinoMelee_DevPly20_3c2e"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","3c2e","B*6b"]),
    (("Start", "UreshinoMelee_DevPly19_2h1h"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2h1h","1i1h","B*6b"]),
    (("Start", "UreshinoMelee_DevPly19_2i1g"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2i1g","8b5b","B*6b"]),
    (("Start", "UreshinoMelee_DevPly18_1h2i"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h2i","B*6b"]),
    (("Start", "UreshinoMelee_DevPly18_1h2h"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h2h","B*6b"]),
    (("Start", "UreshinoMelee_DevPly17_2h1h"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2h1h","8b5b","B*6b"]),
    (("Start", "UreshinoMelee_DevPly17_2i1g"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2i1g","1h2h","B*6b"]),
    (("Start", "UreshinoMelee_DevPly16_1g2h"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g2h","B*6b"]),
    (("Start", "UreshinoMelee_DevPly16_1g2g"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g2g","B*6b"]),
    (("Start", "UreshinoMelee_DevPly15_1i1g"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1g","8b5b","B*6b"]),
    (("Start", "UreshinoMelee_DevPly15_2h1h"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","2h1h","1g1h","B*6b"]),
    (("Start", "UreshinoMelee_DevPly14_3c2e"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","3c2e","B*6b"]),
    (("Start", "UreshinoMelee_DevPly14_5a4b"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","5a4b","2h5h","8b5b","8i7g","3d3e","7f7e","5c5d","5g5f"]),
    (("Start", "UreshinoMelee_DevPly13_1i1g"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","1i1g","1f1g+","B*6b"]),
    (("Start", "UreshinoMelee_DevPly13_1i1f"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","1i1f","1a1f","B*6b"]),
    (("Start", "UreshinoMelee_DevPly12_5a5b"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","5a5b","3c1a","3d3e","1a2a","3a3b","2a3b","4a3b","N*4d"]),
    (("Start", "UreshinoMelee_DevPly12_5a6b"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","5a6b","3c1a","3d3e","1a2a","3a3b","2a3b","4a3b","N*5d"]),
    (("Start", "UreshinoMelee_DevPly11_1i1g"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","1i1g","3a2b","B*6b"]),
    (("Start", "UreshinoMelee_DevPly11_1i1f"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","1i1f","3a2b","B*6b"]),
    (("Start", "UreshinoMelee_DevPly10_1a1b"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1a1b","B*6b"]),
    (("Start", "UreshinoMelee_DevPly10_1a1c"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1a1c","B*6b"]),
    (("Start", "UreshinoMelee_DevPly09_1f1e"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","1f1e","2b8h+","P*5b"]),
    (("Start", "UreshinoMelee_DevPly09_8h4d"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","6h7g","8b5b","B*6b"]),
    (("Start", "UreshinoMelee_DevPly08_2b6f"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "UreshinoMelee_DevPly08_2b7g+"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","2b7g+","6h7g","8b5b","B*6b"]),
    (("Start", "UreshinoMelee_DevPly07_8h4d"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","6h7g","8b5b","B*6b"]),
    (("Start", "UreshinoMelee_DevPly07_8h3c+"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","8h3c+","2b3c","2h5h","3c9i+","8i7g","9i7g","6h7g","8b5b","B*6b"]),
    (("Start", "UreshinoMelee_DevPly06_2b6f"), ["6i7h","3c3d","7g7f","8c8d","7i6h","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "UreshinoMelee_DevPly06_2b7g+"), ["6i7h","3c3d","7g7f","8c8d","7i6h","2b7g+","6h7g","8b5b","B*6b"]),
    (("Start", "UreshinoMelee_DevPly05_8h4d"), ["6i7h","3c3d","7g7f","8c8d","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7h7g","8b5b","B*6b"]),
    (("Start", "UreshinoMelee_DevPly05_8h3c+"), ["6i7h","3c3d","7g7f","8c8d","8h3c+","2b3c","2h5h","3c9i+","8i7g","9i7g","7h7g","8b5b","B*6b"]),
    (("Start", "UreshinoMelee_DevPly04_2b6f"), ["6i7h","3c3d","7g7f","2b6f","8h6f","8b5b","B*6b"]),
    (("Start", "UreshinoMelee_DevPly04_2b7g+"), ["6i7h","3c3d","7g7f","2b7g+","8h7g","8b5b","B*6b"]),
    (("Start", "UreshinoMelee"), ["6i7h","3c3d","7g7f","8c8d","7i6h","1c1d","1g1f","1d1e","8h2b+","1e1f","2b3c","2a3c","2g2f","1f1g+","1i1h","1g1h","2f2e","1h1i","2e2d","1i2i","2d2c+","2i3i","2c2b","3a3b","2b2a","3b2c","2a3a","2c1d","2h2i","1d1e","2i2h","1e1f","2h2i","1f1g+","2i2h","1g2h","B*6b"])
  ]
