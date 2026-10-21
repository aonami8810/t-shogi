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

-- | Node ID: Aigakari_Equilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_Equilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly03_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_Equilibrium_DevPly04_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly05_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly05_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_Equilibrium_DevPly06_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly07_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly07_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_Equilibrium_DevPly08_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_Equilibrium_DevPly09_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly09_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly10_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_Equilibrium_DevPly10_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_Equilibrium_DevPly11_2h2e (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly11_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_Equilibrium_DevPly12_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_Equilibrium_DevPly12_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_Equilibrium_DevPly13_2d1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly13_2d2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly14_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_Equilibrium_DevPly14_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly15_2h2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly15_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly16_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly16_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly17_2h2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly17_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly18_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly18_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly19_2h2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly19_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly20_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly20_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly21_2h2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly21_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly22_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly22_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly23_2h2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly23_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly24_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly24_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly25_2h2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly25_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly27_2h2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly27_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly29_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_Equilibrium_DevPly29_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly31_2f2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly31_2f2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly33_2e2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly33_2e3e (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly35_2f2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_Equilibrium_DevPly35_2f2d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly05_2b5e (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly05_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly06_2b5e (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly06_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly08_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly09_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly09_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly10_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly10_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly11_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly11_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly12_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly12_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly13_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly13_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly14_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly14_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly15_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly15_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly16_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly16_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly17_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly17_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly18_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly18_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly19_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly19_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly20_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly20_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly21_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly21_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly22_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly22_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly23_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly23_8d7e (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly24_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly24_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly25_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly25_8d7e (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly26_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly26_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly27_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly27_8d7e (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly28_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly28_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly29_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly29_8d7e (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly30_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly30_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly31_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly31_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly32_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_Equilibrium_DevPly32_8h7i (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly03_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly04_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly05_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly05_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly06_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly07_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly07_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly08_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly09_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly09_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly10_1a1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly10_3b3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly11_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly11_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly12_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly12_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly13_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly13_B*1h (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly14_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly14_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly15_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly15_B*1h (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly16_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly16_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly17_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly17_B*1h (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly18_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly18_S*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly19_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly19_B*1h (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly20_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly20_S*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly21_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly21_B*1h (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly22_4f3e (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly22_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly23_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly23_B*1h (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly24_3e2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly24_3e4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly25_3e2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly25_3e4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly26_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly26_B*1h (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly27_2h2e (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly27_3e4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly28_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly28_B*1h (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly29_2d1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly29_2d2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly30_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly30_B*1h (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly31_2h2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly31_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly32_S*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly32_S*1h (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly33_2h2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly33_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly34_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly34_B*1h (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly35_2h2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly35_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly36_B*1h (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_Equilibrium_DevPly36_B*3g (Type: Saddle, Potential: 50.0)
-- | Node ID: QEGTB_AtamaKin (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_HaraKin (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_RookHead (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_TsurushiKei (Type: Minimum, Potential: 9999.0)
-- | Node ID: Sankenbisha_Equilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly03_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly04_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly05_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly05_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly06_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly07_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly07_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly08_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly09_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly09_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly10_7g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly10_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly11_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly11_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly12_7g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly12_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly13_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly13_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly14_7g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly14_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly15_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly15_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly16_7g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly16_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly17_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly17_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly18_7g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly18_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly19_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly19_B*1e (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly20_7g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly20_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly21_B*1e (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly21_B*1g (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly22_7f7d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly22_7f8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly23_B*1e (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly23_B*1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly24_7f7d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly24_7f8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly25_B*1e (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly25_B*1g (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly26_7g3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly26_7g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly27_8f6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly27_8f9f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly28_8f5c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_Equilibrium_DevPly28_8f6d (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly03_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly10_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly12_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly14_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly14_G*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly16_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly16_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly17_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly17_B*1h (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly18_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly18_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly19_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly19_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly20_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly20_B*2f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly21_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly21_B*2f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly22_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly22_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly23_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly23_B*2f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly24_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly24_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly25_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly25_B*1h (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly26_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly26_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly27_B*1e (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly27_B*1g (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly28_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly28_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly29_B*1e (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly29_B*1g (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly30_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly30_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly31_3c2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly31_B*1e (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly32_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly32_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly33_3c2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly33_B*1e (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly34_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly34_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly35_3c2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly35_9h9i+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly36_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly36_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly37_2d1e (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly37_2d3e (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly38_7g5e (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_Equilibrium_DevPly38_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Start (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_Equilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_Equilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly03_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly08_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly10_2b5e (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly10_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly12_2b5e (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly12_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly14_2b5e (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly14_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly19_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly19_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly21_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly21_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly23_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly23_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly25_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly25_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly27_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly27_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly28_8b8e (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly30_8f7f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly30_8f9f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly31_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly31_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly32_8b8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly33_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly33_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly34_8b8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly35_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly35_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly36_8b8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly37_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly37_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly38_8b8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly39_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly39_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly40_8b8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_Equilibrium_DevPly42_8b8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly03_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly04_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly04_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly05_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly05_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly06_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly07_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly07_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly08_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly09_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly09_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly10_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly10_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly11_2h2e (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly11_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly12_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly12_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly13_2d1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly13_2d2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly14_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly14_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly15_2d1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly15_2d2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly16_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly16_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly17_3d1d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly17_3d4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly18_3d1d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly18_3d4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly19_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly19_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly20_3f3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly20_3f3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly21_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly21_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly22_2g5d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly22_3d4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly23_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly23_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly24_1g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly24_3f3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly25_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly25_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly26_1f4c (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly26_3e8e (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly27_2b6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly27_2b7g+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly28_1i1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly28_1i1e (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly29_3c1e (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly29_3c6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly30_1i1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_Equilibrium_DevPly30_1i1e (Type: Saddle, Potential: 50.0)

-- ============================================================================
-- 2. 勾配流（V-Path）の型レベル二項遷移関係の定義（Type Family）
--    アクティブ・ラーニングで収束が確認されたパスのみが 'True と判定されます。
-- ============================================================================
type family ValidTransition (from :: Symbol) (to :: Symbol) :: Bool where
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly35_2f2d" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly35_2f2c+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly33_2e3e" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly33_2e2d" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly31_2f2d" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly31_2f2c+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly29_8h4d" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly29_8h3c+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly27_2h2d" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly27_2h2c+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly25_2h2d" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly25_2h2c+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly24_2b6f" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly24_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly23_2h2d" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly23_2h2c+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly22_2b6f" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly22_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly21_2h2d" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly21_2h2c+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly20_2b6f" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly20_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly19_2h2d" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly19_2h2c+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly18_2b6f" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly18_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly17_2h2d" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly17_2h2c+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly16_2b6f" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly16_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly15_2h2d" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly15_2h2c+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly14_2b6f" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly14_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly13_2d1d" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly13_2d2c+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly12_2b6f" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly12_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly11_2h2e" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly11_8h4d" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly10_2b6f" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly10_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly09_8h4d" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly09_8h3c+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly08_2b6f" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly08_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly07_8h4d" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly06_2b6f" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly06_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly05_8h4d" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly04_2b6f" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly04_2b7g+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "Aigakari_Equilibrium" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly36_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly36_B*3g" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly35_2h2d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly35_2h2c+" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly34_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly34_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly33_2h2d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly33_2h2c+" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly32_S*1f" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly32_S*1h" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly31_2h2d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly31_2h2c+" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly30_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly30_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly29_2d1d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly29_2d2c+" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly28_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly28_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly27_2h2e" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly27_3e4d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly26_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly26_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly25_3e2d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly25_3e4d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly24_3e2d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly24_3e4d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly23_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly23_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly22_4f3e" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly22_S*1b" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly21_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly21_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly20_S*1b" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly20_S*1d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly19_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly19_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly18_S*1b" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly18_S*1d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly17_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly17_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly16_B*1b" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly16_B*1d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly15_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly15_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly14_B*1b" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly14_B*1d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly13_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly13_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly12_B*1b" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly12_B*1d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly11_B*1b" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly11_B*1d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly10_1a1b" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly10_3b3c" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly09_8h4d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly09_8h3c+" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly08_2b6f" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly08_2b7g+" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly07_8h4d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly06_2b6f" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly06_2b7g+" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly05_8h4d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly04_2b6f" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly04_2b7g+" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "Kakugawari_Equilibrium" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly42_8b8f" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly40_8b8f" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly39_B*1b" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly39_B*1d" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly38_8b8f" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly37_B*1b" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly37_B*1d" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly36_8b8f" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly35_B*1b" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly35_B*1d" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly34_8b8f" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly33_B*1b" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly33_B*1d" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly32_8b8f" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly31_B*1b" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly31_B*1d" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly30_8f7f" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly30_8f9f" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly28_8b8e" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly27_B*1b" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly27_B*1d" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly25_7g8f" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly25_B*1b" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly23_7g8f" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly23_B*1b" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly21_7g8f" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly21_B*1b" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly19_B*1b" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly19_B*1d" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly14_2b5e" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly14_2b6f" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly12_2b5e" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly12_2b6f" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly10_2b5e" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly10_2b6f" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly08_2b6f" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "Yagura_Equilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "Yagura_Equilibrium" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly38_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly38_7g5e" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly37_2d1e" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly37_2d3e" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly36_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly36_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly35_3c2d" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly35_9h9i+" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly34_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly34_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly33_3c2d" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly33_B*1e" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly32_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly32_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly31_3c2d" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly31_B*1e" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly30_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly30_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly29_B*1e" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly29_B*1g" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly28_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly28_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly27_B*1e" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly27_B*1g" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly26_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly26_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly25_B*1f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly25_B*1h" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly24_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly24_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly23_B*1f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly23_B*2f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly22_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly22_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly21_B*1f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly21_B*2f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly20_B*1f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly20_B*2f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly19_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly19_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly18_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly18_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly17_B*1f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly17_B*1h" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly16_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly16_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly14_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly14_G*1b" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly12_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly10_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "Shikenbisha_Equilibrium" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly30_1i1e" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly30_1i1d" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly29_3c1e" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly29_3c6f" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly28_1i1e" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly28_1i1d" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly27_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly27_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly26_1f4c" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly26_3e8e" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly25_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly25_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly24_1g4d" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly24_3f3c+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly23_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly23_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly22_2g5d" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly22_3d4d" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly21_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly21_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly20_3f3c+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly20_3f3c" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly19_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly19_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly18_3d4d" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly18_3d1d" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly17_3d4d" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly17_3d1d" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly16_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly16_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly15_2d1d" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly15_2d2c+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly14_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly14_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly13_2d1d" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly13_2d2c+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly12_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly12_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly11_2h2e" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly11_8h4d" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly10_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly10_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly09_8h4d" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly09_8h3c+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly08_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly08_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly07_8h4d" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly06_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly06_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly05_8h4d" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly04_2b6f" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly04_2b7g+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "Yokofudori_Equilibrium" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly28_8f6d" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly28_8f5c+" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly27_8f9f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly27_8f6f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly26_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly26_7g3c+" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly25_B*1e" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly25_B*1g" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly24_7f8f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly24_7f7d" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly23_B*1e" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly23_B*1g" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly22_7f8f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly22_7f7d" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly21_B*1e" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly21_B*1g" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly20_7g8f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly20_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly19_2a3c" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly19_B*1e" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly18_7g8f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly18_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly17_2a3c" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly17_B*1f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly16_7g8f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly16_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly15_2a3c" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly15_B*1f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly14_7g8f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly14_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly13_2a3c" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly13_2b6f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly12_7g8f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly12_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly11_2a3c" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly11_2b6f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly10_7g8f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly10_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly09_2b6f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly09_2b7g+" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly08_2b6f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly08_2b7g+" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly07_8h4d" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly06_2b6f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly06_2b7g+" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly05_8h4d" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly04_2b6f" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly04_2b7g+" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "Sankenbisha_Equilibrium" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly32_8h7i" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly32_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly31_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly31_B*1f" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly30_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly30_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly29_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly29_8d7e" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly28_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly28_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly27_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly27_8d7e" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly26_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly26_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly25_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly25_8d7e" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly24_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly24_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly23_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly23_8d7e" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly22_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly22_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly21_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly21_B*1f" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly20_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly20_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly19_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly19_B*1f" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly18_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly18_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly17_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly17_B*1f" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly16_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly16_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly15_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly15_B*1f" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly14_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly14_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly13_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly13_2b5e" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly12_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly12_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly11_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly11_2b5e" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly10_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly10_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly09_2b5e" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly09_2b6f" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly08_2b5e" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly08_2b6f" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly06_2b5e" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly06_2b6f" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly05_2b5e" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium_DevPly05_2b6f" = 'True
  ValidTransition "Start" "GokigenNakabisha_Equilibrium" = 'True
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
    (("Start", "Aigakari_Equilibrium_DevPly35_2f2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2e","7d8d","2e2f","8d7d","2f2d","3c2d","8h2b+","7d7f","7h8h","7f7h+","6i7h","R*9h"]),
    (("Start", "Aigakari_Equilibrium_DevPly35_2f2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2e","7d8d","2e2f","8d7d","2f2c+","3c2c","8h2b+","2c2b","P*2c","3b2c","S*1b","2b1b"]),
    (("Start", "Aigakari_Equilibrium_DevPly33_2e3e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2e","7d8d","2e3e","3c4b","8h2b+","3d3e","2b3b","4b3b","P*2b","8d8g+"]),
    (("Start", "Aigakari_Equilibrium_DevPly33_2e2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2e","7d8d","2e2d","3c2d","8h2b+","8d8g+","7h8g","P*8f","8g8f","P*8e"]),
    (("Start", "Aigakari_Equilibrium_DevPly31_2f2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2d","3c2d","8h2b+","7d7f","7h8h","7f7h+","6i7h","R*9h"]),
    (("Start", "Aigakari_Equilibrium_DevPly31_2f2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2c+","3c2c","8h2b+","2c2b","P*2c","3b2c","S*1b","2b1b"]),
    (("Start", "Aigakari_Equilibrium_DevPly29_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","8h4d","4c4d","S*4c","3b4c","P*2d","8d8g+","7h8g","2c2d"]),
    (("Start", "Aigakari_Equilibrium_DevPly29_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","8h3c+"]),
    (("Start", "Aigakari_Equilibrium_DevPly27_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","2h2d","3c2d","8h2b+","8b8g+","7h8g","P*8f","8g8f","P*8e"]),
    (("Start", "Aigakari_Equilibrium_DevPly27_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","2h2c+","3c2c","8h2b+","2c2b","P*2c","3b2c","S*1b","2b1b"]),
    (("Start", "Aigakari_Equilibrium_DevPly25_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","2h2d","3c2d","8h2b+","8b8g+","7h8g","R*7h","6h7h","P*8h"]),
    (("Start", "Aigakari_Equilibrium_DevPly25_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","2h2c+","3c2c","8h2b+","2c2b","P*2c","3b2c","B*1b","2b1b"]),
    (("Start", "Aigakari_Equilibrium_DevPly24_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","2b6f","8h6f","8b8g+","7h8g","P*8h","6f1a+","8h8i+","1a2a"]),
    (("Start", "Aigakari_Equilibrium_DevPly24_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","2b7g+","8i7g","8b8g+","7h8g","4b3a","2h2c+","3b2c","B*3b"]),
    (("Start", "Aigakari_Equilibrium_DevPly23_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","2h2d","2b8h+","2d2c+","3b2c","P*2b","8h7h","6h7h","8b8g+"]),
    (("Start", "Aigakari_Equilibrium_DevPly23_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","2h2c+","3b2c","8h2b+","8b8g+","7h8g","R*5h","6i5h","P*8h"]),
    (("Start", "Aigakari_Equilibrium_DevPly22_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","2b6f","8h6f","8b8g+","7h8g","P*8h","6f1a+","8h8i+","1a2a"]),
    (("Start", "Aigakari_Equilibrium_DevPly22_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","2b7g+","8i7g","8b8g+","7h8g","P*8f","2h2c+","3b2c","8g8f"]),
    (("Start", "Aigakari_Equilibrium_DevPly21_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","2h2d","2b8h+","2d2c+","3b2c","P*2b","8h7h","6i7h","8b8g+"]),
    (("Start", "Aigakari_Equilibrium_DevPly21_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","2h2c+","3b2c","8h2b+","8b8g+","7h8g","R*5h","4i5h","P*2h"]),
    (("Start", "Aigakari_Equilibrium_DevPly20_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","2b6f","8h6f","8b8g+","7h8g","P*8h","6f1a+","8h8i+","1a2a"]),
    (("Start", "Aigakari_Equilibrium_DevPly20_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","2b7g+","8i7g","8b8g+","7h8g","P*8f","2h2c+","3b2c","8g8f"]),
    (("Start", "Aigakari_Equilibrium_DevPly19_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","2h2d","2b8h+","2d2c+","3b2c","P*2b","8h7h","6i7h","8b8g+"]),
    (("Start", "Aigakari_Equilibrium_DevPly19_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","2h2c+","3b2c","8h2b+","8b2b","B*5b","2b5b","P*2b","5b2b"]),
    (("Start", "Aigakari_Equilibrium_DevPly18_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","2b6f","8g8f","6f8h+","2h2c+","3b2c","R*5b","4a5b","P*2b"]),
    (("Start", "Aigakari_Equilibrium_DevPly18_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","2b7g+","8i7g","8f8g+","7h8g","P*8f","2h2c+","3b2c","8g8f"]),
    (("Start", "Aigakari_Equilibrium_DevPly17_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","2h2d","8f8h+","2d2c+","3b2c","P*8b","8h7h","6i7h","2b9i+"]),
    (("Start", "Aigakari_Equilibrium_DevPly17_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","2h2c+","3b2c","8h2b+","8f8i+","7h8i","R*5h","4i5h","N*5f"]),
    (("Start", "Aigakari_Equilibrium_DevPly16_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","2b6f","8h6f","8b8f","6f1a+","8f8i+","7h8i","N*5f","5g5f"]),
    (("Start", "Aigakari_Equilibrium_DevPly16_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","2b7g+","8i7g","8b8f","2h2c+","3b2c","B*5b","8f8h+","5b4a+"]),
    (("Start", "Aigakari_Equilibrium_DevPly15_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","2h2d","2b8h+","2d2c+","3b2c","8g8f","8h7h","6i7h","8b8f"]),
    (("Start", "Aigakari_Equilibrium_DevPly15_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","2h2c+","3b2c","8h2b+","8b2b","8g8f","B*5h","4i5h","R*4i"]),
    (("Start", "Aigakari_Equilibrium_DevPly14_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","2b6f","8h6f","8b5b","6f1a+","2a3c","1a3c","3b3c","N*4c"]),
    (("Start", "Aigakari_Equilibrium_DevPly14_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","2b7g+","8i7g","8b5b","2h2c+","3b2c","7g8e","R*5h","4i5h"]),
    (("Start", "Aigakari_Equilibrium_DevPly13_2d1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d1d","2b8h+","1d1c+","1a1c","P*2b","8h7h","6i7h","1c1g+"]),
    (("Start", "Aigakari_Equilibrium_DevPly13_2d2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2c+","3b2c","8h2b+","8b2b","B*5b","2b5b","P*2b","5b2b"]),
    (("Start", "Aigakari_Equilibrium_DevPly12_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","2b6f","8h6f","P*2h","2d2a+","3b2a","N*4c"]),
    (("Start", "Aigakari_Equilibrium_DevPly12_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","2b7g+","8i7g","P*2h","2d2a+","3b2a","N*4c"]),
    (("Start", "Aigakari_Equilibrium_DevPly11_2h2e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2e","2b8h+","2e2d","8h7h","6i7h","S*5h","4i5h","B*4i"]),
    (("Start", "Aigakari_Equilibrium_DevPly11_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","8h4d","2b4d","2h2d","4d9i+","2d2a+","3b2a","N*4c"]),
    (("Start", "Aigakari_Equilibrium_DevPly10_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2b6f","8h6f","2c2d","6f1a+","8b5b","1a2a","3b2a","N*4c"]),
    (("Start", "Aigakari_Equilibrium_DevPly10_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2b7g+","8i7g","2c2d","2h2d","P*2h","2d2a+","3b2a","N*4c"]),
    (("Start", "Aigakari_Equilibrium_DevPly09_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7h7g","L*4h"]),
    (("Start", "Aigakari_Equilibrium_DevPly09_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h3c+","2a3c","2h5h","3c2e","8i7g","2b7g+","7h7g","2e3g+"]),
    (("Start", "Aigakari_Equilibrium_DevPly08_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2b6f","8h6f","8b5b","6f1a+","2a3c","1a3c","3a4b","N*4c"]),
    (("Start", "Aigakari_Equilibrium_DevPly08_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2b7g+","8i7g","8b5b","7g8e","2a3c","8h3c+","3a4b","N*4c"]),
    (("Start", "Aigakari_Equilibrium_DevPly07_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7i6h","7g6h"]),
    (("Start", "Aigakari_Equilibrium_DevPly07_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h3c+","2a3c","2h5h","3c2e","8i7g","2b7g+","7i6h","7g6h"]),
    (("Start", "Aigakari_Equilibrium_DevPly06_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2b6f","8h6f","8b5b","6f1a+","2a3c","1a3c","3a4b","N*4c"]),
    (("Start", "Aigakari_Equilibrium_DevPly06_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2b7g+","8i7g","8b5b","B*4b","3a4b","7g6e","B*5h","2h5h"]),
    (("Start", "Aigakari_Equilibrium_DevPly05_8h4d"), ["7g7f","3c3d","2g2f","8c8d","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7i6h","7g6h"]),
    (("Start", "Aigakari_Equilibrium_DevPly05_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","8h3c+","2a3c","2h5h","B*4h","3i4h","3c4e","B*5b","8b5b"]),
    (("Start", "Aigakari_Equilibrium_DevPly04_2b6f"), ["7g7f","3c3d","2g2f","2b6f","8h6f","8b5b","6f1a+","2a3c","1a3c","3a4b","N*4c"]),
    (("Start", "Aigakari_Equilibrium_DevPly04_2b7g+"), ["7g7f","3c3d","2g2f","2b7g+","8i7g","8b5b","B*4b","3a4b","7g6e","B*5h","2h5h"]),
    (("Start", "Aigakari_Equilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7i6h","7g6h"]),
    (("Start", "Aigakari_Equilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2a3c","2h5h","B*4h","3i4h","3c4e","B*5b","8b5b"]),
    (("Start", "Aigakari_Equilibrium"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2e","7d8d","2e2f","8d7d","8h3c+"]),
    (("Start", "Kakugawari_Equilibrium_DevPly36_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","3b3c","B*1f","B*1h","1i1h","4b4a","2h2c+","2b2c","1f4c+","3c4c","B*4b"]),
    (("Start", "Kakugawari_Equilibrium_DevPly36_B*3g"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","3b3c","B*1f","B*3g","2i3g","4b4a","2h2c+","2b2c","1f4c+","3c4c","B*4b"]),
    (("Start", "Kakugawari_Equilibrium_DevPly35_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","3b3c","2h2d","2c2d","P*3b","4b3b","B*4b","3a4b","6h7i","B*6i"]),
    (("Start", "Kakugawari_Equilibrium_DevPly35_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","3b3c","2h2c+","2b2c","P*3b","4b3b","P*2b","3b2b","B*1b","2b1b"]),
    (("Start", "Kakugawari_Equilibrium_DevPly34_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","B*1f","1g1f","2a3c","2h2c+","2b2c","B*4a","4b4a","B*4b"]),
    (("Start", "Kakugawari_Equilibrium_DevPly34_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","B*1h","1i1h","2a3c","2h2c+","2b2c","B*4a","4b4a","B*4b"]),
    (("Start", "Kakugawari_Equilibrium_DevPly33_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","2h2d","2c2d","B*4a","4b4a","6h7i","B*6i","7i6i","B*5i"]),
    (("Start", "Kakugawari_Equilibrium_DevPly33_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","2h2c+","2b2c","B*4a","4b4a","6h5i","B*6i","5h6i","B*5h"]),
    (("Start", "Kakugawari_Equilibrium_DevPly32_S*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*1f","1g1f","B*6i","6h6i","4b3a","2h2c+","2b2c","S*4a"]),
    (("Start", "Kakugawari_Equilibrium_DevPly32_S*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*1h","1i1h","B*6i","6h6i","4b3a","2h2c+","2b2c","S*4a"]),
    (("Start", "Kakugawari_Equilibrium_DevPly31_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","2h2d","2c2d","S*4a","4b4a","B*3a","4a3a","P*3c","2a3c"]),
    (("Start", "Kakugawari_Equilibrium_DevPly31_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","2h2c+","2b2c","S*4a","4b4a","B*3a","4a3a","P*2b","3b2b"]),
    (("Start", "Kakugawari_Equilibrium_DevPly30_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","B*1f","1g1f","3d3e","2h2c+","2b2c","S*4a","4b4a","B*3a"]),
    (("Start", "Kakugawari_Equilibrium_DevPly30_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","B*1h","1i1h","3d3e","2h2c+","2b2c","S*4a","4b4a","B*3a"]),
    (("Start", "Kakugawari_Equilibrium_DevPly29_2d1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d1d","1c1d","3e3d","B*5h","6h5h","R*4h","5h4h","4b5a"]),
    (("Start", "Kakugawari_Equilibrium_DevPly29_2d2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2c+","2b2c","3e3d","2c3d","S*4a","4b4a","B*3a","4a3a"]),
    (("Start", "Kakugawari_Equilibrium_DevPly28_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","B*1f","1g1f","3d3e","2d2b+","3b2b","P*3b","4b3b","P*3c"]),
    (("Start", "Kakugawari_Equilibrium_DevPly28_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","B*1h","1i1h","3d3e","2d2b+","3b2b","P*3b","4b3b","P*3c"]),
    (("Start", "Kakugawari_Equilibrium_DevPly27_2h2e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2e","2d2e","3e3d","B*5h","6h5h","R*4h","5h4h","P*3h"]),
    (("Start", "Kakugawari_Equilibrium_DevPly27_3e4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","3e4d","4c4d","2h2d","S*5h","2d2b+","3b2b","6h5h","B*4h"]),
    (("Start", "Kakugawari_Equilibrium_DevPly26_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","B*1f","1g1f","3d3e","2d2c+","2b2c","2h2c+","3b2c","P*3b"]),
    (("Start", "Kakugawari_Equilibrium_DevPly26_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","B*1h","1i1h","3d3e","2d2c+","2b2c","2h2c+","3b2c","P*3b"]),
    (("Start", "Kakugawari_Equilibrium_DevPly25_3e2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","3e2d","2c2d","2e2d","S*5h","6h5h","B*4h","5h4h","4b3a"]),
    (("Start", "Kakugawari_Equilibrium_DevPly25_3e4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","3e4d","4c4d","S*4a","4b4a","B*3a","4a3a","P*3c","2a3c"]),
    (("Start", "Kakugawari_Equilibrium_DevPly24_3e2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3e2d","2c2d","2e2d","S*5h","6h5h","B*4h","5h4h","4b3a"]),
    (("Start", "Kakugawari_Equilibrium_DevPly24_3e4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3e4d","4c4d","S*4a","4b4a","B*3a","4a3a","P*3c","2a3c"]),
    (("Start", "Kakugawari_Equilibrium_DevPly23_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","B*1f","1g1f","4b3a","S*4a","3a4a","B*3a","4a3a","B*4a"]),
    (("Start", "Kakugawari_Equilibrium_DevPly23_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","B*1h","1i1h","4b3a","S*4a","3a4a","B*3a","4a3a","B*4a"]),
    (("Start", "Kakugawari_Equilibrium_DevPly22_4f3e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","4f3e","3d3e","S*4a","4b4a","B*3a","4a3a","P*3c","2a3c"]),
    (("Start", "Kakugawari_Equilibrium_DevPly22_S*1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","S*1b","1a1b","B*4a","4b4a","6h5i","S*5h","2h5h","B*6i"]),
    (("Start", "Kakugawari_Equilibrium_DevPly21_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","B*1f","1g1f","4b5a","S*4a","5a4a","B*3a","4a3a","B*4a"]),
    (("Start", "Kakugawari_Equilibrium_DevPly21_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","B*1h","1i1h","4b5a","S*4a","5a4a","B*3a","4a3a","B*4a"]),
    (("Start", "Kakugawari_Equilibrium_DevPly20_S*1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","S*1b","1a1b","B*4a","4b4a","P*3c","2a3c","6h7i","3c2e"]),
    (("Start", "Kakugawari_Equilibrium_DevPly20_S*1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","S*1d","1c1d","B*4a","4b4a","P*3c","2a3c","6h7i","3c2e"]),
    (("Start", "Kakugawari_Equilibrium_DevPly19_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","B*1f","1g1f","4b5a","S*4a","5a4a","B*3a","4a3a","B*4a"]),
    (("Start", "Kakugawari_Equilibrium_DevPly19_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","B*1h","1i1h","4b5a","S*4a","5a4a","B*3a","4a3a","B*4a"]),
    (("Start", "Kakugawari_Equilibrium_DevPly18_S*1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","S*1b","1a1b","B*4a","5a4a","P*3c","2a3c","3g4f","3c2e"]),
    (("Start", "Kakugawari_Equilibrium_DevPly18_S*1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","S*1d","1c1d","B*4a","5a4a","P*3c","2a3c","3g4f","3c2e"]),
    (("Start", "Kakugawari_Equilibrium_DevPly17_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","B*1f","1g1f","2a3c","B*4a","5a4a","B*3a","4a3a","P*3e"]),
    (("Start", "Kakugawari_Equilibrium_DevPly17_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","B*1h","1i1h","2a3c","B*4a","5a4a","B*3a","4a3a","P*3e"]),
    (("Start", "Kakugawari_Equilibrium_DevPly16_B*1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","B*1b","1a1b","P*3b","4a3b","2h5h","B*3i","4i3i","B*4i"]),
    (("Start", "Kakugawari_Equilibrium_DevPly16_B*1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","B*1d","1c1d","P*3b","4a3b","2h5h","B*3i","4i3i","B*4i"]),
    (("Start", "Kakugawari_Equilibrium_DevPly15_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","B*1f","1g1f","2a3c","B*5b","4a5b","B*4a","5a4a","2e2d"]),
    (("Start", "Kakugawari_Equilibrium_DevPly15_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","B*1h","1i1h","2a3c","B*5b","4a5b","B*4a","5a4a","2e2d"]),
    (("Start", "Kakugawari_Equilibrium_DevPly14_B*1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","B*1b","1a1b","8i7g","B*5h","4i5h","B*4i","5i4i","2a3c"]),
    (("Start", "Kakugawari_Equilibrium_DevPly14_B*1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","B*1d","1c1d","8i7g","B*5h","4i5h","B*4i","5i4i","2a3c"]),
    (("Start", "Kakugawari_Equilibrium_DevPly13_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","B*1f","1g1f","2a3c","B*5b","4a5b","B*4a","5a4a","2e2d"]),
    (("Start", "Kakugawari_Equilibrium_DevPly13_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","B*1h","1i1h","2a3c","B*5b","4a5b","B*4a","5a4a","2e2d"]),
    (("Start", "Kakugawari_Equilibrium_DevPly12_B*1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","B*1b","1a1b","2h5h","B*4h","3i4h","B*3i","4i3i","8b5b"]),
    (("Start", "Kakugawari_Equilibrium_DevPly12_B*1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","B*1d","1c1d","2h5h","B*4h","3i4h","B*7i","6i7i","8b5b"]),
    (("Start", "Kakugawari_Equilibrium_DevPly11_B*1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","B*1b","1a1b","2h5h","B*4h","3i4h","B*3i","4i3i","8b5b"]),
    (("Start", "Kakugawari_Equilibrium_DevPly11_B*1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","B*1d","1c1d","2h5h","B*4h","3i4h","B*7i","6i7i","8b5b"]),
    (("Start", "Kakugawari_Equilibrium_DevPly10_1a1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","1a1b","2b3b","8b3b","S*5b","3b5b","B*3a","4a3a","2h5h"]),
    (("Start", "Kakugawari_Equilibrium_DevPly10_3b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b3c","2b3c","2a3c","S*5b","8b5b","B*3a","4a3a","2h5h"]),
    (("Start", "Kakugawari_Equilibrium_DevPly09_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7h7g","L*4h"]),
    (("Start", "Kakugawari_Equilibrium_DevPly09_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h3c+","2a3c","2h5h","3c2e","8i7g","2b7g+","7h7g","2e3g+"]),
    (("Start", "Kakugawari_Equilibrium_DevPly08_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2b6f","8h6f","8b5b","6f1a+","2a3c","1a3c","3a4b","N*4c"]),
    (("Start", "Kakugawari_Equilibrium_DevPly08_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2b7g+","8i7g","8b5b","7g8e","2a3c","8h3c+","3a4b","N*4c"]),
    (("Start", "Kakugawari_Equilibrium_DevPly07_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7i6h","7g6h"]),
    (("Start", "Kakugawari_Equilibrium_DevPly07_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h3c+","2a3c","2h5h","3c2e","8i7g","2b7g+","7i6h","7g6h"]),
    (("Start", "Kakugawari_Equilibrium_DevPly06_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2b6f","8h6f","8b5b","6f1a+","2a3c","1a3c","3a4b","N*4c"]),
    (("Start", "Kakugawari_Equilibrium_DevPly06_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2b7g+","8i7g","8b5b","B*4b","3a4b","7g6e","B*5h","2h5h"]),
    (("Start", "Kakugawari_Equilibrium_DevPly05_8h4d"), ["7g7f","3c3d","2g2f","8c8d","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7i6h","7g6h"]),
    (("Start", "Kakugawari_Equilibrium_DevPly05_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","8h3c+","2a3c","2h5h","B*4h","3i4h","3c4e","B*5b","8b5b"]),
    (("Start", "Kakugawari_Equilibrium_DevPly04_2b6f"), ["7g7f","3c3d","2g2f","2b6f","8h6f","8b5b","6f1a+","2a3c","1a3c","3a4b","N*4c"]),
    (("Start", "Kakugawari_Equilibrium_DevPly04_2b7g+"), ["7g7f","3c3d","2g2f","2b7g+","8i7g","8b5b","B*4b","3a4b","7g6e","B*5h","2h5h"]),
    (("Start", "Kakugawari_Equilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7i6h","7g6h"]),
    (("Start", "Kakugawari_Equilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2a3c","2h5h","B*4h","3i4h","3c4e","B*5b","8b5b"]),
    (("Start", "Kakugawari_Equilibrium"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","3b3c","B*1f"]),
    (("Start", "Yagura_Equilibrium_DevPly42_8b8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","2e2d","4a4b","B*1f","9a9b","8h7i","8b8f","8g8f","2c2d","1f3d","3c3d","2h2d","3d2d","P*2c"]),
    (("Start", "Yagura_Equilibrium_DevPly40_8b8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","2e2d","4a4b","B*1f","8b8f","8g8f","2c2d","1f3d","3c3d","2h2d","3d2d","P*2c"]),
    (("Start", "Yagura_Equilibrium_DevPly39_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","2e2d","4a4b","B*1b","1a1b","2d2c+","3b2c","2h2c+","3c2c","P*2d","2c2d"]),
    (("Start", "Yagura_Equilibrium_DevPly39_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","2e2d","4a4b","B*1d","1c1d","2d2c+","3b2c","2h2c+","3c2c","P*2d","2c2d"]),
    (("Start", "Yagura_Equilibrium_DevPly38_8b8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","2e2d","8b8f","8g8f","2c2d","2h2d","3c2d","P*2c","2d2c","B*2d"]),
    (("Start", "Yagura_Equilibrium_DevPly37_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","B*1b","1a1b","8h9h","8b8g+","9h8g","P*8f","7g8f","P*8h"]),
    (("Start", "Yagura_Equilibrium_DevPly37_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","B*1d","1c1d","8h9h","8b8g+","9h8g","P*8f","7g8f","P*8h"]),
    (("Start", "Yagura_Equilibrium_DevPly36_8b8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","8b8f","8g8f","P*8g","8h8g","3c4b","B*4c","3b4c","R*3b"]),
    (("Start", "Yagura_Equilibrium_DevPly35_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","B*1b","1a1b","8h9h","8b8g+","9h8g","P*8f","7g8f","P*8h"]),
    (("Start", "Yagura_Equilibrium_DevPly35_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","B*1d","1c1d","8h9h","8b8g+","9h8g","P*8f","7g8f","P*8h"]),
    (("Start", "Yagura_Equilibrium_DevPly34_8b8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","8b8f","8g8f","P*8g","8h8g","3c3b","B*3a","3b3a","R*3b"]),
    (("Start", "Yagura_Equilibrium_DevPly33_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","B*1b","1a1b","8h9h","8b8g+","9h8g","P*8f","7g8f","P*8h"]),
    (("Start", "Yagura_Equilibrium_DevPly33_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","B*1d","1c1d","8h9h","8b8g+","9h8g","P*8f","7g8f","P*8h"]),
    (("Start", "Yagura_Equilibrium_DevPly32_8b8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","8b8f","8g8f","P*8g","8h8g","3c4b","B*4c","3b4c","R*3b"]),
    (("Start", "Yagura_Equilibrium_DevPly31_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","B*1b","1a1b","8h9h","8b8g+","9h8g","P*8f","7g8f","P*8h"]),
    (("Start", "Yagura_Equilibrium_DevPly31_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","B*1d","1c1d","8h9h","8b8g+","9h8g","P*8f","7g8f","P*8h"]),
    (("Start", "Yagura_Equilibrium_DevPly30_8f7f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f7f","7g7f","P*8f","8g8f","P*8g","7f8g","3c4b","B*4c"]),
    (("Start", "Yagura_Equilibrium_DevPly30_8f9f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f9f","9g9f","P*8f","8g8f","3c4b","B*4c","3b4c","R*3b"]),
    (("Start", "Yagura_Equilibrium_DevPly28_8b8e"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8e","8f8e","3c4b","B*4c","3b4c","R*3b","4b3b","8h9h"]),
    (("Start", "Yagura_Equilibrium_DevPly27_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","B*1b","1a1b","8g8f","8b8f","7g8f","P*8g","8h8g","B*7g"]),
    (("Start", "Yagura_Equilibrium_DevPly27_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","B*1d","1c1d","8g8f","8b8f","7g8f","P*8g","8h8g","B*7g"]),
    (("Start", "Yagura_Equilibrium_DevPly25_7g8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","7g8f","8e8f","8g8f","8b8f","8h7i","8f8i+","7i8i","P*8h"]),
    (("Start", "Yagura_Equilibrium_DevPly25_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","B*1b","1a1b","8h7i","B*7h","6i7h","3c4b","B*4c","3b4c"]),
    (("Start", "Yagura_Equilibrium_DevPly23_7g8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","7g8f","8e8f","8g8f","8b8f","8h7i","8f8i+","7i8i","P*8h"]),
    (("Start", "Yagura_Equilibrium_DevPly23_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","B*1b","1a1b","8h7i","B*7h","6i7h","3c4b","B*4c","3b4c"]),
    (("Start", "Yagura_Equilibrium_DevPly21_7g8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7g8f","8e8f","8g8f","8b8f","B*4c","3b4c","7h7i","8f8i+"]),
    (("Start", "Yagura_Equilibrium_DevPly21_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","B*1b","1a1b","7h7i","B*7h","6i7h","3c4b","B*4c","3b4c"]),
    (("Start", "Yagura_Equilibrium_DevPly19_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","B*1b","1a1b","8h7i","B*7h","6i7h","3c4b","B*4c","3b4c"]),
    (("Start", "Yagura_Equilibrium_DevPly19_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","B*1d","1c1d","8h7i","B*7h","6i7h","3c4b","B*4c","3b4c"]),
    (("Start", "Yagura_Equilibrium_DevPly14_2b5e"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","2b5e","5f5e","4b3a","B*2b","3a2b","6h7i","B*7h","6i7h"]),
    (("Start", "Yagura_Equilibrium_DevPly14_2b6f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","2b6f","7g6f","4b3a","B*2b","3a2b","6h7i","B*7h","6i7h"]),
    (("Start", "Yagura_Equilibrium_DevPly12_2b5e"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","2b5e","5f5e","4b5a","B*5b","4a5b","5e5d","5c5d","5i4i"]),
    (("Start", "Yagura_Equilibrium_DevPly12_2b6f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","2b6f","7g6f","4b5a","B*5b","4a5b","6f5e","B*4i","5i4i"]),
    (("Start", "Yagura_Equilibrium_DevPly10_2b5e"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","2b5e","5f5e","2a3c","B*5b","4a5b","5e5d","5c5d","2h5h"]),
    (("Start", "Yagura_Equilibrium_DevPly10_2b6f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","2b6f","7g6f","2a3c","B*5b","4a5b","6f5e","B*5h","2h5h"]),
    (("Start", "Yagura_Equilibrium_DevPly08_2b6f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","2b6f","7g6f","8b5b","B*4b","3a4b","6f5e","B*5h","2h5h"]),
    (("Start", "Yagura_Equilibrium_DevPly03_8h4d"), ["7g7f","8c8d","8h4d","4c4d","2h5h","B*4h","3i4h","8b5b","B*6b","7a6b"]),
    (("Start", "Yagura_Equilibrium_DevPly03_8h3c+"), ["7g7f","8c8d","8h3c+","2a3c","2h5h","B*4h","3i4h","3c4e","B*5b","8b5b"]),
    (("Start", "Yagura_Equilibrium"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","2e2d","4a4b","B*1f","9a9b","8h7i"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly38_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","3c2d","6f6e","7g8f","8e8f","9i9h","8f8g+","S*4a","8g7h","4a3b+","3a3b"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly38_7g5e"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","3c2d","6f6e","7g5e","5d5e","9i9h","B*1h","2h1h","B*1g","2i1g","2d1e"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly37_2d1e"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","3c2d","2d1e","1f1e","9h8i+","1e1d","8i7i","6i7i","1a1d","1i1d"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly37_2d3e"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","3c2d","2d3e","3f3e","9h8i+","3e3d","8i7i","6i7i","P*3h","3d3c+"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly36_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","7g8f","8e8f","9i9h","8f8g+","2d2c+","8g7h","2c3b","3a3b"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly36_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","S*1b","1a1b","9i9h","3c2d","S*4a","3a4a","G*3a","4a3a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly35_3c2d"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","3c2d","2e2d","9h8i+","2d2c+","8i7i","2c3b","3a3b","6i7i"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly35_9h9i+"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","9h9i+","7g9i","L*1h","2h1h","B*1g","2i1g","1d1e","1f1e"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly34_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","7g8f","8e8f","9i9h","8f8g+","S*4a","8g7h","4a3b+","3a3b"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly34_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","S*1b","1a1b","9i9h","B*1h","2h1h","1d1e","1f1e","1b1e"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly33_3c2d"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","3c2d","2e2d","2c2d","S*4a","3a4a","S*3a","4a3a","G*4a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly33_B*1e"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","B*1e","1f1e","1d1e","1i1e","1a1e","S*4a","3a4a","G*3a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly32_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","7g8f","8e8f","8g8f","B*1h","2h1h","B*1g","2i1g","1d1e"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly32_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","S*1b","1a1b","G*4a","3a4a","2h3i","S*3h","7h3h","G*4i"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly31_3c2d"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3c2d","2e2d","2c2d","S*4a","3a4a","S*3a","4a3a","G*4a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly31_B*1e"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","B*1e","1f1e","1d1e","1i1e","1a1e","S*4a","3a4a","G*4b"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly30_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","7g8f","8e8f","8g8f","B*1h","2h1h","B*1g","2i1g","1d1e"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly30_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","S*1b","1a1b","G*4a","3a4a","2h3i","S*3h","7h3h","G*4i"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly29_B*1e"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","B*1e","1f1e","1d1e","1i1e","1a1e","S*4a","3a4a","G*3a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly29_B*1g"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","B*1g","1i1g","1d1e","1f1e","1a1e","1g1e","P*1h","2h1h"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly28_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","7g8f","8e8f","8g8f","B*1h","2h1h","B*1g","2i1g","4b3a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly28_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","S*1b","1a1b","G*4a","4b4a","2h3i","S*3h","7h3h","G*4i"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly27_B*1e"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","B*1e","1f1e","1d1e","1i1e","1a1e","S*4a","4b4a","G*3a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly27_B*1g"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","B*1g","1i1g","4b3a","S*4a","3a4a","G*3a","4a3a","B*4a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly26_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","7g8f","8e8f","8g8f","B*1h","2h1h","B*1g","2i1g","4b3a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly26_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","S*1b","1a1b","G*4a","4b4a","2h3i","S*3h","7h3h","G*4i"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly25_B*1f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","B*1f","1g1f","4b3a","S*4a","3a4a","G*3a","4a3a","B*4a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly25_B*1h"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","B*1h","2h1h","4b3a","S*4a","3a4a","G*3a","4a3a","B*4a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly24_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","7g8f","8e8f","8g8f","B*1h","2h1h","B*2h","7h2h","4b3a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly24_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","S*1b","1a1b","G*4a","4b4a","2h3i","S*3h","7h3h","G*4i"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly23_B*1f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","B*1f","1g1f","4b3a","S*4a","3a4a","G*3a","4a3a","B*4a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly23_B*2f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","B*2f","2g2f","4b3a","S*4a","3a4a","G*3a","4a3a","B*4a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly22_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","7g8f","8e8f","8g8f","8b8f","S*4a","4b4a","G*3a","4a3a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly22_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","S*1b","1a1b","G*4a","4b4a","7h4h","S*2h","4h2h","G*1f"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly21_B*1f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","B*1f","1g1f","4b3a","S*4a","3a4a","G*3a","4a3a","B*4a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly21_B*2f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","B*2f","2g2f","4b3a","S*4a","3a4a","G*3a","4a3a","B*4a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly20_B*1f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","B*1f","1g1f","4b3a","S*4a","3a4a","G*3a","4a3a","B*4a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly20_B*2f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","B*2f","2g2f","4b3a","S*4a","3a4a","G*3a","4a3a","B*4a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly19_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","7g8f","8e8f","8g8f","8b8f","S*4a","4b4a","G*3a","4a3a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly19_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","S*1b","1a1b","G*4a","4b4a","2h3i","S*3h","6h3h","G*4i"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly18_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7g8f","8e8f","8g8f","8b8f","S*4a","4b4a","G*3a","4a3a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly18_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","S*1b","1a1b","G*4a","4b4a","2h3i","S*3h","6h3h","G*4i"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly17_B*1f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","B*1f","1g1f","4b3a","S*4a","3a4a","G*3a","4a3a","B*4a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly17_B*1h"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","B*1h","1i1h","4b3a","S*4a","3a4a","G*3a","4a3a","B*4a"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly16_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","7g8f","8e8f","8g8f","2b6f","6h6f","8b8f","6f6c+","6b6c"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly16_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","S*1b","1a1b","G*4a","4b4a","3h4i","2b6f","6h6f","S*3i"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly14_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","7g8f","8e8f","G*5b"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly14_G*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","G*1b","1a1b","4h4i","2b6f","6h6f","G*3i","4i3i","2a3c","G*5b"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly12_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","7g8f","8e8f","8g8f","2b6f","6h6f","8b8f","6f6c+","6b6c"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly10_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7g8f","8e8f","8g8f","2b6f","6h6f","8b8f","6f6c+","8f8i+"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7i6h","7g6h"]),
    (("Start", "Shikenbisha_Equilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2a3c","2h5h","B*4h","3i4h","3c4e","B*5b","8b5b"]),
    (("Start", "Shikenbisha_Equilibrium"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","3c2d","6f6e"]),
    (("Start", "Yokofudori_Equilibrium_DevPly30_1i1e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f2f","B*3c","2f2e","3c5a+","1i1e"," *9b","3e3b+","4a3b","1e1c+","8h7h","6i7h","2b9i+"]),
    (("Start", "Yokofudori_Equilibrium_DevPly30_1i1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f2f","B*3c","2f2e","3c5a+","1i1d"," *9b","3e3b+","4a3b","1d1c+","8h7h","6i7h","2b9i+"]),
    (("Start", "Yokofudori_Equilibrium_DevPly29_3c1e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f2f","B*3c","2f2e","3c1e","1i1e","8h7h","6i7h","2b9i+","3e3b+","4a3b","1e1c+"]),
    (("Start", "Yokofudori_Equilibrium_DevPly29_3c6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f2f","B*3c","2f2e","3c6f","6g6f","8h7h","6i7h","2b6f","3e3b+","4a3b","1i1c+"]),
    (("Start", "Yokofudori_Equilibrium_DevPly28_1i1e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f2f","B*3c","1i1e","8h7h","6i7h","3c9i+","3e3b+","4a3b","1e1c+","1a1c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly28_1i1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f2f","B*3c","1i1d","8h7h","6i7h","3c9i+","3e3b+","4a3b","1d1c+","1a1c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly27_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f2f","2b6f","6g6f","8h7h","6i7h","S*5h","4i5h","B*4i","5i4i"]),
    (("Start", "Yokofudori_Equilibrium_DevPly27_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f2f","2b7g+","8i7g","8h7h","6i7h","S*5h","4i5h","B*4i","5i4i"]),
    (("Start", "Yokofudori_Equilibrium_DevPly26_1f4c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f4c","3b4c","1i1c+","8h7h","6i7h","2b9i+","3e3a+","4a3a"]),
    (("Start", "Yokofudori_Equilibrium_DevPly26_3e8e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","3e8e","8h8e","1f4c","3b4c","1i1c+","2b9i+","P*2b","1a1c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly25_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","2b6f","3f6f","8h7h","6i7h","S*5h","4i5h","B*4i","5i4i"]),
    (("Start", "Yokofudori_Equilibrium_DevPly25_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","2b7g+","8i7g","8h7h","6i7h","S*5h","4i5h","B*4i","5i4i"]),
    (("Start", "Yokofudori_Equilibrium_DevPly24_1g4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g4d","2b4d","3f3b+","4a3b","1i1c+","8h7h","6i7h","4d9i+"]),
    (("Start", "Yokofudori_Equilibrium_DevPly24_3f3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","3f3c+","2a3c","1g5c","8h7h","5c5b"]),
    (("Start", "Yokofudori_Equilibrium_DevPly23_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","2b6f","6g6f","8h7h","6i7h","S*5h","4i5h","B*4i","5i4i"]),
    (("Start", "Yokofudori_Equilibrium_DevPly23_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","2b7g+","8i7g","8h7h","6i7h","S*5h","4i5h","B*4i","5i4i"]),
    (("Start", "Yokofudori_Equilibrium_DevPly22_2g5d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g5d","5c5d","3d3b+","4a3b","S*4a","5a4a","P*2c","8h7h"]),
    (("Start", "Yokofudori_Equilibrium_DevPly22_3d4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","3d4d","2b4d","2g6c","8h7h","6i7h","4d9i+","6c4a","3b4a","G*5b"]),
    (("Start", "Yokofudori_Equilibrium_DevPly21_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","2b6f","3f6f","8h7h","6i7h","S*5h","4i5h","B*4i","5i4i"]),
    (("Start", "Yokofudori_Equilibrium_DevPly21_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","2b7g+","8i7g","8h7h","6i7h","S*5h","4i5h","B*4i","5i4i"]),
    (("Start", "Yokofudori_Equilibrium_DevPly20_3f3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","3f3c+","2a3c","4e6c+","8h7h","6i7h","S*5h","4i5h","B*4i"]),
    (("Start", "Yokofudori_Equilibrium_DevPly20_3f3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","3f3c","2a3c","4e6c+","8h7h","6i7h","S*5h","4i5h","B*4i"]),
    (("Start", "Yokofudori_Equilibrium_DevPly19_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","2b6f","6g6f","8h7h","6i7h","S*5h","4i5h","B*4i","5i4i"]),
    (("Start", "Yokofudori_Equilibrium_DevPly19_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","2b7g+","8i7g","8h7h","6i7h","S*5h","4i5h","B*4i","5i4i"]),
    (("Start", "Yokofudori_Equilibrium_DevPly18_3d4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","3d4d","2b4d","P*2b","8h7h","6i7h","4d9i+","2b2a+","9i8i","N*4c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly18_3d1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","3d1d","1c1d","P*8b","8h7h","6i7h","2b9i+","8b8a+","9i8i","N*4c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly17_3d4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","3d4d","2b4d","P*2b","8h7h","6i7h","4d9i+","2b2a+","9i8i","N*4c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly17_3d1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","3d1d","1c1d","P*8b","8h7h","6i7h","2b9i+","8b8a+","9i8i","N*4c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly16_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","2b6f","8h6f","8f8i+","7h8i","P*3h","3d3b+","4a3b","6f1a+"]),
    (("Start", "Yokofudori_Equilibrium_DevPly16_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","2b7g+","8i7g","8f8h+","3d3b+","4a3b","S*4a","5a4a","B*3a"]),
    (("Start", "Yokofudori_Equilibrium_DevPly15_2d1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d1d","8f8h+","1d1c+","1a1c","P*8b","8h7h","6i7h","2b9i+"]),
    (("Start", "Yokofudori_Equilibrium_DevPly15_2d2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d2c+","3b2c","8h2b+","8f8i+","7h8i","R*5h","4i5h","N*5f"]),
    (("Start", "Yokofudori_Equilibrium_DevPly14_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","2b6f","8h6f","8b8f","2d2a+","3b2a","N*4c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly14_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","2b7g+","8i7g","8b8f","2d2a+","3b2a","N*4c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly13_2d1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","2d1d","2b8h+","1d1c+","1a1c","8g8f","8h7h","6i7h","1c1g+"]),
    (("Start", "Yokofudori_Equilibrium_DevPly13_2d2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","2d2c+","3b2c","8h2b+","8b2b","8g8f","B*5h","4i5h","R*4i"]),
    (("Start", "Yokofudori_Equilibrium_DevPly12_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","2b6f","8h6f","P*2h","2d2a+","3b2a","N*4c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly12_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","2b7g+","8i7g","P*2h","2d2a+","3b2a","N*4c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly11_2h2e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2e","2b8h+","2e2d","8h7h","6i7h","S*5h","4i5h","B*4i"]),
    (("Start", "Yokofudori_Equilibrium_DevPly11_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","8h4d","2b4d","2h2d","4d9i+","2d2a+","3b2a","N*4c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly10_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2b6f","8h6f","2c2d","6f1a+","8b5b","1a2a","3b2a","N*4c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly10_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2b7g+","8i7g","2c2d","2h2d","P*2h","2d2a+","3b2a","N*4c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly09_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7h7g","L*4h"]),
    (("Start", "Yokofudori_Equilibrium_DevPly09_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h3c+","2a3c","2h5h","3c2e","8i7g","2b7g+","7h7g","2e3g+"]),
    (("Start", "Yokofudori_Equilibrium_DevPly08_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2b6f","8h6f","8b5b","6f1a+","2a3c","1a3c","3a4b","N*4c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly08_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2b7g+","8i7g","8b5b","7g8e","2a3c","8h3c+","3a4b","N*4c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly07_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7i6h","7g6h"]),
    (("Start", "Yokofudori_Equilibrium_DevPly07_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h3c+","2a3c","2h5h","3c2e","8i7g","2b7g+","7i6h","7g6h"]),
    (("Start", "Yokofudori_Equilibrium_DevPly06_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2b6f","8h6f","8b5b","6f1a+","2a3c","1a3c","3a4b","N*4c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly06_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2b7g+","8i7g","8b5b","B*4b","3a4b","7g6e","B*5h","2h5h"]),
    (("Start", "Yokofudori_Equilibrium_DevPly05_8h4d"), ["7g7f","3c3d","2g2f","8c8d","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7i6h","7g6h"]),
    (("Start", "Yokofudori_Equilibrium_DevPly05_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","8h3c+","2a3c","2h5h","B*4h","3i4h","3c4e","B*5b","8b5b"]),
    (("Start", "Yokofudori_Equilibrium_DevPly04_2b6f"), ["7g7f","3c3d","2g2f","2b6f","8h6f","8b5b","6f1a+","2a3c","1a3c","3a4b","N*4c"]),
    (("Start", "Yokofudori_Equilibrium_DevPly04_2b7g+"), ["7g7f","3c3d","2g2f","2b7g+","8i7g","8b5b","B*4b","3a4b","7g6e","B*5h","2h5h"]),
    (("Start", "Yokofudori_Equilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7i6h","7g6h"]),
    (("Start", "Yokofudori_Equilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2a3c","2h5h","B*4h","3i4h","3c4e","B*5b","8b5b"]),
    (("Start", "Yokofudori_Equilibrium"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f2f","B*3c","2f2e","3c5a+"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly28_8f6d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","8b8f","7g8f","8f6d","6c6d","S*4a","5a4a","R*3a","4a3a","P*7b","6a7b"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly28_8f5c+"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","8b8f","7g8f","8f5c+","6b5c","S*4a","5a4a","R*3a","4a3a","P*7b","6a7b"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly27_8f9f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","8b8f","8f9f","9g9f","B*1h","2h1h","R*1g","2i1g","1d1e","7g3c+"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly27_8f6f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","8b8f","8f6f","7g6f","B*1h","2h1h","R*1g","2i1g","1d1e","6f3c+"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly26_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","7g4d","8b8f","8g8f","4c4d","S*4a","5a4a","R*3a","4a3a"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly26_7g3c+"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","7g3c+","2a3c","8f8b+","B*1h","2h1h","B*1g","2i1g","3c2e"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly25_B*1e"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","B*1e","1f1e","8f8g+","7g3c+","2a3c","7h8g","8b8g+","7f7c+"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly25_B*1g"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","B*1g","1i1g","8f8g+","7g3c+","2a3c","7h8g","8b8g+","7f7c+"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly24_7f8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","7f8f","8e8f","7g3c+","2a3c","8g8f","8b8f","S*4a","8f8i+"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly24_7f7d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","7f7d","7c7d","7g3c+","2a3c","S*4a","5a4a","S*3a","4a3a"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly23_B*1e"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","B*1e","1f1e","1d1e","7g3c+","2a3c","7f7c+","8a7c","1i1e"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly23_B*1g"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","B*1g","1i1g","3c2d","7g1a+","2d2e","1a2a","2e3f","N*4c"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly22_7f8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","7f8f","8e8f","7g3c+","2a3c","8g8f","8b8f","S*4a","4b4a"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly22_7f7d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","7f7d","7c7d","7g3c+","2a3c","S*4a","4b4a","S*3a","4a3a"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly21_B*1e"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","B*1e","1f1e","1d1e","7g3c+","2a3c","1i1e","1a1e","S*4a"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly21_B*1g"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","B*1g","1i1g","4b3a","7g3c+","2a3c","S*2a","3a2a","S*2b"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly20_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","7g8f","8e8f","8g8f","8b8f","S*4a","4b4a","2h3i","8f8i+"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly20_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","7g4d","4c4d","S*4a","4b4a","2h3i","S*3h","4i3h","B*4i"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly19_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","2a3c","7g3c+","3b3c","S*3b","4b3b","N*1c","1a1c","2h3i"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly19_B*1e"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","B*1e","1f1e","1d1e","7g2b+","3b2b","1i1e","1a1e","S*4a"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly18_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","7g8f","8e8f","8g8f","8b8f","S*4a","4b4a","P*8b","8f8i+"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly18_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","7g4d","4c4d","S*4a","4b4a","2h3i","S*3h","4i3h","B*4i"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly17_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","2a3c","7g3c+","3b3c","S*3b","4b3b","N*2d","3c2d","2h3i"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly17_B*1f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","B*1f","1g1f","4b3a","7g2b+","3b2b","S*3b","2b3b","S*4a"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly16_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7g8f","8e8f","8g8f","8b8f","S*4a","4b4a","P*8b","8f8i+"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly16_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7g4d","4c4d","S*4a","4b4a","2h3i","S*3h","4i3h","B*4i"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly15_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","2a3c","7g3c+","2b3c","S*4a","4b4a","N*4d","4c4d","3h3i"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly15_B*1f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","B*1f","1g1f","4b3a","7g2b+","3b2b","S*3b","8b3b","S*4a"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly14_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","7g8f","8e8f","8g8f","2b9i+","S*4a","4b4a","3h3i","9i8i"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly14_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","7g4d","2b4d","S*4a","4b4a","3h3i","4d9i+","4g4f","9i8i"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly13_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","2a3c","7g3c+","2b3c","S*4a","4b4a","N*4d","3c4d","4h3i"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly13_2b6f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","2b6f","7g6f","4b4a","6f1a+","8b4b","1a2a","4a5a","N*4c"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly12_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","7g8f","8e8f","8g8f","2b9i+","S*4a","5a4a","4h3i","9i8i"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly12_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","7g4d","2b4d","S*4a","5a4a","4h3i","4d9i+","4g4f","9i8i"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly11_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","2a3c","7g3c+","2b3c","N*4c"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly11_2b6f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","2b6f","7g6f","8b5b","6f1a+","2a3c","1a3c","3b3c","N*4c"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly10_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","7g8f","8e8f","8g8f","2b9i+","8i7g","9i7g","7h7g","8b8f"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly10_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","7g4d","2b4d","8i7g","4d7g+","7h7g","B*5h","4i5h","N*5f"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly09_2b6f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","2b6f","7g6f","8b5b","6f1a+","2a3c","1a3c","3a4b","N*4c"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly09_2b7g+"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","2b7g+","8i7g","B*5h","4i5h","8b5b","7g8e","2a3c","8e7c+"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly08_2b6f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","2b6f","7g6f","8b5b","6f1a+","2a3c","1a3c","3a4b","N*4c"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly08_2b7g+"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","2b7g+","8i7g","B*5h","4i5h","8b5b","7g8e","2a3c","8e7c+"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly07_8h4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h4d","2b4d","8i7g","4d7g+","7h7g","B*5h","4i5h","N*5f"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly07_8h3c+"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h3c+","2a3c","8i7g","B*5h","4i5h","3c4e","7g8e","2b9i+"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly06_2b6f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","2b6f","8h6f","8b5b","6f1a+","2a3c","1a3c","3a4b","N*4c"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly06_2b7g+"), ["7g7f","3c3d","7i6h","8c8d","2h7h","2b7g+","8i7g","8b5b","B*4b","3a4b","7g6e","B*5h","6i5h"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly05_8h4d"), ["7g7f","3c3d","7i6h","8c8d","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","6h7g","L*4h"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly05_8h3c+"), ["7g7f","3c3d","7i6h","8c8d","8h3c+","2a3c","2h5h","B*4h","3i4h","3c4e","B*5b","8b5b"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly04_2b6f"), ["7g7f","3c3d","7i6h","2b6f","8h6f","8b5b","6f1a+","2a3c","1a3c","3a4b","N*4c"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly04_2b7g+"), ["7g7f","3c3d","7i6h","2b7g+","8i7g","8b5b","B*4b","3a4b","7g6e","B*5h","2h5h"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","2b4d","2h5h","4d9i+","8i7g","9i7g","7i6h","7g6h"]),
    (("Start", "Sankenbisha_Equilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2a3c","2h5h","B*4h","3i4h","3c4e","B*5b","8b5b"]),
    (("Start", "Sankenbisha_Equilibrium"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","8b8f","7g8f"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly32_8h7i"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","3g3f","8d7c","S*7h","8h7i","7h7i+","6i7i","8g8h+","7i8h","8b8h+","S*4a","3a4a"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly32_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","3g3f","8d7c","S*7h","8h4d","4c4d","6i7h","8g8h+","7h8h","8b8h+","S*4a","3a4a"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly31_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","3g3f","8d7c","2a3c","8h3c+","3b3c","S*2a","3a2a","R*3a","2b3a","N*2d"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly31_B*1f"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","3g3f","8d7c","B*1f","1g1f","8g8h+","S*4a","3a4a","B*3a","4a3a","R*4a"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly30_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","3g3f","8h4d","4c4d","S*4a","3a4a","R*3a","4a3a","2h3i","8g8h+"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly30_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","3g3f","8h3c+","2a3c","S*2a","3a2a","R*3a","3b3a","2h1h","8g8h+"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly29_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","2a3c","8h3c+","3b3c","S*2a","3a2a","R*3a","2a3a","N*2d"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly29_8d7e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","8d7e","8h2b+","3b2b","7f7e","8g8h+","S*3b","8b3b","S*4a"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly28_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8h4d","4c4d","S*4a","3a4a","R*3a","4a3a","2h3i","8g8h+"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly28_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8h3c+","2a3c","S*2a","3a2a","R*3a","3b3a","2h1h","8g8h+"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly27_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","2a3c","8h3c+","3b3c","S*2a","3a2a","R*3a","2a3a","N*2d"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly27_8d7e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","8d7e","8h2b+","3b2b","7f7e","8f8i+","S*3b","2b3b","S*4a"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly26_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8h4d","4c4d","S*4a","3a4a","R*3a","4a3a","2h3i","S*3h"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly26_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8h3c+","2a3c","S*2a","3a2a","R*3a","3b3a","2h3i","3c2e"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly25_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","2a3c","8h3c+","3b3c","8g8f","B*1h","2h1h","B*2h","1h2h"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly25_8d7e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8d7e","8h2b+","3b2b","7f7e","8f8g+","S*3b","8b3b","S*4a"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly24_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8h4d","4c4d","S*4a","3a4a","R*3a","4a3a","2h3i","S*3h"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly24_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8h3c+","2a3c","S*2a","3a2a","R*3a","3b3a","2h1h","3c2e"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly23_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2a3c","8h3c+","3b3c","S*2a","3a2a","R*3a","2b3a","N*2d"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly23_8d7e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","8d7e","8h2b+","3b2b","7f7e","B*1h","2h1h","B*2h","1h2h"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly22_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","8h4d","4c4d","S*4a","3a4a","R*3a","4a3a","2h3i","S*3h"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly22_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","8h3c+","2a3c","S*2a","3a2a","R*3a","3b3a","2h1h","S*2h"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly21_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2a3c","8h3c+","3b3c","S*3b","8b3b","R*2a","3a2a","N*2d"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly21_B*1f"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","B*1f","1g1f","P*7h","8h2b+","3b2b","6i7h","B*1h","2h1h"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly20_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","8h4d","4c4d","S*4a","3a4a","R*3a","4a3a","2h3i","S*3h"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly20_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","8h3c+","2a3c","S*2a","3a2a","R*3a","3b3a","2h1h","S*2h"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly19_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","2a3c","8h3c+","2b3c","S*2a","3a2a","N*2d","2c2d","3h3i"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly19_B*1f"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","B*1f","1g1f","3d3e","8h2b+","3b2b","S*3b","2b3b","S*4a"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly18_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","8h4d","4c4d","S*4a","3a4a","3h3i","S*3h","2h3h","B*4i"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly18_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","8h3c+","2a3c","S*2a","3a2a","3h4i","S*3i","4i3i","B*3h"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly17_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","2a3c","8h3c+","2b3c","S*2a","3a2a","N*2d","2c2d","4h3i"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly17_B*1f"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","B*1f","1g1f","8b4b","8h2b+","3b2b","S*3b","2b3b","S*4a"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly16_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","8h4d","4c4d","S*4a","4b4a","4h3i","S*3h","2h3h","B*4i"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly16_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","8h3c+","2a3c","S*4a","4b4a","4h4i","S*3i","4i3i","B*3h"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly15_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","2a3c","8h3c+","2b3c","N*4d","3c4d","4g4f","B*4i","5i4i"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly15_B*1f"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","B*1f","1g1f","4b3a","8h2b+","3b2b","S*3b","2b3b","B*4a"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly14_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","8h4d","2b4d","4g4f","4d9i+","4f4e","9i8i","4e4d","4c4d"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly14_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","8h3c+","2a3c","4g4f","B*4i","5i4i","4b4a","B*4b","8b4b"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly13_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","2a3c","8h3c+","2b3c","N*4d","3c4d","4g4f","4d9i+","4f4e"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly13_2b5e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","2b5e","8h5e","4b4a","5e1a+","8b5b","1a2a","4a5a","N*4c"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly12_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","8h4d","2b4d","5f5e","4d1g+","1i1g","B*4i","5i4i","5a4a"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly12_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","8h3c+","2a3c","5f5e","B*4i","5i4i","3c4e","B*4a","5a4a"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly11_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","2a3c","8h3c+","2b3c","N*4c"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly11_2b5e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","2b5e","8h5e","5a4a","5e1a+","4a5a","1a2a","8b5b","N*4c"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly10_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","8h4d","2b4d","5f5e","4d1g+","1i1g","B*5h","2h5h","8b5b"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly10_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","8h3c+","2a3c","5f5e","B*5h","2h5h","3c4e","B*5b","8b5b"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly09_2b5e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","2b5e","8h5e","8b5b","5e1a+","2a3c","1a3c","3a4b","N*4c"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly09_2b6f"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","2b6f","8h6f","8b5b","6f1a+","2a3c","1a3c","3a4b","N*4c"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly08_2b5e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","2b5e","8h5e","8b5b","5e1a+","2a3c","1a3c","3a4b","N*4c"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly08_2b6f"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","2b6f","8h6f","8b5b","6f1a+","2a3c","1a3c","3a4b","N*4c"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly06_2b5e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","2b5e","5f5e","8b5b","B*4b","3a4b","5e5d","5c5d","2h5h"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly06_2b6f"), ["5g5f","3c3d","5h5e","8c8d","5e5f","2b6f","6g6f","8b5b","B*4b","3a4b","5f5e","B*5h","2h5h"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly05_2b5e"), ["5g5f","3c3d","5h5e","8c8d","2b5e","5f5e","8b5b","B*4b","3a4b","5e5d","5c5d","2h5h"]),
    (("Start", "GokigenNakabisha_Equilibrium_DevPly05_2b6f"), ["5g5f","3c3d","5h5e","8c8d","2b6f","6g6f","8b5b","B*4b","3a4b","5f5e","B*5h","2h5h"]),
    (("Start", "GokigenNakabisha_Equilibrium"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","3g3f","8d7c","S*7h"])
  ]
