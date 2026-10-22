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
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly03_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly04_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly04_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly05_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly05_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly06_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly06_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly07_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly07_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly08_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly08_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly09_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly09_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly10_2b3a (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly10_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly11_2h2e (Type: Saddle, Potential: 50.0)
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
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly23_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly24_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly24_2b7g+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly25_2h2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly25_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly27_2h2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly27_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly29_6g6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly29_7h7g (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly31_2f2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly31_2f6f (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly33_2e3e (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly33_2e5e (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly35_2f2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Aigakari_StratifiedEquilibrium_DevPly35_2f6f (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly04_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly04_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly06_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly06_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly08_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly08_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly09_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly09_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly10_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly10_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly11_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly11_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly12_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly12_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly13_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly13_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly14_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly14_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly15_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly15_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly16_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly16_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly17_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly17_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly18_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly18_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly19_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly19_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly20_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly20_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly21_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly21_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly22_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly22_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly23_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly23_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly24_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly24_8d7e (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly25_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly25_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly26_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly26_8d7e (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly27_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly27_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly28_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly28_8d7e (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly29_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly29_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly30_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly30_8d7e (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly31_8h3c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly31_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly32_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: GokigenNakabisha_StratifiedEquilibrium_DevPly32_8b8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly03_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly04_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly04_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly05_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly05_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly06_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly06_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly07_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly07_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly08_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly08_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly09_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly09_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly10_3b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly10_5a4b (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly11_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly11_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly12_2b3a (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly12_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly13_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly13_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly14_2b3a (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly14_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly15_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly15_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly16_5a4b (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly16_5a5b (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly17_P*3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly17_P*3e (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly18_5a4a (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly18_5a5b (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly19_S*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly19_S*1d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly20_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly20_B*1h (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly21_S*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly21_S*1d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly22_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly22_B*1h (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly23_S*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly23_S*1d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly24_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly24_B*1h (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly25_3e2d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly25_3e4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly26_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly26_B*1h (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly27_2h2e (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly27_3e4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly28_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly28_B*1h (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly29_2d1d (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly29_2d2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly30_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly30_B*1h (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly31_2h2c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly31_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly32_S*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly32_S*1h (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly33_2h2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly33_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly34_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly34_B*1h (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly35_2h2c+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly35_2h2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly36_4b5a (Type: Saddle, Potential: 0.0)
-- | Node ID: Kakugawari_StratifiedEquilibrium_DevPly36_B*1h (Type: Saddle, Potential: 50.0)
-- | Node ID: QEGTB_AtamaKin (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_CrossRook_Encircle (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_Discovered_Mate (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_Dragon_SideDive (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_EscapeRoute_Block (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_Focal_Sacrifice (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_HaraKin (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_HasamiUchi_Pincer (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_HeadSilver_BackGold (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_Horse_Dive (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_IkkenRyu (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_KnightFork_DTM1 (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_KnightFork_PullIn (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_Lance_Skewer (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_Miyako_Center (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_Okuri (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_Pincer_Brinkmate (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_RookHead (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_Ryu_SokoFu (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_Secchin_Corner (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_ShiriKin (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_Silver_Diagonal (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_Staggered_Dragons (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_Tatataki_Pawn (Type: Minimum, Potential: 9999.0)
-- | Node ID: QEGTB_TsurushiKei (Type: Minimum, Potential: 9999.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly03_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly04_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly04_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly05_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly05_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly06_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly06_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly07_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly07_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly08_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly08_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly09_7g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly09_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly10_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly10_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly11_7g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly11_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly12_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly12_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly13_7g4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly13_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly14_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly14_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly15_7g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly15_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly16_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly16_B*1f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly17_7g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly17_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly18_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly18_B*1e (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly19_7g4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly19_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly20_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly20_B*1e (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly21_7g4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly21_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly22_B*1e (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly22_B*1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly23_7f7d (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly23_7f8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly24_5a4a (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly24_5a4b (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly25_7f7c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly25_7f7d (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly26_5a4a (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly26_5a4b (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly27_7g3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly27_7g4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly28_5a4a (Type: Saddle, Potential: 50.0)
-- | Node ID: Sankenbisha_StratifiedEquilibrium_DevPly28_5a4b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly03_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly04_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly04_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly06_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly06_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly08_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly08_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly09_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly10_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly10_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly11_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly12_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly12_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly13_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly14_5a4a (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly14_5a5b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly15_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly15_G*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly17_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly17_S*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly18_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly18_B*1h (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly19_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly19_S*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly20_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly20_B*2f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly21_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly21_S*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly22_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly22_B*2f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly23_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly23_S*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly24_B*1f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly24_B*1h (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly25_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly25_S*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly26_B*1e (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly26_B*1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly27_7g8f (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly27_S*1b (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly28_B*1e (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly28_B*1g (Type: Saddle, Potential: 0.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly29_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly29_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly30_B*1e (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly30_B*1g (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly31_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly31_S*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly32_3c2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly32_B*1e (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly33_6i5i (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly33_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly34_3c2d (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly34_4i3i+ (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly35_7g5i (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly35_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly36_6i5i (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly36_6i7i (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly37_7g5i (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly37_7g8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly38_2a3c (Type: Saddle, Potential: 50.0)
-- | Node ID: Shikenbisha_StratifiedEquilibrium_DevPly38_2d1e (Type: Saddle, Potential: 50.0)
-- | Node ID: Start (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly03_8h4d (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly06_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly06_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly08_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly08_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly10_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly10_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly12_2b5e (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly12_2b6f (Type: Saddle, Potential: 0.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly14_2b5e (Type: Saddle, Potential: 0.0)
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
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly28_8b8e (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly30_8f7f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly30_8f9f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly31_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly31_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly32_8b8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly33_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly33_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly34_8b8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly35_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly35_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly36_8b8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly37_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly37_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly38_8b8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly39_B*1b (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly39_B*1d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yagura_StratifiedEquilibrium_DevPly40_8b8f (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly03_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly03_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly04_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly04_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly05_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly05_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly06_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly06_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly07_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly07_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly08_2a3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly08_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly09_8h3c+ (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly09_8h4d (Type: Saddle, Potential: 50.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly10_2b3a (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly10_2b3c (Type: Saddle, Potential: 0.0)
-- | Node ID: Yokofudori_StratifiedEquilibrium_DevPly11_2h2e (Type: Saddle, Potential: 50.0)
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
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly10_2b3a" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly10_2b3c" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly09_8h4d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly09_8h3c+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly08_2a3c" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly08_2b3c" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly07_8h4d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly06_2a3c" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly06_2b3c" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly05_8h4d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly04_2a3c" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly04_2b3c" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "Aigakari_StratifiedEquilibrium" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly36_4b5a" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly36_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly35_2h2d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly35_2h2c+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly34_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly34_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly33_2h2d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly33_2h2c+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly32_S*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly32_S*1h" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly31_2h2d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly31_2h2c+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly30_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly30_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly29_2d1d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly29_2d2c+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly28_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly28_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly27_2h2e" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly27_3e4d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly26_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly26_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly25_3e2d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly25_3e4d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly24_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly24_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly23_S*1b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly23_S*1d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly22_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly22_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly21_S*1b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly21_S*1d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly20_B*1f" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly20_B*1h" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly19_S*1b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly19_S*1d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly18_5a4a" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly18_5a5b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly17_P*3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly17_P*3e" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly16_5a4b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly16_5a5b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly15_B*1b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly15_B*1d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly14_2b3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly14_2b3a" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly13_B*1b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly13_B*1d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly12_2b3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly12_2b3a" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly11_B*1b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly11_B*1d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly10_3b3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly10_5a4b" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly09_8h4d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly09_8h3c+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly08_2a3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly08_2b3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly07_8h4d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly06_2a3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly06_2b3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly05_8h4d" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly04_2a3c" = 'True
  ValidTransition "Start" "Kakugawari_StratifiedEquilibrium_DevPly04_2b3c" = 'True
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
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly10_2a3c" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly10_2b3c" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly08_2a3c" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly08_2b3c" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly06_2a3c" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly06_2b3c" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "Yagura_StratifiedEquilibrium" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly38_2a3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly38_2d1e" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly37_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly37_7g5i" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly36_6i5i" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly36_6i7i" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly35_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly35_7g5i" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly34_3c2d" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly34_4i3i+" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly33_6i5i" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly33_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly32_3c2d" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly32_B*1e" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly31_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly31_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly30_B*1e" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly30_B*1g" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly29_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly29_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly28_B*1e" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly28_B*1g" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly27_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly27_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly26_B*1e" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly26_B*1g" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly25_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly25_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly24_B*1f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly24_B*1h" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly23_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly23_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly22_B*1f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly22_B*2f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly21_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly21_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly20_B*1f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly20_B*2f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly19_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly19_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly18_B*1f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly18_B*1h" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly17_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly17_S*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly15_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly15_G*1b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly14_5a4a" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly14_5a5b" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly13_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly12_2a3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly12_2b3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly11_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly10_2a3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly10_2b3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly09_7g8f" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly08_2a3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly08_2b3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly06_2a3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly06_2b3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly04_2a3c" = 'True
  ValidTransition "Start" "Shikenbisha_StratifiedEquilibrium_DevPly04_2b3c" = 'True
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
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly10_2b3a" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly10_2b3c" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly09_8h4d" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly09_8h3c+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly08_2a3c" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly08_2b3c" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly07_8h4d" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly06_2a3c" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly06_2b3c" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly05_8h4d" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly04_2a3c" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly04_2b3c" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "Yokofudori_StratifiedEquilibrium" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly28_5a4a" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly28_5a4b" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly27_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly27_7g3c+" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly26_5a4a" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly26_5a4b" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly25_7f7d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly25_7f7c+" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly24_5a4a" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly24_5a4b" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly23_7f8f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly23_7f7d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly22_B*1e" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly22_B*1g" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly21_7g8f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly21_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly20_2a3c" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly20_B*1e" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly19_7g8f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly19_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly18_2a3c" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly18_B*1e" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly17_7g8f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly17_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly16_2a3c" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly16_B*1f" = 'True
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
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly10_2a3c" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly10_2b3c" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly09_7g8f" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly09_7g4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly08_2a3c" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly08_2b3c" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly07_8h4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly07_8h3c+" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly06_2a3c" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly06_2b3c" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly05_8h4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly05_8h3c+" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly04_2a3c" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly04_2b3c" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly03_8h4d" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium_DevPly03_8h3c+" = 'True
  ValidTransition "Start" "Sankenbisha_StratifiedEquilibrium" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly32_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly32_8b8f" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly31_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly31_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly30_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly30_8d7e" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly29_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly29_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly28_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly28_8d7e" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly27_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly27_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly26_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly26_8d7e" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly25_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly25_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly24_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly24_8d7e" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly23_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly23_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly22_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly22_B*1f" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly21_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly21_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly20_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly20_B*1f" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly19_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly19_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly18_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly18_B*1f" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly17_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly17_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly16_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly16_B*1f" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly15_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly15_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly14_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly14_2b5e" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly13_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly13_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly12_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly12_2b5e" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly11_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly11_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly10_2b5e" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly10_2b6f" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly09_8h4d" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly09_8h3c+" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly08_2b5e" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly08_2b6f" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly06_2b5e" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly06_2b6f" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly04_2a3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium_DevPly04_2b3c" = 'True
  ValidTransition "Start" "GokigenNakabisha_StratifiedEquilibrium" = 'True
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
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly35_2f6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2e","7d8d","2e2f","8d7d","2f6f","7d7f","6f7f","P*6f","7f7c+","8a7c","8h6f","3c4b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly35_2f2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2e","7d8d","2e2f","8d7d","2f2d","3c2d","8h2b+","7d7f","7h8h","7f7h+","6i7h","R*9h"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly33_2e3e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2e","7d8d","2e3e","3c4b","8h2b+","3d3e","2b3b","4b3b","P*2b","8d8g+"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly33_2e5e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2e","7d8d","2e5e","8d8g+","7h8g","P*8f","8g8f","P*8e","5e8e","P*6f"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly31_2f6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f6f","7d7f","6f7f","P*6f","8h6f","3c4b","6f2b+","P*6h"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly31_2f2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2d","3c2d","S*2e","2d2e","8h2b+","7d7f","7h8h","7f7h+"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly29_6g6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","6g6f","8d8g+","7h8g","P*8f","8g8f","P*8e","8f8e","P*8d"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly29_7h7g"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","7h7g","8d8g","7g8g","P*6f","8h6f","3c4b","R*5b","4a5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly27_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","2h2d","3c2d","8h2b+","R*6h","6i6h","8b8g+","7h8g","P*8f"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly27_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","2h2c+","3c2c","8h2b+","2c2b","P*2c","3b2c","S*3a","2b3a"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly25_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","2h2d","3c2d","8h2b+","8b8g+","7h8g","R*7h","6h7h","P*8h"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly25_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","2h2c+","3c2c","8h2b+","2c2b","P*2c","3b2c","B*3a","2b3a"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly24_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","2b6f","8h6f","4b5a","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly24_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","2b7g+","8i7g","4b5a","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly23_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","2h2d","2b7g+","8i7g","2c2d","B*3a","4b3a","P*2b","R*5h"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly23_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","2h2c+","3b2c","8h2b+","R*5h","6i5h","4b5a","P*5b"]),
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
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly17_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","2h2d","8f8h+","P*5b"]),
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
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly11_2h2e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2e","2d2e","8h3c","2a3c","8i7g","B*5h","4i5h","R*4i"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly11_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","8h4d","4c4d","2h2d","B*5h","P*5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly10_2b3a"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2b3a","2b5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly10_2b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2b3c","2b5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly09_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h4d","4c4d","2h5h","B*4h","3i4h","8b5b","B*4b","4a4b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly09_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h3c+","2a3c","2h5h","3c2e","8i7g","2b7g+","2b5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly08_2a3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2a3c","8h3c","2b3c","N*4c"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly08_2b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2b3c","2b5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly07_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h4d","4c4d","2h5h","B*4h","3i4h","8b5b","B*4b","3a4b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly07_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h3c+","2b3c","2b5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly06_2a3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2a3c","8h3c","2b3c","N*4c"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly06_2b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2b3c","2b5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly05_8h4d"), ["7g7f","3c3d","2g2f","8c8d","8h4d","4c4d","2h5h","B*4h","3i4h","8b5b","B*4b","3a4b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly05_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","8h3c+","2b3c","2b5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly04_2a3c"), ["7g7f","3c3d","2g2f","2a3c","8h3c","2b3c","N*4c"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly04_2b3c"), ["7g7f","3c3d","2g2f","2b3c","2b5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","4c4d","2h5h","B*4h","3i4h","8b5b","B*4b","3a4b"]),
    (("Start", "Aigakari_StratifiedEquilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2b5b"]),
    (("Start", "Aigakari_StratifiedEquilibrium"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","P*2c","2d2h","8e8f","8g8f","8b8f","P*8g","8f8b","3i3h","7a7b","5i6h","5a4b","4i5h","4b3c","6h7h","7b6c","3h2g","8b8d","2h2f","8d7d","2f2e","7d8d","2e2f","8d7d","8h3c+"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly36_4b5a"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","3b3c","B*1f","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly36_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","3b3c","B*1f","B*1h","1i1h","4b4a","2h2c+","2b2c","1f4c+","3c4c","B*4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly35_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","3b3c","2h2d","2c2d","P*3b","4b3b","B*4b","3a4b","6h7i","B*6i"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly35_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","3b3c","2h2c+","2b2c","P*3b","4b3b","P*2b","3b2b","B*1b","2b1b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly34_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","B*1f","B*5a","6a5a","1g1f","2a3c","2h2c+","2b2c","B*4a"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly34_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","B*1h","B*5a","6a5a","1i1h","2a3c","2h2c+","2b2c","B*4a"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly33_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","2h2d","B*5i","6h5i","2c2d","B*4a","4b4a","B*4b","3a4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly33_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","2h2c+","2b2c","B*3c","2a3c","P*2b","3a2b","6h5i","B*6i"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly32_S*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*1f","B*3a","4b3a","1g1f","B*6i","6h6i","B*5i","6i5i"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly32_S*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*1h","B*3a","4b3a","1i1h","B*6i","6h6i","B*5i","6i5i"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly31_2h2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","2h2d","S*5i","6h5i","B*4h","4i4h","2c2d","S*4a","4b4a"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly31_2h2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","2h2c+","2b2c","S*3a","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly30_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","B*1f","S*3a","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly30_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","B*1h","S*3a","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly29_2d1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d1d","B*5i","6h5i","1c1d","S*3a","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly29_2d2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2c+","2b2c","S*3a","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly28_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","B*1f","S*3a","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly28_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","B*1h","S*3a","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly27_2h2e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2e","B*5i","6h5i","2d2e","S*3a","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly27_3e4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","3e4d","B*5i","6h5i","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly26_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","B*1f","S*3a","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly26_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","B*1h","S*3a","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly25_3e2d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","3e2d","B*5i","6h5i","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly25_3e4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","3e4d","B*5i","6h5i","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly24_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","B*1f","S*3a","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly24_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","B*1h","S*3a","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly23_S*1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","S*1b","B*5i","6h5i","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly23_S*1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","S*1d","B*5i","6h5i","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly22_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","B*1f","S*3a","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly22_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","B*1h","S*3a","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly21_S*1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","S*1b","B*5i","6h5i","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly21_S*1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","S*1d","B*5i","6h5i","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly20_B*1f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","B*1f","S*3a","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly20_B*1h"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","B*1h","S*3a","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly19_S*1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","S*1b","B*4h","2h4h","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly19_S*1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","S*1d","B*4h","2h4h","4b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly18_5a4a"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4a","S*4b","3b4b","B*3b","4b3b","P*3c","S*5h","2h5h"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly18_5a5b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a5b","S*4a","5b5a","P*5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly17_P*3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","P*3c","2a3c","B*4b","3b4b","2h5h","3c2e","3g4f","2e1g+"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly17_P*3e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","P*3e","3d3e","B*4b","3b4b","2h5h","B*3i","4i3i","B*4i"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly16_5a4b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","5a4b","B*3a","4b3a","P*3b","4a3b","2e2d","B*4h","4i4h"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly16_5a5b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","5a5b","B*4b","B*4h","2h4h","4a4b","B*4a","5b4a","P*3b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly15_B*1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","B*1b","1a1b","8i7g","B*5h","4i5h","B*4i","B*4b","4a4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly15_B*1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","B*1d","1c1d","8i7g","B*5h","4i5h","B*4i","B*4b","4a4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly14_2b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","2b3c","2b5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly14_2b3a"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","2b3a","2b5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly13_B*1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","B*1b","1a1b","2h5h","B*4h","3i4h","B*3i","B*4b","4a4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly13_B*1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","B*1d","1c1d","2h5h","B*4h","3i4h","B*3i","B*4b","4a4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly12_2b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","2b3c","2b5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly12_2b3a"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","2b3a","2b5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly11_B*1b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","B*1b","1a1b","2h5h","B*4h","3i4h","B*3i","B*4b","8b4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly11_B*1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","B*1d","1c1d","2h5h","B*4h","3i4h","B*3i","B*4b","8b4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly10_3b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b3c","2b5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly10_5a4b"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","5a4b","B*3a","4a3a","2b3a","4b3a","G*4a","3b4a","2e2d"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly09_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h4d","4c4d","2h5h","B*4h","3i4h","8b5b","B*4b","4a4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly09_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h3c+","2a3c","2h5h","3c2e","8i7g","2b7g+","2b5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly08_2a3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2a3c","8h3c","2b3c","N*4c"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly08_2b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2b3c","2b5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly07_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h4d","4c4d","2h5h","B*4h","3i4h","8b5b","B*4b","3a4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly07_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h3c+","2b3c","2b5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly06_2a3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2a3c","8h3c","2b3c","N*4c"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly06_2b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2b3c","2b5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly05_8h4d"), ["7g7f","3c3d","2g2f","8c8d","8h4d","4c4d","2h5h","B*4h","3i4h","8b5b","B*4b","3a4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly05_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","8h3c+","2b3c","2b5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly04_2a3c"), ["7g7f","3c3d","2g2f","2a3c","8h3c","2b3c","N*4c"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly04_2b3c"), ["7g7f","3c3d","2g2f","2b3c","2b5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","4c4d","2h5h","B*4h","3i4h","8b5b","B*4b","3a4b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2b5b"]),
    (("Start", "Kakugawari_StratifiedEquilibrium"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h2b+","3b2b","7i8h","7a6b","3i4h","6b7b","4h3g","4a3b","6i7h","5a4b","5i6h","6c6d","3g4f","7c7d","4f3e","3c3d","2e2d","2c2d","2h2d","P*2c","2d2h","3d3e","S*5h","S*3a","P*3c","3b3c","B*1f","B*5i","4i5i","1a1b","1f4c+","3c4c","2h2c+","2b2c","B*1e","2a3c","1e3c+","4b3c","P*3d","2c3d","N*2e","3d2e"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly40_8b8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","2e2d","4a4b","B*1f","8b8f","8g8f","2c2d","1f3d","3c3d","2h2d","3d2d","P*2c"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly39_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","2e2d","4a4b","B*1b","1a1b","2d2c+","3b2c","2h2c+","3c2c","P*2d","2c2d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly39_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","2e2d","4a4b","B*1d","1c1d","2d2c+","3b2c","2h2c+","3c2c","P*2d","2c2d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly38_8b8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","2e2d","8b8f","8g8f","2c2d","2h2d","3c2d","P*2c","2d2c","B*2d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly37_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","B*1b","1a1b","2e2d","8b8g+","8h8g","2c2d","2h2d","3c2d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly37_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","B*1d","1c1d","2e2d","8b8g+","8h8g","2c2d","2h2d","3c2d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly36_8b8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","8b8f","8g8f","P*8g","8h8g","3c4b","B*4c","3b4c","R*3b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly35_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","B*1b","1a1b","2e2d","8b8g+","8h8g","2c2d","2h2d","3c2d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly35_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","B*1d","1c1d","2e2d","8b8g+","8h8g","2c2d","2h2d","3c2d"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly34_8b8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","8b8f","B*2d","2c2d","8g8f","2d2e","2h2e","P*8g","8h8g"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly33_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","B*1b","8b8g","8h8g","1a1b","R*3b","3c3b","8g9h","P*8h"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly33_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","B*1d","8b8g","8h8g","1c1d","R*3b","3c3b","8g9h","P*8h"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly32_8b8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","8b8f","B*2d","2c2d","8g8f","2d2e","2h2e","P*8g","8h8g"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly31_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","B*1b","8b8g","8h8g","P*8f","7g8f","1a1b","R*4c","3b4c"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly31_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","B*1d","8b8g","8h8g","P*8f","7g8f","1c1d","R*4c","3b4c"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly30_8f7f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f7f","B*2d","2c2d","7g7f","B*7g","8i7g","2d2e","2h2e"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly30_8f9f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f9f","B*2d","2c2d","9g9f","B*7i","8h7i","2d2e","2h2e"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly28_8b8e"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8e","B*2d","2c2d","8f8e","B*7i","8h7i","2d2e","R*2c"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly27_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","B*1b","8f8g","8h7i","1a1b","2e2d","8g8h+","7g8h","8b8h+"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly27_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","B*1d","8f8g","8h7i","1c1d","2e2d","8g8h+","7g8h","8b8h+"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly25_7g8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","7g8f","8e8f","B*2d","2c2d","8g8f","8b8f","8h7i","8f8i"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly25_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","B*1b","1a1b","8h7i","B*6h","6i6h","3c4b","B*3a","4b5a"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly23_7g8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","7g8f","8e8f","B*2d","2c2d","8g8f","8b8f","8h7i","8f8i"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly23_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","B*1b","1a1b","8h7i","B*6h","6i6h","3c4b","B*3a","4b5a"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly21_7g8f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7g8f","8e8f","B*2d","2c2d","8g8f","S*7g","8i7g","B*6g"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly21_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","B*1b","1a1b","7h7i","B*6h","6i6h","3c4b","B*3a","4b3a"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly19_B*1b"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","B*1b","1a1b","8h7i","B*6h","6i6h","3c4b","B*3a","4b3a"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly19_B*1d"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","B*1d","1c1d","8h7i","B*6h","6i6h","3c4b","B*3a","4b3a"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly14_2b5e"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","2b5e","5f5e","4b5a","2b5b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly14_2b6f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","2b6f","7g6f","4b5a","2b5b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly12_2b5e"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","2b5e","5f5e","4b5a","2b5b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly12_2b6f"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","2b6f","7g6f","4b5a","2b5b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly10_2a3c"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","2a3c","5f5e","3c4e","5e5d","4e3g+","2i3g","6b6c","N*4c"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly10_2b3c"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","2b3c","2b5b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly08_2a3c"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","2a3c","2h5h","3c4e","5g5f","4e3g+","2i3g","2b6f","N*4c"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly08_2b3c"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","2b3c","2b5b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly06_2a3c"), ["7g7f","8c8d","6g6f","3c3d","7i6h","2a3c","2h5h","3c4e","8i7g","4e5g+","6h5g","2b6f","N*4c"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly06_2b3c"), ["7g7f","8c8d","6g6f","3c3d","7i6h","2b3c","2b5b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly03_8h4d"), ["7g7f","8c8d","8h4d","3a4b","4d5c+","4b5c","P*5b"]),
    (("Start", "Yagura_StratifiedEquilibrium_DevPly03_8h3c+"), ["7g7f","8c8d","8h3c+","2a3c","P*5b"]),
    (("Start", "Yagura_StratifiedEquilibrium"), ["7g7f","8c8d","6g6f","3c3d","7i6h","6c6d","6h7g","7a6b","5g5f","5a4b","4i5h","3a3b","5i6h","4b3c","6h7h","5c5d","7h8h","4c4d","8h7h","8d8e","7h8h","6b5c","2g2f","7c7d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","4g4f","3b4c","3i4h","4c3b","4h4g","6a5b","2e2d","4a4b","B*1f","2b3c","8h7i"," *1b","2d2c+","3b2c","2h2c+","1b2c","1f3d","2c3d","S*2c","3d2c","P*2d","2c2d","P*2e","2d2e","4g3f","2e1d"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly38_2a3c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","3c2d","6f6e","2a3c","7g3c+","3b3c","G*2a","3a2a","2h1h","6i7h","7i7h"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly38_2d1e"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","3c2d","6f6e","2d1e","1f1e","6i7h","7i7h","1d1e","7g1a+","G*1h","2h1h"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly37_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","3c2d","7g8f","8e8f","8g8f","6i7h","7i7h","G*1h","2h1h","B*1g"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly37_7g5i"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","3c2d","7g5i","6i7h","7i7h","G*1h","2h1h","B*1g","2i1g","R*2h"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly36_6i5i"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","6i5i","7g5i","3c2d","G*4a","3a4a","R*3a","4a3a","2h3i"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly36_6i7i"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","6i7i","7h7i","3c2d","G*4a","3a4a","R*3a","4a3a","2h3i"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly35_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","7g8f","8e8f","8g8f","6i7h","7i7h","G*1h","2h1h","B*1g"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly35_7g5i"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","7g5i","6i7h","7i7h","G*1h","2h1h","B*1g","2i1g","R*2h"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly34_3c2d"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","3c2d","7g8f","8e8f","2e2d","4i6i+","2d2c+","6i7h","7i7h"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly34_4i3i+"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i3i+","2h3i","B*3h","7h3h","3d3e","3f3e","P*3g","2i3g"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly33_6i5i"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","6i5i","4i2i","2h2i","B*2h","2i2h","3c2d","2e2d","2c2d"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly33_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","7g8f","8e8f","S*2b","3b2b","8g8f","4i6i+","G*3b","2b3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly32_3c2d"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","3c2d","S*2b","3b2b","G*4b","3a4b","2e2d","2c2d","S*3b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly32_B*1e"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","B*1e","S*2b","3b2b","7g8f","8e8f","1f1e","8f8g+","1e1d"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly31_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","7g8f","8e8f","S*2b","3b2b","8g8f","S*1h","2h1h","B*1g"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly31_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","S*1b","R*1h","2h1h","B*2g","1h2g","1a1b","G*4a","3a4a"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly30_B*1e"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","B*1e","G*4a","3a4a","S*4b","3b4b","1f1e","1d1e","1i1e"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly30_B*1g"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","B*1g","1i1g","R*1h","2h1h","1d1e","G*4a","3a4a","1f1e"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly29_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","7g8f","8e8f","G*4a","3a4a","S*4b","3b4b","8g8f","S*1h"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly29_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","S*1b","R*1h","2h1h","B*2g","1h2g","1a1b","G*4a","3a4a"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly28_B*1e"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","B*1e","S*3a","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly28_B*1g"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","B*1g","1i1g","R*1h","2h1h","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly27_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","7g8f","8e8f","S*3a","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly27_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","S*1b","R*1h","2h1h","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly26_B*1e"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","B*1e","S*3a","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly26_B*1g"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","B*1g","1i1g","R*1h","2h1h","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly25_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","7g8f","R*1h","2h1h","8e8f","R*4a","4b4a","S*4b","3b4b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly25_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","S*1b","R*1h","2h1h","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly24_B*1f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","B*1f","S*3a","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly24_B*1h"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","B*1h","S*3a","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly23_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","7g8f","R*2h","7h2h","8e8f","R*4a","4b4a","S*4b","3b4b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly23_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","S*1b","R*2h","7h2h","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly22_B*1f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","B*1f","S*3a","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly22_B*2f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","B*2f","S*3a","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly21_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","7g8f","R*2h","6h2h","8e8f","R*4a","4b4a","S*4b","3b4b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly21_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","S*1b","R*2h","6h2h","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly20_B*1f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","B*1f","S*3a","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly20_B*2f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","B*2f","S*3a","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly19_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","7g8f","B*3i","2h3i","8e8f","S*5a","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly19_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","S*1b","B*3i","2h3i","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly18_B*1f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","B*1f","S*3a","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly18_B*1h"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","B*1h","S*3a","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly17_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","7g8f","B*4i","3h4i","8e8f","S*3a","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly17_S*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","S*1b","B*4i","3h4i","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly15_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","7g8f","8e8f","8g8f","B*3i","4h3i","4b5a","G*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly15_G*1b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","G*1b","4b5a","1b2b","3a2b","B*4b","3b4b","4h3i","G*4i"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly14_5a4a"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4a","G*4b","3a4b","4h3i","4a5a","5g5f","G*4i","3h4i"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly14_5a5b"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a5b","G*4b","5b4b","4h4i","4b5a","5g5f","G*3i","4i3i"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly13_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","7g8f","8e8f","8g8f","2b6f","P*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly12_2a3c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","2a3c","5g5f","3c4e","5f5e","4e3g+","2i3g","2b5e","N*4c"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly12_2b3c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","2b3c","2b5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly11_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","7g8f","8e8f","8g8f","2b6f","P*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly10_2a3c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","2a3c","5g5f","3c4e","5f5e","4e3g+","2i3g","2b5e","N*4c"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly10_2b3c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","2b3c","2b5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly09_7g8f"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","7g8f","8e8f","8g8f","2b6f","P*5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly08_2a3c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","2a3c","5g5f","3c4e","5f5e","4e3g+","2i3g","2b5e","N*4c"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly08_2b3c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","2b3c","2b5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly06_2a3c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","2a3c","8i7g","3c4e","7g6e","4e5g+","6e5c+","5g6h","5c5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly06_2b3c"), ["7g7f","3c3d","6g6f","8c8d","2h6h","2b3c","2b5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly04_2a3c"), ["7g7f","3c3d","6g6f","2a3c","2h5h","3c4e","8i7g","4e5g+","5h5g","2b6f","N*4c"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly04_2b3c"), ["7g7f","3c3d","6g6f","2b3c","2b5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","4c4d","2h5h","B*4h","3i4h","8b5b","B*4b","3a4b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2b5b"]),
    (("Start", "Shikenbisha_StratifiedEquilibrium"), ["7g7f","3c3d","6g6f","8c8d","2h6h","8d8e","8h7g","6b6c","4i4h","7a6b","3i3h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7b7a","2h1h","7a8b","6h7h","8b7b","1h2h","1c1d","1g1f","5c5d","2g2f","4b3a","2f2e","2b3c","3g3f","R*4i","S*9h","4i6i+","2e2d","3c2d","6f6e","6i7h","7i7h","G*1g","1i1g","B*1i","2h1i","R*1h","1i1h","1a1b","7g2b+","3a2b","G*1c","1b1c","G*1b","2b1b"]),
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
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly23_3d4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d4d","2b4d","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly22_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2b6f","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly22_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2b7g+","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly21_3f3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3c+","2a3c","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly21_3f3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3c","2a3c","P*5b"]),
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
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly15_2d1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d1d","8f8h+","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly15_2d2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d2c+","3b2c","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly14_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","2b6f","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly14_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","2b7g+","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly13_2d1d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","2d1d","2b8h+","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly13_2d2c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","2d2c+","3b2c","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly12_2b6f"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","2b6f","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly12_2b7g+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","2b7g+","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly11_2h2e"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2e","2d2e","8h3c","2a3c","8i7g","B*5h","4i5h","R*4i"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly11_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","8h4d","4c4d","2h2d","B*5h","P*5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly10_2b3a"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2b3a","2b5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly10_2b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2b3c","2b5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly09_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h4d","4c4d","2h5h","B*4h","3i4h","8b5b","B*4b","4a4b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly09_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","8h3c+","2a3c","2h5h","3c2e","8i7g","2b7g+","2b5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly08_2a3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2a3c","8h3c","2b3c","N*4c"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly08_2b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","2b3c","2b5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly07_8h4d"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h4d","4c4d","2h5h","B*4h","3i4h","8b5b","B*4b","3a4b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly07_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","8h3c+","2b3c","2b5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly06_2a3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2a3c","8h3c","2b3c","N*4c"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly06_2b3c"), ["7g7f","3c3d","2g2f","8c8d","2f2e","2b3c","2b5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly05_8h4d"), ["7g7f","3c3d","2g2f","8c8d","8h4d","4c4d","2h5h","B*4h","3i4h","8b5b","B*4b","3a4b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly05_8h3c+"), ["7g7f","3c3d","2g2f","8c8d","8h3c+","2b3c","2b5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly04_2a3c"), ["7g7f","3c3d","2g2f","2a3c","8h3c","2b3c","N*4c"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly04_2b3c"), ["7g7f","3c3d","2g2f","2b3c","2b5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","4c4d","2h5h","B*4h","3i4h","8b5b","B*4b","3a4b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2b5b"]),
    (("Start", "Yokofudori_StratifiedEquilibrium"), ["7g7f","3c3d","2g2f","8c8d","2f2e","8d8e","7i7h","3a3b","2e2d","2c2d","2h2d","8e8f","8g8f","8b8f","2d3d","8f8h+","7i8h","B*4e","3d3f","4e2g+","3f3d","2g1g","3d3f","1g1f","3f3e","1f2f","B*3c","2f2e","3c5a+"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly28_5a4a"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","8b8f","7g8f","5a4a","S*4b","3b4b","R*3a","4a3a","8f5c+","R*1h","2h1h"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly28_5a4b"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","8b8f","7g8f","5a4b","8f5c","6b5c","R*4a","4b4a","S*4b","5c4b","P*7b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly27_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","8b8f","7g4d","4c4d","P*5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly27_7g3c+"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","8b8f","7g3c+","2a3c","P*5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly26_5a4a"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","5a4a","8f8b+","B*1g","1i1g","4a5a","P*5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly26_5a4b"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","5a4b","7g3c","2a3c","8f8b+","B*1g","1i1g","B*1i","2h1i"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly25_7f7d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f7d","7c7d","P*5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly25_7f7c+"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f7c+","8a7c","P*5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly24_5a4a"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","5a4a","S*4b","3b4b","7g3c+","4b3c","S*4b","4a4b","7f7c+"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly24_5a4b"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","5a4b","7g3c","2a3c","S*3a","4b5a","P*5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly23_7f8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","7f8f","8e8f","P*5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly23_7f7d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","7f7d","7c7d","P*5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly22_B*1e"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","B*1e","7g3c","2a3c","S*3a","4b5a","P*5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly22_B*1g"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","B*1g","1i1g","4b5a","P*5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly21_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7g8f","B*1g","1i1g","8e8f","S*3a","4b5a","2b5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly21_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7g4d","B*1g","1i1g","4c4d","S*4c","4b5a","2b5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly20_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2a3c","7g3c","3b3c","S*3a","4b5a","N*4c"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly20_B*1e"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","B*1e","S*3a","4b5a","7g3c","2a3c","3a4b","3b4b","1f1e"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly19_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","7g8f","B*1g","1i1g","8e8f","S*3a","4b5a","B*4b","3b4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly19_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","7g4d","B*1g","1i1g","4c4d","S*4c","4b5a","4c5b","6a5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly18_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","2a3c","7g3c","3b3c","S*3a","4b5a","N*4c"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly18_B*1e"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","B*1e","S*3a","4b5a","7g3c","2a3c","3a4b","3b4b","1f1e"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly17_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","7g8f","B*3i","2h3i","8e8f","S*5a","4b5a","B*4b","3b4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly17_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","7g4d","B*3i","2h3i","4c4d","S*4c","4b5a","4c5b","6a5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly16_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","2a3c","7g3c","3b3c","S*3a","4b5a","N*4c"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly16_B*1f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","B*1f","S*3a","4b5a","7g3c","2a3c","3a4b","8b4b","1g1f"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly15_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","7g8f","8e8f","S*3a","4b5a","3a4b","8b4b","8g8f","S*2h","P*5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly15_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","7g4d","4c4d","S*4c","4b5a","4c5b","8b5b","3h3i","S*3h"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly14_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","2a3c","7g3c","2b3c","S*5a","4b5a","N*4c"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly14_2b6f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","2b6f","S*3c","4b5a","2b5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly13_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","7g8f","8e8f","S*3c","4b5a","3c4b","8b4b","8g8f","2b9i+","P*5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly13_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","7g4d","2b4d","S*3c","4b5a","2b5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly12_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","2a3c","S*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly12_2b6f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","2b6f","S*4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly11_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","7g8f","8e8f","8g8f","2b9i+","P*5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly11_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","7g4d","4c4d","8i7g","B*5h","4i5h","8b5b","B*4b","3a4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly10_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","2a3c","7g3c","2b3c","N*4c"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly10_2b3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","2b3c","2b5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly09_7g8f"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","7g8f","8e8f","8g8f","2b9i+","P*5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly09_7g4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","7g4d","4c4d","8i7g","B*5h","4i5h","8b5b","B*4b","3a4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly08_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","2a3c","7g3c","2b3c","N*4c"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly08_2b3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","2b3c","2b5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly07_8h4d"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h4d","4c4d","8i7g","B*5h","4i5h","8b5b","B*4b","3a4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly07_8h3c+"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h3c+","2b3c","2b5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly06_2a3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","2a3c","8h3c","2b3c","N*4c"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly06_2b3c"), ["7g7f","3c3d","7i6h","8c8d","2h7h","2b3c","2b5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly05_8h4d"), ["7g7f","3c3d","7i6h","8c8d","8h4d","4c4d","2h5h","B*7i","6i7i","8b5b","B*4b","3a4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly05_8h3c+"), ["7g7f","3c3d","7i6h","8c8d","8h3c+","2b3c","2b5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly04_2a3c"), ["7g7f","3c3d","7i6h","2a3c","8h3c","2b3c","N*4c"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly04_2b3c"), ["7g7f","3c3d","7i6h","2b3c","2b5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly03_8h4d"), ["7g7f","3c3d","8h4d","4c4d","2h5h","B*4h","3i4h","8b5b","B*4b","3a4b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium_DevPly03_8h3c+"), ["7g7f","3c3d","8h3c+","2b3c","2b5b"]),
    (("Start", "Sankenbisha_StratifiedEquilibrium"), ["7g7f","3c3d","7i6h","8c8d","2h7h","8d8e","8h7g","6b6c","3i4h","4a3b","5i4h","5a4b","4h3h","3a2b","3h2h","7a6b","1g1f","1c1d","3g3f","2b3c","7h7f","4b5a","6i7h","8e8f","7f8f","8b8f","7g8f","B*1g","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly32_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","3g3f","8d7c","S*7h","2a3c","R*2a","3a2a","8h3c+","3b3c","N*2d","3c2d","2e2d"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly32_8b8f"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","3g3f","8d7c","S*7h","8b8f","8h2b","3b2b","S*3b","2b3b","8g8f","S*1h","2h1h"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly31_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","3g3f","8d7c","8h4d","B*3g","2i3g","4c4d","R*4a","3a4a","S*3a","4a3a"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly31_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","3g3f","8d7c","8h3c+","2a3c","R*2a","3a2a","S*1b","1a1b","2h3i","8b8g+"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly30_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","3g3f","2a3c","R*2a","3a2a","S*1b","1a1b","8h3c+","3b3c","N*2d"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly30_8d7e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","3g3f","8d7e","8h2b","3b2b","S*3b","8b3b","R*4a","3a4a","7f7e"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly29_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","8h4d","B*3i","2h3i","4c4d","R*4a","3a4a","S*4b","8b4b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly29_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","8h3c+","2a3c","R*4a","3a4a","S*4b","8b4b","2h1h","3c2e"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly28_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","2a3c","R*2a","3a2a","S*1b","1a1b","8g8f","S*2g","2h2g"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly28_8d7e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8d7e","8h2b","3b2b","S*3b","2b3b","R*4a","3a4a","S*4b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly27_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","8h4d","B*3i","2h3i","4c4d","R*4a","3a4a","S*4b","3b4b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly27_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","8h3c+","2a3c","R*4a","3a4a","S*4b","3b4b","P*8b","S*2g"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly26_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","2a3c","R*2a","3a2a","S*1b","1a1b","8h3c+","S*2g","2h2g"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly26_8d7e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8d7e","8h2b","3b2b","S*3b","8b3b","R*4a","3a4a","S*4b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly25_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8h4d","B*3i","2h3i","4c4d","R*4a","3a4a","S*4b","8b4b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly25_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8h3c+","2a3c","R*2a","3a2a","S*1b","1a1b","8g8f","S*2g"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly24_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","2a3c","R*2a","3a2a","S*1b","1a1b","8h3c+","S*2g","2h2g"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly24_8d7e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8d7e","8h2b","3b2b","S*3b","8b3b","R*4a","3a4a","S*4b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly23_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","8h4d","B*3i","2h3i","4c4d","R*4a","3a4a","S*4b","8b4b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly23_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","8h3c+","2a3c","R*2a","3a2a","S*1b","1a1b","2h3i","S*3h"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly22_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","2a3c","R*2a","3a2a","S*1b","1a1b","8h3c+","S*2g","2h2g"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly22_B*1f"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","B*1f","8h2b","3b2b","S*3b","8b3b","R*4a","3a4a","S*4b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly21_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","8h4d","B*3i","2h3i","4c4d","R*4a","3a4a","S*4b","8b4b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly21_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","8h3c+","2a3c","R*2a","3a2a","S*1b","1a1b","2h3i","S*3h"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly20_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","2a3c","R*2a","3a2a","S*1b","1a1b","8h3c+","R*1h","2h1h"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly20_B*1f"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","B*1f","8h2b","3b2b","S*3b","2b3b","R*4a","3a4a","S*4b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly19_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","8h4d","B*4i","3h4i","4c4d","S*4b","3b4b","B*3b","S*4h"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly19_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","8h3c+","2a3c","S*4b","3b4b","3h3i","S*3h","2h3h","B*2h"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly18_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","2a3c","S*4b","8b4b","8h3c+","2b3c","3h3i","S*3h","2h3h"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly18_B*1f"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","B*1f","8h2b","3b2b","S*3b","8b3b","S*4b","3b4b","1g1f"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly17_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","8h4d","B*3i","4h3i","4c4d","S*4b","8b4b","B*4a","S*3h"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly17_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","8h3c+","2a3c","S*4b","8b4b","4h5i","S*4h","5h4h","B*4i"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly16_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","2a3c","8h3c","2b3c","S*3a","4b5a","N*4c"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly16_B*1f"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","B*1f","S*3a","4b5a","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly15_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","8h4d","4c4d","4g4f","4b5a","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly15_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","8h3c+","2a3c","4g4f","4b5a","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly14_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","2a3c","8h3c","2b3c","4g4f","4b5a","N*4c"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly14_2b5e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","2b5e","8h5e","4b5a","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly13_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","8h4d","2b4d","4g4f","B*4h","2h4h","4b5a","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly13_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","8h3c+","2a3c","4g4f","B*4h","2h4h","4b5a","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly12_2a3c"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","2a3c","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly12_2b5e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","2b5e","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly11_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","8h4d","2b4d","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly11_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","8h3c+","2a3c","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly10_2b5e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","2b5e","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly10_2b6f"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","2b6f","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly09_8h4d"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","8h4d","2b4d","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly09_8h3c+"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","8h3c+","2a3c","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly08_2b5e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","2b5e","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly08_2b6f"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","2b6f","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly06_2b5e"), ["5g5f","3c3d","5h5e","8c8d","5e5f","2b5e","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly06_2b6f"), ["5g5f","3c3d","5h5e","8c8d","5e5f","2b6f","P*5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly04_2a3c"), ["5g5f","3c3d","5h5e","2a3c","5e5d","3c4e","5d5c+","2b7g+","5c5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium_DevPly04_2b3c"), ["5g5f","3c3d","5h5e","2b3c","2b5b"]),
    (("Start", "GokigenNakabisha_StratifiedEquilibrium"), ["5g5f","3c3d","5h5e","8c8d","5e5f","8d8e","7g7f","6b6c","7i6h","4a3b","4i5h","5a4b","3i4h","3a2b","5i4h","4b3a","4h3h","7a6b","3h2h","6b7c","2g2f","7c8d","2f2e","8e8f","8g8f","8b8f","P*8g","8f8b","3g3f","8d7c","S*7h","8b8g+","7h8g","B*3g","2h3g","1c1d","8h2b+","3a2b","S*1c","1a1c","B*1a","2b1a","R*1b","1a1b","R*1a","1b1a"])
  ]
