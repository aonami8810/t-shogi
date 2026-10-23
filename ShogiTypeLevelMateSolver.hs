{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module Main where

import Data.Type.Bool (If, Not)
import Data.Proxy

-- ============================================================================
-- 1. 純粋代数的型レベル座標・8方向基本単体 (Direction8)・盤面の定義
--    (GHC.TypeLits の Nat 算術ソルバーを排除し、1ステップ項書換えで正規化)
-- ============================================================================

-- | 手番 (White = 先手 Sente / Black = 後手 Gote)
data Color = White | Black
  deriving (Eq, Show)

-- | 盤面座標 (1..9 の有限代数データ型)
data Coord = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9
  deriving (Eq, Show)

-- | マス座標 (筋, 段)
data Sq = Sq Coord Coord
  deriving (Eq, Show)

-- | 8方向基本単体 (Fundamental 1-Simplices: Δ_N .. Δ_SE)
data Dir8 = DN | DS | DW | DE | DNW | DNE | DSW | DSE
  deriving (Eq, Show)

-- | 双対（反対）方向単体 (Coboundary 反転作用素)
type family OppDir (d :: Dir8) :: Dir8 where
  OppDir 'DN  = 'DS
  OppDir 'DS  = 'DN
  OppDir 'DW  = 'DE
  OppDir 'DE  = 'DW
  OppDir 'DNW = 'DSE
  OppDir 'DNE = 'DSW
  OppDir 'DSW = 'DNE
  OppDir 'DSE = 'DNW

-- | 駒種
data PieceType = King | Gold | Silver | Rook | PRook | Bishop | PBishop | Lance | Knight | Pawn
  deriving (Eq, Show)

-- | 駒 (所有者, 駒種)
data Piece = P Color PieceType
  deriving (Eq, Show)

-- | 指し手 (盤上移動 MoveFromTo / 持ち駒打ち DropAt)
data TMove
  = MoveFromTo Sq Sq Bool
  | DropAt PieceType Sq
  deriving (Eq, Show)

-- | 帰納的ペアノ自然数（探索深さ・手数証明用）
data PNat = Z | S PNat
  deriving (Eq, Show)

type N0 = 'Z
type N1 = 'S N0
type N2 = 'S N1
type N3 = 'S N2
type N4 = 'S N3
type N5 = 'S N4
type N7 = 'S ('S N5)

-- | 型レベル証明木 (1パス短絡探索で直接構築されるカリー＝ハワード証明項の型表現)
data TProofTree
  = TOrNode  TMove TProofTree
  | TAndNode [(TMove, TProofTree)]

-- | ゲーム理論的勝敗値（詰み手数と型レベル証明木の対）
data GameResult
  = SenteWin PNat TProofTree
  | NoMate

-- | 型レベル盤面
type TypeBoard = [(Sq, Piece)]

-- ============================================================================
-- 2. O(1) パターンマッチによる8方向単体幾何演算（TypeLits算術ゼロ）
-- ============================================================================

type family EqColor (a :: Color) (b :: Color) :: Bool where
  EqColor 'White 'White = 'True
  EqColor 'Black 'Black = 'True
  EqColor _      _      = 'False

type family EqPt (a :: PieceType) (b :: PieceType) :: Bool where
  EqPt 'King    'King    = 'True
  EqPt 'Gold    'Gold    = 'True
  EqPt 'Silver  'Silver  = 'True
  EqPt 'Rook    'Rook    = 'True
  EqPt 'PRook   'PRook   = 'True
  EqPt 'Bishop  'Bishop  = 'True
  EqPt 'PBishop 'PBishop = 'True
  EqPt 'Lance   'Lance   = 'True
  EqPt 'Knight  'Knight  = 'True
  EqPt 'Pawn    'Pawn    = 'True
  EqPt _        _        = 'False

type family EqCoord (a :: Coord) (b :: Coord) :: Bool where
  EqCoord 'C1 'C1 = 'True
  EqCoord 'C2 'C2 = 'True
  EqCoord 'C3 'C3 = 'True
  EqCoord 'C4 'C4 = 'True
  EqCoord 'C5 'C5 = 'True
  EqCoord 'C6 'C6 = 'True
  EqCoord 'C7 'C7 = 'True
  EqCoord 'C8 'C8 = 'True
  EqCoord 'C9 'C9 = 'True
  EqCoord _   _   = 'False

type family EqSq (s1 :: Sq) (s2 :: Sq) :: Bool where
  EqSq ('Sq x1 y1) ('Sq x2 y2) = If (EqCoord x1 x2) (EqCoord y1 y2) 'False

type family PredC (c :: Coord) :: Maybe Coord where
  PredC 'C1 = 'Nothing
  PredC 'C2 = 'Just 'C1
  PredC 'C3 = 'Just 'C2
  PredC 'C4 = 'Just 'C3
  PredC 'C5 = 'Just 'C4
  PredC 'C6 = 'Just 'C5
  PredC 'C7 = 'Just 'C6
  PredC 'C8 = 'Just 'C7
  PredC 'C9 = 'Just 'C8

type family SuccC (c :: Coord) :: Maybe Coord where
  SuccC 'C1 = 'Just 'C2
  SuccC 'C2 = 'Just 'C3
  SuccC 'C3 = 'Just 'C4
  SuccC 'C4 = 'Just 'C5
  SuccC 'C5 = 'Just 'C6
  SuccC 'C6 = 'Just 'C7
  SuccC 'C7 = 'Just 'C8
  SuccC 'C8 = 'Just 'C9
  SuccC 'C9 = 'Nothing

-- | 8方向単体境界作用素 ∂_d(Sq): 指定方向へ1ステップ進んだ座標
type family Step1 (d :: Dir8) (sq :: Sq) :: Maybe Sq where
  Step1 'DN  ('Sq x y) = MkSq ('Just x) (PredC y)
  Step1 'DS  ('Sq x y) = MkSq ('Just x) (SuccC y)
  Step1 'DW  ('Sq x y) = MkSq (PredC x) ('Just y)
  Step1 'DE  ('Sq x y) = MkSq (SuccC x) ('Just y)
  Step1 'DNW ('Sq x y) = MkSq (PredC x) (PredC y)
  Step1 'DNE ('Sq x y) = MkSq (SuccC x) (PredC y)
  Step1 'DSW ('Sq x y) = MkSq (PredC x) (SuccC y)
  Step1 'DSE ('Sq x y) = MkSq (SuccC x) (SuccC y)

type family MkSq (mx :: Maybe Coord) (my :: Maybe Coord) :: Maybe Sq where
  MkSq ('Just x) ('Just y) = 'Just ('Sq x y)
  MkSq _         _         = 'Nothing

-- | 2座標の1ステップ相対関係（距離2以上は即座に 'Nothing に短絡評価）
type family RelAxis (a :: Coord) (b :: Coord) :: Maybe Ordering where
  RelAxis 'C1 'C1 = 'Just 'EQ
  RelAxis 'C1 'C2 = 'Just 'LT
  RelAxis 'C2 'C1 = 'Just 'GT
  RelAxis 'C2 'C2 = 'Just 'EQ
  RelAxis 'C2 'C3 = 'Just 'LT
  RelAxis 'C3 'C2 = 'Just 'GT
  RelAxis 'C3 'C3 = 'Just 'EQ
  RelAxis 'C3 'C4 = 'Just 'LT
  RelAxis 'C4 'C3 = 'Just 'GT
  RelAxis 'C4 'C4 = 'Just 'EQ
  RelAxis 'C4 'C5 = 'Just 'LT
  RelAxis 'C5 'C4 = 'Just 'GT
  RelAxis 'C5 'C5 = 'Just 'EQ
  RelAxis 'C5 'C6 = 'Just 'LT
  RelAxis 'C6 'C5 = 'Just 'GT
  RelAxis 'C6 'C6 = 'Just 'EQ
  RelAxis 'C6 'C7 = 'Just 'LT
  RelAxis 'C7 'C6 = 'Just 'GT
  RelAxis 'C7 'C7 = 'Just 'EQ
  RelAxis 'C7 'C8 = 'Just 'LT
  RelAxis 'C8 'C7 = 'Just 'GT
  RelAxis 'C8 'C8 = 'Just 'EQ
  RelAxis 'C8 'C9 = 'Just 'LT
  RelAxis 'C9 'C8 = 'Just 'GT
  RelAxis 'C9 'C9 = 'Just 'EQ
  RelAxis _   _   = 'Nothing

-- | from から to への8方向単体ベクトル（1ステップ隣接でない場合は 'Nothing）
type family RelDir1 (from :: Sq) (to :: Sq) :: Maybe Dir8 where
  RelDir1 ('Sq fx fy) ('Sq tx ty) = CombineRelAxis (RelAxis fx tx) (RelAxis fy ty)

type family CombineRelAxis (mx :: Maybe Ordering) (my :: Maybe Ordering) :: Maybe Dir8 where
  CombineRelAxis ('Just 'EQ) ('Just 'GT) = 'Just 'DN
  CombineRelAxis ('Just 'EQ) ('Just 'LT) = 'Just 'DS
  CombineRelAxis ('Just 'GT) ('Just 'EQ) = 'Just 'DW
  CombineRelAxis ('Just 'LT) ('Just 'EQ) = 'Just 'DE
  CombineRelAxis ('Just 'GT) ('Just 'GT) = 'Just 'DNW
  CombineRelAxis ('Just 'LT) ('Just 'GT) = 'Just 'DNE
  CombineRelAxis ('Just 'GT) ('Just 'LT) = 'Just 'DSW
  CombineRelAxis ('Just 'LT) ('Just 'LT) = 'Just 'DSE
  CombineRelAxis _           _           = 'Nothing

-- | 各駒の8方向1ステップ単体複体テーブル C_1(P)
type family HasUnitSimplex (c :: Color) (pt :: PieceType) (d :: Dir8) :: Bool where
  HasUnitSimplex _      'King   _    = 'True
  HasUnitSimplex 'White 'Pawn   'DN  = 'True
  HasUnitSimplex 'Black 'Pawn   'DS  = 'True
  -- 金将: 斜め後ろ2方向 (WhiteならDSW/DSE, BlackならDNW/DNE) 以外の6単体
  HasUnitSimplex 'White 'Gold   'DSW = 'False
  HasUnitSimplex 'White 'Gold   'DSE = 'False
  HasUnitSimplex 'White 'Gold   _    = 'True
  HasUnitSimplex 'Black 'Gold   'DNW = 'False
  HasUnitSimplex 'Black 'Gold   'DNE = 'False
  HasUnitSimplex 'Black 'Gold   _    = 'True
  -- 銀将: 前方 + 斜め4方向の5単体
  HasUnitSimplex 'White 'Silver 'DN  = 'True
  HasUnitSimplex 'White 'Silver 'DNW = 'True
  HasUnitSimplex 'White 'Silver 'DNE = 'True
  HasUnitSimplex 'White 'Silver 'DSW = 'True
  HasUnitSimplex 'White 'Silver 'DSE = 'True
  HasUnitSimplex 'Black 'Silver 'DS  = 'True
  HasUnitSimplex 'Black 'Silver 'DNW = 'True
  HasUnitSimplex 'Black 'Silver 'DNE = 'True
  HasUnitSimplex 'Black 'Silver 'DSW = 'True
  HasUnitSimplex 'Black 'Silver 'DSE = 'True
  -- 香・飛・竜・角・馬
  HasUnitSimplex 'White 'Lance  'DN  = 'True
  HasUnitSimplex 'Black 'Lance  'DS  = 'True
  HasUnitSimplex _      'Rook   'DN  = 'True
  HasUnitSimplex _      'Rook   'DS  = 'True
  HasUnitSimplex _      'Rook   'DW  = 'True
  HasUnitSimplex _      'Rook   'DE  = 'True
  HasUnitSimplex _      'PRook  _    = 'True
  HasUnitSimplex _      'Bishop 'DNW = 'True
  HasUnitSimplex _      'Bishop 'DNE = 'True
  HasUnitSimplex _      'Bishop 'DSW = 'True
  HasUnitSimplex _      'Bishop 'DSE = 'True
  HasUnitSimplex _      'PBishop _   = 'True
  HasUnitSimplex _      _       _    = 'False

-- | 位置 from の駒 P c pt が to を攻撃しているか（O(1) 単体テーブル照合）
type family CanPieceAttack (from :: Sq) (c :: Color) (pt :: PieceType) (to :: Sq) :: Bool where
  CanPieceAttack from c pt to = MatchUnitDir c pt (RelDir1 from to)

type family MatchUnitDir (c :: Color) (pt :: PieceType) (md :: Maybe Dir8) :: Bool where
  MatchUnitDir _ _  'Nothing  = 'False
  MatchUnitDir c pt ('Just d) = HasUnitSimplex c pt d

-- | 盤面上の指定マスの駒を検索
type family LookupSq (sq :: Sq) (bd :: TypeBoard) :: Maybe Piece where
  LookupSq _  '[] = 'Nothing
  LookupSq sq ('(s, p) ': rest) = If (EqSq sq s) ('Just p) (LookupSq sq rest)

type family IsEmptySq (sq :: Sq) (bd :: TypeBoard) :: Bool where
  IsEmptySq _  '[] = 'True
  IsEmptySq sq ('(s, _) ': rest) = If (EqSq sq s) 'False (IsEmptySq sq rest)

type family HasFriendly (c :: Color) (sq :: Sq) (bd :: TypeBoard) :: Bool where
  HasFriendly _ _  '[] = 'False
  HasFriendly c sq ('(s, 'P c2 _) ': rest) =
    If (EqSq sq s) (EqColor c c2) (HasFriendly c sq rest)

-- | 指定マス target が attacker 側の駒から利きを受けているか
type family IsSqAttackedBy (attacker :: Color) (target :: Sq) (bd :: TypeBoard) :: Bool where
  IsSqAttackedBy _        _      '[] = 'False
  IsSqAttackedBy attacker target ('(sq, 'P c pt) ': rest) =
    If (EqColor c attacker)
       (If (CanPieceAttack sq c pt target)
           'True
           (IsSqAttackedBy attacker target rest))
       (IsSqAttackedBy attacker target rest)

-- ============================================================================
-- 3. 盤面・持ち駒の型レベル状態遷移
-- ============================================================================

type family RemoveSq (sq :: Sq) (bd :: TypeBoard) :: TypeBoard where
  RemoveSq _  '[] = '[]
  RemoveSq sq ('(s, p) ': rest) = If (EqSq sq s) rest ('(s, p) ': RemoveSq sq rest)

type family ApplyBoardMove (c :: Color) (m :: TMove) (bd :: TypeBoard) :: TypeBoard where
  ApplyBoardMove c ('DropAt pt to) bd = '(to, 'P c pt) ': bd
  ApplyBoardMove c ('MoveFromTo from to 'False) bd =
     MovePiece c from to (LookupSq from bd) (RemoveSq to (RemoveSq from bd))
  ApplyBoardMove c ('MoveFromTo from to 'True) bd =
    MovePromotedPiece c from to (LookupSq from bd) (RemoveSq to (RemoveSq from bd))

type family MovePiece (c :: Color) (from :: Sq) (to :: Sq) (mp :: Maybe Piece) (bd :: TypeBoard) :: TypeBoard where
  MovePiece _ _ _  'Nothing         bd = bd
  MovePiece _ _ to ('Just ('P c p)) bd = '(to, 'P c p) ': bd

type family PromoteType (pt :: PieceType) :: PieceType where
  PromoteType 'Rook   = 'PRook
  PromoteType 'Bishop = 'PBishop
  PromoteType 'Silver = 'Gold
  PromoteType 'Pawn   = 'Gold
  PromoteType 'Lance  = 'Gold
  PromoteType 'Knight = 'Gold
  PromoteType pt      = pt

type family MovePromotedPiece (c :: Color) (from :: Sq) (to :: Sq) (mp :: Maybe Piece) (bd :: TypeBoard) :: TypeBoard where
  MovePromotedPiece _ _ _  'Nothing         bd = bd
  MovePromotedPiece _ _ to ('Just ('P c p)) bd = '(to, 'P c (PromoteType p)) ': bd

type family RemoveOnePt (pt :: PieceType) (h :: [PieceType]) :: [PieceType] where
  RemoveOnePt _  '[] = '[]
  RemoveOnePt pt (x ': xs) = If (EqPt pt x) xs (x ': RemoveOnePt pt xs)

type family ApplyWhiteHand (m :: TMove) (bd :: TypeBoard) (hand :: [PieceType]) :: [PieceType] where
  ApplyWhiteHand ('DropAt pt _) _ hand = RemoveOnePt pt hand
  ApplyWhiteHand ('MoveFromTo _ to _) bd hand = AddCaptured (LookupSq to bd) hand

type family AddCaptured (mp :: Maybe Piece) (hand :: [PieceType]) :: [PieceType] where
  AddCaptured 'Nothing             hand = hand
  AddCaptured ('Just ('P _ 'King)) hand = hand
  AddCaptured ('Just ('P _ pt))    hand = pt ': hand

-- | 持ち駒の重複駒種排除
type family NubPt (pts :: [PieceType]) :: [PieceType] where
  NubPt '[] = '[]
  NubPt (x ': xs) = x ': NubPt (RemoveAllPt x xs)

type family RemoveAllPt (x :: PieceType) (pts :: [PieceType]) :: [PieceType] where
  RemoveAllPt _ '[] = '[]
  RemoveAllPt x (y ': ys) = If (EqPt x y) (RemoveAllPt x ys) (y ': RemoveAllPt x ys)

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

-- ============================================================================
-- 4. 双対コバウンダリ (δ* K) による王手生成 ＆ F5合法応手生成
-- ============================================================================

type AllDirs8 = '[ 'DN, 'DS, 'DW, 'DE, 'DNW, 'DNE, 'DSW, 'DSE ]

-- | 玉の周囲8方向の基本単体座標リスト
type family NeighborSqs (kSq :: Sq) (dirs :: [Dir8]) :: [(Dir8, Sq)] where
  NeighborSqs _   '[] = '[]
  NeighborSqs kSq (d ': ds) = ConsMayDirSq d (Step1 d kSq) (NeighborSqs kSq ds)

type family ConsMayDirSq (d :: Dir8) (ms :: Maybe Sq) (rest :: [(Dir8, Sq)]) :: [(Dir8, Sq)] where
  ConsMayDirSq _ 'Nothing  rest = rest
  ConsMayDirSq d ('Just s) rest = '(d, s) ': rest

-- | 攻手（White）の双対コバウンダリ王手候補生成：
-- 敵玉 kSq から方向 d の隣接空マス s に対し、OppDir d の単体射線を持つ持ち駒のみを打つ
type family GenSenteCheckMoves (bd :: TypeBoard) (hand :: [PieceType]) (kSq :: Sq) :: [TMove] where
  GenSenteCheckMoves bd hand kSq =
    Append (GenCoboundaryDrops (NubPt hand) (NeighborSqs kSq AllDirs8) bd)
           (GenCoboundaryBoardMoves bd bd (NeighborSqs kSq AllDirs8))

type family GenCoboundaryDrops (pts :: [PieceType]) (ns :: [(Dir8, Sq)]) (bd :: TypeBoard) :: [TMove] where
  GenCoboundaryDrops '[] _ _ = '[]
  GenCoboundaryDrops (pt ': pts) ns bd =
    Append (DropsForPtOnNeighbors pt ns bd) (GenCoboundaryDrops pts ns bd)

type family DropsForPtOnNeighbors (pt :: PieceType) (ns :: [(Dir8, Sq)]) (bd :: TypeBoard) :: [TMove] where
  DropsForPtOnNeighbors _  '[] _ = '[]
  DropsForPtOnNeighbors pt ('(d, s) ': rest) bd =
    If (HasUnitSimplex 'White pt (OppDir d))
       (If (IsEmptySq s bd)
           ('DropAt pt s ': DropsForPtOnNeighbors pt rest bd)
           (DropsForPtOnNeighbors pt rest bd))
       (DropsForPtOnNeighbors pt rest bd)

-- | 盤上の先手駒による王手移動
type family GenCoboundaryBoardMoves (scan :: TypeBoard) (fullBd :: TypeBoard) (ns :: [(Dir8, Sq)]) :: [TMove] where
  GenCoboundaryBoardMoves '[] _ _ = '[]
  GenCoboundaryBoardMoves ('(from, 'P 'White pt) ': rest) fullBd ns =
    Append (BoardMovesToNeighbors from pt ns fullBd)
           (GenCoboundaryBoardMoves rest fullBd ns)
  GenCoboundaryBoardMoves (_ ': rest) fullBd ns =
    GenCoboundaryBoardMoves rest fullBd ns

type family BoardMovesToNeighbors (from :: Sq) (pt :: PieceType) (ns :: [(Dir8, Sq)]) (bd :: TypeBoard) :: [TMove] where
  BoardMovesToNeighbors _    _  '[] _ = '[]
  BoardMovesToNeighbors from pt ('(d, to) ': rest) bd =
    If (CanPieceAttack from 'White pt to)
       (If (HasFriendly 'White to bd)
           (BoardMovesToNeighbors from pt rest bd)
           (If (HasUnitSimplex 'White pt (OppDir d))
               ('MoveFromTo from to 'False ': BoardMovesToNeighbors from pt rest bd)
               (BoardMovesToNeighbors from pt rest bd)))
       (BoardMovesToNeighbors from pt rest bd)

-- | 受手（Black）のF5合法応手生成（王の8方向移動・王手駒の捕獲のうち、移動先がWhiteの利きにない手のみ）
type family GenGoteLegalMoves (bd :: TypeBoard) (kSq :: Sq) :: [TMove] where
  GenGoteLegalMoves bd kSq =
    Append (GenDefenderCaptures bd bd kSq)
           (GenSafeKingSteps kSq (NeighborSqs kSq AllDirs8) bd)

type family GenSafeKingSteps (kSq :: Sq) (ns :: [(Dir8, Sq)]) (bd :: TypeBoard) :: [TMove] where
  GenSafeKingSteps _   '[] _ = '[]
  GenSafeKingSteps kSq ('(_, to) ': rest) bd =
    If (HasFriendly 'Black to bd)
       (GenSafeKingSteps kSq rest bd)
       (If (IsSqAttackedBy 'White to (ApplyBoardMove 'Black ('MoveFromTo kSq to 'False) bd))
           (GenSafeKingSteps kSq rest bd)
           ('MoveFromTo kSq to 'False ': GenSafeKingSteps kSq rest bd))

type family GenDefenderCaptures (scan :: TypeBoard) (fullBd :: TypeBoard) (kSq :: Sq) :: [TMove] where
  GenDefenderCaptures '[] _ _ = '[]
  GenDefenderCaptures ('(from, 'P 'Black 'King) ': rest) fullBd kSq =
    GenDefenderCaptures rest fullBd kSq
  GenDefenderCaptures ('(from, 'P 'Black pt) ': rest) fullBd kSq =
    Append (TryCaptureCheckingPieces from pt fullBd fullBd kSq)
           (GenDefenderCaptures rest fullBd kSq)
  GenDefenderCaptures (_ ': rest) fullBd kSq =
    GenDefenderCaptures rest fullBd kSq

type family TryCaptureCheckingPieces (from :: Sq) (pt :: PieceType) (scan :: TypeBoard) (fullBd :: TypeBoard) (kSq :: Sq) :: [TMove] where
  TryCaptureCheckingPieces _ _ '[] _ _ = '[]
  TryCaptureCheckingPieces from pt ('(to, 'P 'White _) ': rest) fullBd kSq =
    If (CanPieceAttack from 'Black pt to)
       (If (IsSqAttackedBy 'White kSq (ApplyBoardMove 'Black ('MoveFromTo from to 'False) fullBd))
           (TryCaptureCheckingPieces from pt rest fullBd kSq)
           ('MoveFromTo from to 'False ': TryCaptureCheckingPieces from pt rest fullBd kSq))
       (TryCaptureCheckingPieces from pt rest fullBd kSq)
  TryCaptureCheckingPieces from pt (_ ': rest) fullBd kSq =
    TryCaptureCheckingPieces from pt rest fullBd kSq

type family UpdateKingSq (kSq :: Sq) (m :: TMove) :: Sq where
  UpdateKingSq kSq ('MoveFromTo from to _) = If (EqSq from kSq) to kSq
  UpdateKingSq kSq _ = kSq

-- ============================================================================
-- 5. 短絡評価（Alpha-Beta / CPS）型レベルAND-OR完全探索 (1パス証明木生成)
-- ============================================================================

-- | 盤面 bd, 先手持ち駒 hand, 後手玉座標 kSq, 探索深さ d に対するコンパイル時ソルバー
type family Solve (bd :: TypeBoard) (hand :: [PieceType]) (kSq :: Sq) (d :: PNat) :: GameResult where
  Solve bd hand kSq d = ToGameResult d (SearchOR bd hand kSq d)

type family ToGameResult (d :: PNat) (mp :: Maybe TProofTree) :: GameResult where
  ToGameResult d ('Just tree) = 'SenteWin d tree
  ToGameResult _ 'Nothing     = 'NoMate

-- | 先手番（ORノード）：詰みが見つかった瞬間に残りの候補手 ms の型展開を短絡打ち切り！
type family SearchOR (bd :: TypeBoard) (hand :: [PieceType]) (kSq :: Sq) (d :: PNat) :: Maybe TProofTree where
  SearchOR _  _    _   'Z     = 'Nothing
  SearchOR bd hand kSq ('S d) = TryOrMoves bd hand kSq (GenSenteCheckMoves bd hand kSq) d

type family TryOrMoves (bd :: TypeBoard) (hand :: [PieceType]) (kSq :: Sq) (ms :: [TMove]) (d :: PNat) :: Maybe TProofTree where
  TryOrMoves _  _    _   '[]       _ = 'Nothing
  TryOrMoves bd hand kSq (m ': ms) d =
    OrShortCircuit m
      (SearchAND (ApplyBoardMove 'White m bd) (ApplyWhiteHand m bd hand) kSq d)
      bd hand kSq ms d

-- | CPS短絡分岐：第2引数が 'Just subTree に簡約された瞬間、後続の ms は一切評価されない！
type family OrShortCircuit
  (m :: TMove) (res1 :: Maybe TProofTree)
  (bd :: TypeBoard) (hand :: [PieceType]) (kSq :: Sq) (ms :: [TMove]) (d :: PNat)
  :: Maybe TProofTree where
  OrShortCircuit m ('Just subTree) _  _    _   _  _ = 'Just ('TOrNode m subTree)
  OrShortCircuit _ 'Nothing        bd hand kSq ms d = TryOrMoves bd hand kSq ms d

-- | 後手番（ANDノード）：逃れ手（'Nothing）が1つでも見つかった瞬間に残りの応手を短絡打ち切り！
type family SearchAND (bd :: TypeBoard) (hand :: [PieceType]) (kSq :: Sq) (d :: PNat) :: Maybe TProofTree where
  SearchAND bd hand kSq d = WrapAndNode (TryAndMoves bd hand kSq (GenGoteLegalMoves bd kSq) d)

type family WrapAndNode (mBranches :: Maybe [(TMove, TProofTree)]) :: Maybe TProofTree where
  WrapAndNode 'Nothing         = 'Nothing
  WrapAndNode ('Just branches) = 'Just ('TAndNode branches)

type family TryAndMoves (bd :: TypeBoard) (hand :: [PieceType]) (kSq :: Sq) (ms :: [TMove]) (d :: PNat) :: Maybe [(TMove, TProofTree)] where
  TryAndMoves _  _    _   '[]       _ = 'Just '[]
  TryAndMoves _  _    _   (_ ': _) 'Z = 'Nothing
  TryAndMoves bd hand kSq (m ': ms) ('S d) =
    AndShortCircuit m
      (SearchOR (ApplyBoardMove 'Black m bd) hand (UpdateKingSq kSq m) d)
      bd hand kSq ms ('S d)

-- | ANDノード短絡分岐：第2引数が 'Nothing（逃れ）になった瞬間、残りの応手 ms は一切評価されない！
type family AndShortCircuit
  (m :: TMove) (res1 :: Maybe TProofTree)
  (bd :: TypeBoard) (hand :: [PieceType]) (kSq :: Sq) (ms :: [TMove]) (d :: PNat)
  :: Maybe [(TMove, TProofTree)] where
  AndShortCircuit _ 'Nothing        _  _    _   _  _ = 'Nothing
  AndShortCircuit m ('Just subTree) bd hand kSq ms d =
    ConsAndBranch m subTree (TryAndMoves bd hand kSq ms d)

type family ConsAndBranch (m :: TMove) (sub :: TProofTree) (rest :: Maybe [(TMove, TProofTree)]) :: Maybe [(TMove, TProofTree)] where
  ConsAndBranch _ _   'Nothing   = 'Nothing
  ConsAndBranch m sub ('Just bs) = 'Just ('(m, sub) ': bs)

-- ============================================================================
-- 6. カリー＝ハワード証明木 GADT (MateProof) と O(証明木サイズ) 具体化
-- ============================================================================

-- | コンパイル時に構築された型レベル証明木 TProofTree を保持する GADT
data MateProof (tree :: TProofTree) where
  OrNodeProof
    :: RenderMove m
    => Proxy m
    -> MateProof sub
    -> MateProof ('TOrNode m sub)

  AndNodeProof
    :: AllBranchesProof branches
    -> MateProof ('TAndNode branches)

data AllBranchesProof (branches :: [(TMove, TProofTree)]) where
  NilBranches  :: AllBranchesProof '[]
  ConsBranches :: RenderMove m
               => Proxy m
               -> MateProof sub
               -> AllBranchesProof rest
               -> AllBranchesProof ('(m, sub) ': rest)

-- | すでに型レベルで確定した TProofTree を再探索ゼロ（O(木サイズ)）で GADT 値へ具体化
class ReifyProof (tree :: TProofTree) where
  reifyProof :: MateProof tree

instance (RenderMove m, ReifyProof sub) => ReifyProof ('TOrNode m sub) where
  reifyProof = OrNodeProof (Proxy :: Proxy m) reifyProof

instance ReifyBranches branches => ReifyProof ('TAndNode branches) where
  reifyProof = AndNodeProof reifyBranches

class ReifyBranches (branches :: [(TMove, TProofTree)]) where
  reifyBranches :: AllBranchesProof branches

instance ReifyBranches '[] where
  reifyBranches = NilBranches

instance (RenderMove m, ReifyProof sub, ReifyBranches rest)
      => ReifyBranches ('(m, sub) ': rest) where
  reifyBranches = ConsBranches (Proxy :: Proxy m) reifyProof reifyBranches

-- | 座標・駒・指し手の文字列レンダリング
class KnownCoord (c :: Coord) where
  coordStr :: Proxy c -> String

instance KnownCoord 'C1 where coordStr _ = "1"
instance KnownCoord 'C2 where coordStr _ = "2"
instance KnownCoord 'C3 where coordStr _ = "3"
instance KnownCoord 'C4 where coordStr _ = "4"
instance KnownCoord 'C5 where coordStr _ = "5"
instance KnownCoord 'C6 where coordStr _ = "6"
instance KnownCoord 'C7 where coordStr _ = "7"
instance KnownCoord 'C8 where coordStr _ = "8"
instance KnownCoord 'C9 where coordStr _ = "9"

class KnownPieceName (pt :: PieceType) where
  pieceName :: Proxy pt -> String

instance KnownPieceName 'Gold    where pieceName _ = "G"
instance KnownPieceName 'Silver  where pieceName _ = "S"
instance KnownPieceName 'Rook    where pieceName _ = "R"
instance KnownPieceName 'PRook   where pieceName _ = "+R"
instance KnownPieceName 'Bishop  where pieceName _ = "B"
instance KnownPieceName 'PBishop where pieceName _ = "+B"
instance KnownPieceName 'Lance   where pieceName _ = "L"
instance KnownPieceName 'Knight  where pieceName _ = "N"
instance KnownPieceName 'Pawn    where pieceName _ = "P"
instance KnownPieceName 'King    where pieceName _ = "K"

class RenderMove (m :: TMove) where
  renderMove :: Proxy m -> String

instance (KnownCoord fx, KnownCoord fy, KnownCoord tx, KnownCoord ty)
      => RenderMove ('MoveFromTo ('Sq fx fy) ('Sq tx ty) 'False) where
  renderMove _ = coordStr (Proxy :: Proxy fx) ++ coordStr (Proxy :: Proxy fy)
              ++ "->" ++ coordStr (Proxy :: Proxy tx) ++ coordStr (Proxy :: Proxy ty)

instance (KnownCoord fx, KnownCoord fy, KnownCoord tx, KnownCoord ty)
      => RenderMove ('MoveFromTo ('Sq fx fy) ('Sq tx ty) 'True) where
  renderMove _ = coordStr (Proxy :: Proxy fx) ++ coordStr (Proxy :: Proxy fy)
              ++ "->" ++ coordStr (Proxy :: Proxy tx) ++ coordStr (Proxy :: Proxy ty) ++ "+"

instance (KnownPieceName pt, KnownCoord tx, KnownCoord ty)
      => RenderMove ('DropAt pt ('Sq tx ty)) where
  renderMove _ = pieceName (Proxy :: Proxy pt) ++ "*"
              ++ coordStr (Proxy :: Proxy tx) ++ coordStr (Proxy :: Proxy ty)

showProof :: Int -> MateProof tree -> [String]
showProof indent (OrNodeProof pm sub) =
  (replicate indent ' ' ++ "☗ Sente (OR-Node): " ++ renderMove pm)
  : showProof (indent + 4) sub
showProof indent (AndNodeProof NilBranches) =
  [replicate indent ' ' ++ "└─ [Q.E.D.] Checkmate! (Gote F5-legal responses ~ '[])"]
showProof indent (AndNodeProof branches) =
  showBranches indent branches

showBranches :: Int -> AllBranchesProof branches -> [String]
showBranches _ NilBranches = []
showBranches indent (ConsBranches pm sub rest) =
  (replicate indent ' ' ++ "☖ Gote (AND-Branch): " ++ renderMove pm)
  : showProof (indent + 4) sub
  ++ showBranches indent rest

-- ============================================================================
-- 7. コンパイル時検証テストスイート（1手詰・3手詰・5手詰・7手詰）
-- ============================================================================

-- 【問題1：1手詰】 後手玉(5,1)、先手歩(5,3)、持ち駒：金1枚 → ☗G*52 まで1手詰
type Problem1Board = '[ '( 'Sq 'C5 'C1, 'P 'Black 'King )
                      , '( 'Sq 'C5 'C3, 'P 'White 'Pawn ) ]
type Problem1Hand  = '[ 'Gold ]

verifyMateIn1 :: (Solve Problem1Board Problem1Hand ('Sq 'C5 'C1) N1 ~ 'SenteWin N1 tree)
              => MateProof tree
verifyMateIn1 = reifyProof

-- 【問題2：3手詰】
-- 後手玉(1,1)、先手歩(2,2)、先手金(2,4)、持ち駒：金2枚
-- 1手目: ☗G*21 -> 2手目: ☖11->12 (金の斜め後ろの死角へ逃げる唯一のF5合法手) -> 3手目: ☗G*13 まで3手詰！
type Problem3Board = '[ '( 'Sq 'C1 'C1, 'P 'Black 'King )
                      , '( 'Sq 'C2 'C2, 'P 'White 'Pawn )
                      , '( 'Sq 'C2 'C4, 'P 'White 'Gold ) ]
type Problem3Hand  = '[ 'Gold, 'Gold ]

verifyMateIn3 :: (Solve Problem3Board Problem3Hand ('Sq 'C1 'C1) N3 ~ 'SenteWin N3 tree)
              => MateProof tree
verifyMateIn3 = reifyProof

-- 【問題3：5手詰】
-- 後手玉(1,1)、先手金(3,1)、先手金(2,5)、持ち駒：金3枚
-- 1手目: ☗G*21 -> 2手目: ☖11->12 -> 3手目: ☗G*22 -> 4手目: ☖12->13 -> 5手目: ☗G*14 まで5手詰！
type Problem5Board = '[ '( 'Sq 'C1 'C1, 'P 'Black 'King )
                      , '( 'Sq 'C3 'C1, 'P 'White 'Gold )
                      , '( 'Sq 'C2 'C5, 'P 'White 'Gold ) ]
type Problem5Hand  = '[ 'Gold, 'Gold, 'Gold ]

verifyMateIn5 :: (Solve Problem5Board Problem5Hand ('Sq 'C1 'C1) N5 ~ 'SenteWin N5 tree)
              => MateProof tree
verifyMateIn5 = reifyProof

-- 【問題4：7手詰】
-- 後手玉(1,1)、先手金(3,1)、先手金(2,6)、持ち駒：金4枚
-- 1手目: ☗G*21 -> 2手目: ☖11->12 -> 3手目: ☗G*22 -> 4手目: ☖12->13 -> 5手目: ☗G*23 -> 6手目: ☖13->14 -> 7手目: ☗G*15 まで7手詰！
type Problem7Board = '[ '( 'Sq 'C1 'C1, 'P 'Black 'King )
                      , '( 'Sq 'C3 'C1, 'P 'White 'Gold )
                      , '( 'Sq 'C2 'C6, 'P 'White 'Gold ) ]
type Problem7Hand  = '[ 'Gold, 'Gold, 'Gold, 'Gold ]

verifyMateIn7 :: (Solve Problem7Board Problem7Hand ('Sq 'C1 'C1) N7 ~ 'SenteWin N7 tree)
              => MateProof tree
verifyMateIn7 = reifyProof

main :: IO ()
main = do
  putStrLn "================================================================================"
  putStrLn "  [Compile-Time Shogi Solver] GHC Typechecker AND-OR Proof Tree Verification"
  putStrLn "  (Pure Algebraic 8-Simplex Coboundary + CPS Short-Circuit Type Families)"
  putStrLn "================================================================================"
  putStrLn "\n[1] Mate-in-1 Proof (Solved & Verified at Compile-Time):"
  mapM_ putStrLn (showProof 2 verifyMateIn1)

  putStrLn "\n[2] Mate-in-3 Proof (OR -> AND -> OR -> Checkmate, Solved at Compile-Time):"
  mapM_ putStrLn (showProof 2 verifyMateIn3)

  putStrLn "\n[3] Mate-in-5 Proof (5-Ply Full AND-OR Tree Solved at Compile-Time):"
  mapM_ putStrLn (showProof 2 verifyMateIn5)

  putStrLn "\n[4] Mate-in-7 Proof (7-Ply Full AND-OR Tree Solved at Compile-Time):"
  mapM_ putStrLn (showProof 2 verifyMateIn7)

  putStrLn "\n>>> Q.E.D. All GameResult type equalities (Solve ~ 'SenteWin N tree) proved during GHC compilation!"
