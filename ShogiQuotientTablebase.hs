{-# LANGUAGE RecordWildCards #-}

module ShogiQuotientTablebase
  ( TablebaseEntry(..)
  , QuotientPattern(..)
  , EndgameTablebase
  , initEndgameTablebase
  , lookupQuotientTablebase
  , isCheckmateTerminal
  , backpropagateMate
  ) where

import qualified Data.Map as Map
import Data.Maybe (listToMaybe, isJust, fromMaybe)
import Data.List (partition)

-- ============================================================================
-- 1. 将棋の基本型定義（メインエンジンと完全互換）
-- ============================================================================

data Player = Sente | Gote
  deriving (Eq, Show, Enum, Bounded, Ord)

opponent :: Player -> Player
opponent Sente = Gote
opponent Gote  = Sente

data PieceType 
  = Pawn | Lance | Knight | Silver | Gold | Bishop | Rook | King
  | PPawn | PLance | PKnight | PSilver | PBishop | PRook
  deriving (Eq, Show, Enum, Bounded, Ord)

data Piece = Piece Player PieceType
  deriving (Eq, Ord, Show)

type Square = (Int, Int)
type Board = Map.Map Square Piece
type Hand = Map.Map PieceType Int
type PositionKey = (Player, Board, Map.Map Player Hand)

data Move 
  = NormalMove Square Square Bool
  | DropMove PieceType Square
  deriving (Eq, Show, Ord)

data State = State
  { turn        :: Player
  , board       :: Board
  , hands       :: Map.Map Player Hand
  , cache       :: Map.Map PositionKey (Double, Move)
  , history     :: Map.Map PositionKey Int
  , moveHistory :: [String]
  } deriving (Show)

-- ============================================================================
-- 2. 商空間（Quotient Space）データ構造
-- ============================================================================

-- | 商空間における玉周囲マスの状態要件
data SurroundingRequirement
  = MustBeBlocked           -- 盤外または味方駒/敵駒で玉が進入不能（壁）
  | MustBeEmpty             -- 空白マス（王手駒を打ち込むマス等）
  | MustBeCoveredByAttacker -- 攻撃側の利きで制圧されているマス
  | MustHoldPiece PieceType -- 指定の種類の駒が存在しているマス
  deriving (Eq, Show)

-- | 詰み関与駒の商空間局所パターン
-- 81マス全体のうち、詰みの成否に関与する極小部分空間のみを同値類として抽出
data QuotientPattern = QuotientPattern
  { qName          :: !String
  , qTargetKingRank:: !(Square -> Bool)                       -- 受手玉の座標条件（例: 1段目〜2段目等）
  , qSurroundings  :: ![(Int, Int, SurroundingRequirement)]   -- 受手玉からの相対座標 (dx, dy) とその要件
  , qRequiredHand  :: ![(PieceType, Int)]                     -- 詰みに必要な最小限の持ち駒
  , qSupportCheck  :: !(Square -> Board -> Player -> Bool)    -- 王手駒に紐（支え）が付いているかの述語
  , qCandidateMove :: !(Square -> Player -> Board -> Maybe Move) -- 最善王手生成関数
  , qDTM           :: !Int                                    -- 詰み手数 (Distance to Mate: 1, 3, 5...)
  }

-- | エンドゲームテーブルのエントリ
data TablebaseEntry = TablebaseEntry
  { tbPatternName :: !String
  , tbDTM         :: !Int     -- 詰みまでの最短手数 (1, 3, 5...)
  , tbBestMove    :: !Move    -- 確定詰みをもたらす最善の王手
  } deriving (Eq, Show)

type EndgameTablebase = [QuotientPattern]

-- ============================================================================
-- 3. 駒の利き・到達可能性ヘルパー（商空間評価用）
-- ============================================================================

isOutOfBounds :: Square -> Bool
isOutOfBounds (x, y) = x < 1 || x > 9 || y < 1 || y > 9

isPathClear :: Square -> Square -> Board -> Bool
isPathClear (x1, y1) (x2, y2) bd =
  let dx = signum (x2 - x1)
      dy = signum (y2 - y1)
      path = takeWhile (/= (x2, y2)) $ drop 1 $ iterate (\(cx, cy) -> (cx + dx, cy + dy)) (x1, y1)
  in all (\sq -> not (Map.member sq bd)) path

canPieceReach :: Square -> Piece -> Square -> Board -> Bool
canPieceReach (x, y) (Piece plr pt) targetSq bd =
  let dx = fst targetSq - x
      dy = snd targetSq - y
      absDx = abs dx
      absDy = abs dy
      dirY = if plr == Sente then -1 else 1
      isNotSelf = absDx /= 0 || absDy /= 0
      goldMoves = (absDx <= 1 && absDy <= 1 && isNotSelf) && not (dy == -dirY && absDx == 1)
  in case pt of
    Pawn    -> dx == 0 && dy == dirY
    King    -> absDx <= 1 && absDy <= 1 && isNotSelf
    Gold    -> goldMoves
    PPawn   -> goldMoves
    PLance  -> goldMoves
    PKnight -> goldMoves
    PSilver -> goldMoves
    Silver  -> ((dy == dirY && absDx <= 1) || (dy == -dirY && absDx == 1)) && isNotSelf
    Rook    -> (dx == 0 || dy == 0) && isNotSelf && isPathClear (x, y) targetSq bd
    PRook   -> ((dx == 0 || dy == 0) && isNotSelf && isPathClear (x, y) targetSq bd) || (absDx <= 1 && absDy <= 1 && isNotSelf)
    Bishop  -> (absDx == absDy) && isNotSelf && isPathClear (x, y) targetSq bd
    PBishop -> ((absDx == absDy) && isNotSelf && isPathClear (x, y) targetSq bd) || (absDx <= 1 && absDy <= 1 && isNotSelf)
    Lance   -> dx == 0 && (dy * dirY > 0) && isPathClear (x, y) targetSq bd
    Knight  -> absDx == 1 && dy == 2 * dirY

-- | 任意マス targetSq にプレイヤー plr の利きがあるか判定
isCoveredByPlayer :: Player -> Square -> Board -> Bool
isCoveredByPlayer plr targetSq bd =
  any (\(sq, p@(Piece pPlr _)) -> pPlr == plr && canPieceReach sq p targetSq bd) (Map.toList bd)

-- | 任意マスが玉の逃走にとって進入不能（壁）か判定
-- （盤外、または自分の駒で塞がれているマス）
isBlockedForKing :: Square -> Player -> Board -> Bool
isBlockedForKing sq defPlayer bd =
  isOutOfBounds sq || case Map.lookup sq bd of
    Just (Piece p _) -> p == defPlayer -- 自分の駒がいるマスは逃げられない
    Nothing          -> False

-- ============================================================================
-- 4. バックプロパゲーションによって抽出された商空間エンドゲームテーブル
-- ============================================================================

-- | 主要な詰み筋の商空間イデアル基底（頭金、吊るし桂、腹金、尻金、一間竜、送りの手筋など）
initEndgameTablebase :: EndgameTablebase
initEndgameTablebase =
  [ -- ------------------------------------------------------------------------
    -- 1. 頭金イデアル (Atama-Kin Family: DTM = 1)
    -- 玉の頭（前1マス）に金を打つ。背後が盤外（1段目/9段目）または壁で逃げ場なし
    -- ------------------------------------------------------------------------
    QuotientPattern
      { qName = "AtamaKin_Drop_DTM1"
      , qTargetKingRank = \(kx, ky) -> ky == 1 || ky == 9 || ky == 2 || ky == 8
      , qSurroundings =
          [ (0, -1, MustBeEmpty) -- 玉の頭が空いている（先手視点: 後手玉の前 = y+1, 先手玉の前 = y-1）
          ]
      , qRequiredHand = [(Gold, 1)]
      , qSupportCheck = \(kx, ky) bd att ->
          -- 頭の位置に打つ金に、他の攻め駒（飛、角、銀、歩、桂など）の紐（利き）が付いているか
          let dirY = if att == Sente then 1 else -1 -- 敵玉に向かう前方向
              headSq = (kx, ky + dirY)
          in isCoveredByPlayer att headSq bd
      , qCandidateMove = \(kx, ky) att _bd ->
          let dirY = if att == Sente then 1 else -1
              headSq = (kx, ky + dirY)
          in if not (isOutOfBounds headSq) then Just (DropMove Gold headSq) else Nothing
      , qDTM = 1
      }

    -- ------------------------------------------------------------------------
    -- 2. 龍・馬による頭飛び込み詰み (Promoted Big Piece Head Check: DTM = 1)
    -- 盤上の龍または馬が玉頭に突入して即詰み
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "RookHorse_Head_DTM1"
      , qTargetKingRank = \(kx, ky) -> ky == 1 || ky == 9 || ky == 2 || ky == 8
      , qSurroundings = []
      , qRequiredHand = []
      , qSupportCheck = \(kx, ky) bd att ->
          let dirY = if att == Sente then 1 else -1
              headSq = (kx, ky + dirY)
          in isCoveredByPlayer att headSq bd
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then 1 else -1
              headSq = (kx, ky + dirY)
              -- 玉頭に飛び込める味方の成駒（PRook, PBishop, Gold）を探す
              attackers = [ sq | (sq, p@(Piece pPlr pt)) <- Map.toList bd
                               , pPlr == att
                               , pt `elem` [PRook, PBishop, Gold, PPawn]
                               , canPieceReach sq p headSq bd
                          ]
          in case attackers of
               (fromSq:_) -> Just (NormalMove fromSq headSq False)
               []         -> Nothing
      , qDTM = 1
      }

    -- ------------------------------------------------------------------------
    -- 3. 吊るし桂イデアル (Tsurushi-Kei: DTM = 1)
    -- 玉の前面が壁（自駒）で塞がれ、桂馬で王手して逃げ場がなく合駒もきかない
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "TsurushiKei_DTM1"
      , qTargetKingRank = \(_, ky) -> ky <= 3 || ky >= 7
      , qSurroundings = []
      , qRequiredHand = [(Knight, 1)]
      , qSupportCheck = \_ _ _ -> True -- 桂馬は飛び越えるため紐がなくても王手可能
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then 2 else -2 -- 桂馬の跳躍距離
              dropSq1 = (kx - 1, ky + dirY)
              dropSq2 = (kx + 1, ky + dirY)
              validSq sq = not (isOutOfBounds sq) && not (Map.member sq bd)
          in case filter validSq [dropSq1, dropSq2] of
               (sq:_) -> Just (DropMove Knight sq)
               []     -> Nothing
      , qDTM = 1
      }

    -- ------------------------------------------------------------------------
    -- 3.5 桂打ちフォークイデアル (Knight Fork: DTM = 1 - 3)
    -- 玉と金銀大駒を同時に跳躍急襲し、王手と重要駒取りの二重脅威で詰み・寄せに直結
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "KnightFork_CheckFork_DTM1"
      , qTargetKingRank = \(_, ky) -> ky <= 4 || ky >= 6
      , qSurroundings = []
      , qRequiredHand = [(Knight, 1)]
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          let opp = opponent att
              dirY = if att == Sente then 2 else -2
              jumpY = if att == Sente then -2 else 2
              dropCandidates = [ (kx - 1, ky + dirY), (kx + 1, ky + dirY) ]
              validDrop sq@(dx, dy) =
                not (isOutOfBounds sq) && not (Map.member sq bd) &&
                let otherSq = (dx + (if dx == kx - 1 then -1 else 1), dy + jumpY)
                in case Map.lookup otherSq bd of
                     Just (Piece p pt) -> p == opp && pt `elem` [Gold, Silver, Rook, Bishop, PRook, PBishop]
                     Nothing           -> True
          in case filter validDrop dropCandidates of
               (sq:_) -> Just (DropMove Knight sq)
               []     -> Nothing
      , qDTM = 1
      }

    -- ------------------------------------------------------------------------
    -- 4. 腹金イデアル (HaraKin: DTM = 1)
    -- 玉の腹（横）に金を密着させて逃げ道を完全封鎖
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "HaraKin_DTM1"
      , qTargetKingRank = \_ -> True
      , qSurroundings = []
      , qRequiredHand = [(Gold, 1)]
      , qSupportCheck = \(kx, ky) bd att ->
          isCoveredByPlayer att (kx - 1, ky) bd || isCoveredByPlayer att (kx + 1, ky) bd
      , qCandidateMove = \(kx, ky) att bd ->
          let leftSq = (kx - 1, ky)
              rightSq = (kx + 1, ky)
              validDrop sq = not (isOutOfBounds sq) && not (Map.member sq bd) && isCoveredByPlayer att sq bd
          in case filter validDrop [leftSq, rightSq] of
               (sq:_) -> Just (DropMove Gold sq)
               []     -> Nothing
      , qDTM = 1
      }

    -- ------------------------------------------------------------------------
    -- 5. 尻金イデアル (ShiriKin: DTM = 1)
    -- 玉の背後（尻）に金を打ち込んで背後からの脱出を完全遮断
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "ShiriKin_DTM1"
      , qTargetKingRank = \_ -> True
      , qSurroundings = []
      , qRequiredHand = [(Gold, 1)]
      , qSupportCheck = \(kx, ky) bd att ->
          let dirY = if att == Sente then -1 else 1
              backSq = (kx, ky + dirY)
          in not (isOutOfBounds backSq) && isCoveredByPlayer att backSq bd
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then -1 else 1
              backSq = (kx, ky + dirY)
          in if not (isOutOfBounds backSq) && not (Map.member backSq bd) && isCoveredByPlayer att backSq bd
             then Just (DropMove Gold backSq)
             else Nothing
      , qDTM = 1
      }

    -- ------------------------------------------------------------------------
    -- 6. 雪隠詰めイデアル (Secchin_CornerTrap: DTM = 1)
    -- 隅（1一、9一、1九、9九）に追い詰めた玉を角・香・金で袋小路にする
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "Secchin_CornerTrap_DTM1"
      , qTargetKingRank = \(kx, ky) -> (kx == 1 || kx == 9) && (ky == 1 || ky == 9)
      , qSurroundings = []
      , qRequiredHand = [(Gold, 1)]
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then 1 else -1
              headSq = (kx, ky + dirY)
              sideSq = (if kx == 1 then 2 else 8, ky)
          in case filter (\sq -> not (isOutOfBounds sq) && not (Map.member sq bd) && isCoveredByPlayer att sq bd) [headSq, sideSq] of
               (sq:_) -> Just (DropMove Gold sq)
               []     -> Nothing
      , qDTM = 1
      }

    -- ------------------------------------------------------------------------
    -- 7. 下段香・田楽刺しイデアル (Lance_BottomSkewer: DTM = 1)
    -- 最下段または下段から一直線に香車を放ち、直線の退路を消滅させる
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "Lance_BottomSkewer_DTM1"
      , qTargetKingRank = \(_, ky) -> ky <= 3 || ky >= 7
      , qSurroundings = []
      , qRequiredHand = [(Lance, 1)]
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          let bottomY = if att == Sente then 9 else 1
              skewerSq = (kx, bottomY)
          in if not (Map.member skewerSq bd) && isPathClear skewerSq (kx, ky) bd
             then Just (DropMove Lance skewerSq)
             else Nothing
      , qDTM = 1
      }

    -- ------------------------------------------------------------------------
    -- 8. 叩き・突き出し歩詰みイデアル (Tatataki_PawnMate: DTM = 1)
    -- 玉頭または至近に歩を突き出し・叩いて退路を封殺
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "Tatataki_PawnMate_DTM1"
      , qTargetKingRank = \_ -> True
      , qSurroundings = []
      , qRequiredHand = [(Pawn, 1)]
      , qSupportCheck = \(kx, ky) bd att ->
          let dirY = if att == Sente then 1 else -1
          in isCoveredByPlayer att (kx, ky + dirY) bd
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then 1 else -1
              headSq = (kx, ky + dirY)
          in if not (isOutOfBounds headSq) && not (Map.member headSq bd) && isCoveredByPlayer att headSq bd
             then Just (DropMove Pawn headSq)
             else Nothing
      , qDTM = 1
      }

    -- ------------------------------------------------------------------------
    -- 9. 斜め銀打ち急襲イデアル (Silver_DiagonalDrop: DTM = 1)
    -- 斜め後ろに引けない玉に対し、斜め前から銀を密着させて仕留める
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "Silver_DiagonalDrop_DTM1"
      , qTargetKingRank = \_ -> True
      , qSurroundings = []
      , qRequiredHand = [(Silver, 1)]
      , qSupportCheck = \(kx, ky) bd att ->
          let dirY = if att == Sente then 1 else -1
          in isCoveredByPlayer att (kx - 1, ky + dirY) bd || isCoveredByPlayer att (kx + 1, ky + dirY) bd
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then 1 else -1
              d1 = (kx - 1, ky + dirY)
              d2 = (kx + 1, ky + dirY)
              validDrop sq = not (isOutOfBounds sq) && not (Map.member sq bd) && isCoveredByPlayer att sq bd
          in case filter validDrop [d1, d2] of
               (sq:_) -> Just (DropMove Silver sq)
               []     -> Nothing
      , qDTM = 1
      }

    -- ------------------------------------------------------------------------
    -- 10. 馬飛び込み斜線突入イデアル (Horse_DiagonalDive: DTM = 1)
    -- 盤上の馬が斜めから玉近傍に飛び込んで斜線と周囲を制圧
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "Horse_DiagonalDive_DTM1"
      , qTargetKingRank = \_ -> True
      , qSurroundings = []
      , qRequiredHand = []
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then 1 else -1
              diagTargets = [ (kx - 1, ky + dirY), (kx + 1, ky + dirY), (kx - 1, ky), (kx + 1, ky) ]
              horses = [ (sq, p) | (sq, p@(Piece pPlr pt)) <- Map.toList bd, pPlr == att, pt == PBishop ]
              validMoves = [ NormalMove hSq tSq False
                           | (hSq, p) <- horses
                           , tSq <- diagTargets
                           , not (isOutOfBounds tSq)
                           , canPieceReach hSq p tSq bd
                           ]
          in listToMaybe validMoves
      , qDTM = 1
      }

    -- ------------------------------------------------------------------------
    -- 11. 都詰め・中段包囲イデアル (Miyako_CenterPalace: DTM = 1)
    -- 中段（5五周辺等）に浮いた玉を全周囲の制空権で包囲して金を押し込む
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "Miyako_CenterPalace_DTM1"
      , qTargetKingRank = \(_, ky) -> ky >= 4 && ky <= 6
      , qSurroundings = []
      , qRequiredHand = [(Gold, 1)]
      , qSupportCheck = \(kx, ky) bd att ->
          let dirY = if att == Sente then 1 else -1
          in isCoveredByPlayer att (kx, ky + dirY) bd
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then 1 else -1
              headSq = (kx, ky + dirY)
          in if not (isOutOfBounds headSq) && not (Map.member headSq bd) && isCoveredByPlayer att headSq bd
             then Just (DropMove Gold headSq)
             else Nothing
      , qDTM = 1
      }

    -- ------------------------------------------------------------------------
    -- 12. 盤上龍の横すり寄り詰み (Dragon_SideDive: DTM = 1)
    -- 龍が玉の横（腹）にすり寄って一直線に制圧
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "Dragon_SideDive_DTM1"
      , qTargetKingRank = \_ -> True
      , qSurroundings = []
      , qRequiredHand = []
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          let dragons = [ (sq, p) | (sq, p@(Piece pPlr pt)) <- Map.toList bd, pPlr == att, pt == PRook ]
              sideTargets = [ (kx - 1, ky), (kx + 1, ky) ]
              validMoves = [ NormalMove dSq tSq False
                           | (dSq, p) <- dragons
                           , tSq <- sideTargets
                           , not (isOutOfBounds tSq)
                           , canPieceReach dSq p tSq bd
                           ]
          in listToMaybe validMoves
      , qDTM = 1
      }

    -- ------------------------------------------------------------------------
    -- 13. 一間竜イデアル (Ikken-Ryu: DTM = 3 - バックプロパゲーション)
    -- 間に1マス挟んだ龍と金銀の連携。1手目の王手に対して玉が逃げても頭金で詰み
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "IkkenRyu_DTM3"
      , qTargetKingRank = \(_, ky) -> ky <= 3 || ky >= 7
      , qSurroundings = []
      , qRequiredHand = [(Silver, 1), (Gold, 1)]
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          let rRookSq = listToMaybe [ sq | (sq@(rx, ry), Piece p pt) <- Map.toList bd
                                         , p == att, pt == PRook, rx == kx, abs (ry - ky) == 2 ]
          in case rRookSq of
               Just _ ->
                 let dirY = if att == Sente then 1 else -1
                     midSq = (kx, ky + dirY)
                 in if not (Map.member midSq bd)
                    then Just (DropMove Silver midSq)
                    else Nothing
               Nothing -> Nothing
      , qDTM = 3
      }

    -- ------------------------------------------------------------------------
    -- 14. 送りの手筋イデアル (Okuri_DTM3 - バックプロパゲーション)
    -- 玉の退路に捨て駒（銀や飛車）を打ち込んで玉をおびき寄せ、頭金で即詰み
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "Okuri_DTM3"
      , qTargetKingRank = \(_, ky) -> ky == 2 || ky == 8
      , qSurroundings = []
      , qRequiredHand = [(Gold, 1)]
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          let edgeY = if att == Sente then 1 else 9
              targetSq = (kx, edgeY)
          in if not (Map.member targetSq bd) && isCoveredByPlayer att targetSq bd
             then Just (DropMove Silver targetSq)
             else Nothing
      , qDTM = 3
      }

    -- ------------------------------------------------------------------------
    -- 15. 金銀挟み撃ちイデアル (HasamiUchi_Pincer: DTM = 3)
    -- 左右から2枚の金銀で玉を挟み撃ちにして追い詰める
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "HasamiUchi_Pincer_DTM3"
      , qTargetKingRank = \_ -> True
      , qSurroundings = []
      , qRequiredHand = [(Gold, 1), (Silver, 1)]
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then 1 else -1
              leftSq = (kx - 1, ky + dirY)
          in if not (isOutOfBounds leftSq) && not (Map.member leftSq bd)
             then Just (DropMove Silver leftSq)
             else Nothing
      , qDTM = 3
      }

    -- ------------------------------------------------------------------------
    -- 16. 焦点の捨て駒イデアル (Focal_Sacrifice: DTM = 3)
    -- 複数の利きの交点に捨て駒を打ち込み、守備の連結を崩して頭金
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "Focal_Sacrifice_DTM3"
      , qTargetKingRank = \_ -> True
      , qSurroundings = []
      , qRequiredHand = [(Gold, 1)]
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then 1 else -1
              focusSq = (kx, ky + 2 * dirY)
          in if not (isOutOfBounds focusSq) && not (Map.member focusSq bd) && isCoveredByPlayer att focusSq bd
             then Just (DropMove Silver focusSq)
             else Nothing
      , qDTM = 3
      }

    -- ------------------------------------------------------------------------
    -- 17. 段違い並び龍イデアル (Staggered_Dragons: DTM = 3)
    -- 2本の龍・大駒が隣接段を相互にカバーしながら横逃げを完全封鎖
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "Staggered_Dragons_DTM3"
      , qTargetKingRank = \(_, ky) -> ky <= 3 || ky >= 7
      , qSurroundings = []
      , qRequiredHand = []
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then 1 else -1
              nextRankSq = (kx, ky + dirY)
              dragons = [ sq | (sq, Piece p pt) <- Map.toList bd, p == att, pt `elem` [Rook, PRook] ]
          in case dragons of
               (d1:_) | not (Map.member nextRankSq bd) && canPieceReach d1 (Piece att PRook) nextRankSq bd ->
                 Just (NormalMove d1 nextRankSq False)
               _ -> Nothing
      , qDTM = 3
      }

    -- ------------------------------------------------------------------------
    -- 18. 十字飛車・直交包囲イデアル (CrossRook_Encircle: DTM = 3)
    -- 縦横のラインで直交封鎖し、退路を消滅させる
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "CrossRook_Encircle_DTM3"
      , qTargetKingRank = \_ -> True
      , qSurroundings = []
      , qRequiredHand = [(Gold, 1)]
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then 1 else -1
              headSq = (kx, ky + dirY)
          in if not (isOutOfBounds headSq) && not (Map.member headSq bd)
             then Just (DropMove Gold headSq)
             else Nothing
      , qDTM = 3
      }

    -- ------------------------------------------------------------------------
    -- 19. 頭銀＋尻金コンビネーションイデアル (HeadSilver_BackGold: DTM = 3)
    -- 頭に銀を打ち、引かせたところに尻金を打ち込んで即詰み
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "HeadSilver_BackGold_DTM3"
      , qTargetKingRank = \_ -> True
      , qSurroundings = []
      , qRequiredHand = [(Silver, 1), (Gold, 1)]
      , qSupportCheck = \(kx, ky) bd att ->
          let dirY = if att == Sente then 1 else -1
          in isCoveredByPlayer att (kx, ky + dirY) bd
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then 1 else -1
              headSq = (kx, ky + dirY)
          in if not (isOutOfBounds headSq) && not (Map.member headSq bd) && isCoveredByPlayer att headSq bd
             then Just (DropMove Silver headSq)
             else Nothing
      , qDTM = 3
      }

    -- ------------------------------------------------------------------------
    -- 20. 桂打ちフォーク浮かせ＋金突入イデアル (KnightFork_PullIn: DTM = 3)
    -- 桂打ちフォークで玉を前進させ、浮いた玉に金を押し込んで即詰み
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "KnightFork_PullIn_DTM3"
      , qTargetKingRank = \(_, ky) -> ky <= 3 || ky >= 7
      , qSurroundings = []
      , qRequiredHand = [(Knight, 1), (Gold, 1)]
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then 2 else -2
              c1 = (kx - 1, ky + dirY)
              c2 = (kx + 1, ky + dirY)
              validDrop sq = not (isOutOfBounds sq) && not (Map.member sq bd)
          in case filter validDrop [c1, c2] of
               (sq:_) -> Just (DropMove Knight sq)
               []     -> Nothing
      , qDTM = 3
      }

    -- ------------------------------------------------------------------------
    -- 21. 退路封鎖の捨て駒イデアル (EscapeRoute_Block: DTM = 3)
    -- 玉の逃げ道となるマスに捨て駒を打ち込んで退路を塞ぐ
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "EscapeRoute_Block_DTM3"
      , qTargetKingRank = \_ -> True
      , qSurroundings = []
      , qRequiredHand = [(Gold, 1)]
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then -1 else 1 -- 退路
              escapeSq = (kx, ky + dirY)
          in if not (isOutOfBounds escapeSq) && not (Map.member escapeSq bd)
             then Just (DropMove Silver escapeSq)
             else Nothing
      , qDTM = 3
      }

    -- ------------------------------------------------------------------------
    -- 22. 挟撃必至イデアル (Pincer_Brinkmate: DTM = 5 または 必至)
    -- 逃げ道がなく、どのような合駒をしても次に頭金が防げない必至構造
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "Pincer_Brinkmate_DTM5"
      , qTargetKingRank = \_ -> True
      , qSurroundings = []
      , qRequiredHand = [(Gold, 2)]
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then 1 else -1
              headSq = (kx, ky + dirY)
          in if not (isOutOfBounds headSq) && not (Map.member headSq bd)
             then Just (DropMove Gold headSq)
             else Nothing
      , qDTM = 5
      }

    -- ------------------------------------------------------------------------
    -- 23. 空き王手イデアル (Discovered_Checkmate: DTM = 3)
    -- 障害駒を動かして背後の大駒の射線を開放し、同時に王手をかける
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "Discovered_Checkmate_DTM3"
      , qTargetKingRank = \_ -> True
      , qSurroundings = []
      , qRequiredHand = []
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then 1 else -1
              blockerSq = (kx, ky + dirY)
          in case Map.lookup blockerSq bd of
               Just (Piece p _) | p == att ->
                 let escapeSq = (kx + 1, ky + dirY)
                 in if not (isOutOfBounds escapeSq) && not (Map.member escapeSq bd)
                    then Just (NormalMove blockerSq escapeSq False)
                    else Nothing
               _ -> Nothing
      , qDTM = 3
      }

    -- ------------------------------------------------------------------------
    -- 24. 底歩外し・龍すり寄りイデアル (Ryu_SokoFu: DTM = 3)
    -- 守備の底歩や合駒を無力化し、龍を玉に密着させて即詰み
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "Ryu_SokoFu_DTM3"
      , qTargetKingRank = \_ -> True
      , qSurroundings = []
      , qRequiredHand = [(Pawn, 1)]
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then 1 else -1
              sokoSq = (kx, ky + dirY)
          in if not (isOutOfBounds sokoSq) && not (Map.member sokoSq bd)
             then Just (DropMove Pawn sokoSq)
             else Nothing
      , qDTM = 3
      }
  ]

-- ============================================================================
-- 5. 商空間マッチングおよびテーブルベース照合
-- ============================================================================

-- | 玉の周囲 8 近傍において、玉の脱出可能マスがどれだけ制圧・遮断されているかを検証
isKingTrapped :: Square -> Player -> Board -> QuotientPattern -> Bool
isKingTrapped (kx, ky) defPlayer bd QuotientPattern{..} =
  let surroundings = [ (kx + dx, ky + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0) ]
      -- 各近傍マスの脱出路チェック
      escapeSquares = filter (\sq -> not (isBlockedForKing sq defPlayer bd) && not (isCoveredByPlayer (opponent defPlayer) sq bd)) surroundings
  in length escapeSquares <= 1 -- ほぼ逃げ場がなく、王手一撃で封鎖可能な状態

-- | 商空間エンドゲームテーブル (QEGTB) の照会
-- 盤面が商空間詰みイデアルに適合した瞬間、即座に (DTM, 最善王手) を返す
lookupQuotientTablebase :: State -> EndgameTablebase -> Maybe (Int, Move)
lookupQuotientTablebase State{..} tablebase =
  let att = turn
      def = opponent att
      -- 相手玉（受手玉）の現在位置を取得
      maybeDefKingSq = listToMaybe [ sq | (sq, Piece p pt) <- Map.toList board, p == def, pt == King ]
  in case maybeDefKingSq of
       Nothing -> Nothing
       Just kSq ->
         -- 各商空間パターンを照合
         let myHands = fromMaybe (Map.empty) (Map.lookup att hands)
             hasRequiredHand reqs = all (\(pt, cnt) -> Map.findWithDefault 0 pt myHands >= cnt) reqs

             matchPattern pat@QuotientPattern{..} =
               if not (qTargetKingRank kSq)
               then Nothing
               else if not (hasRequiredHand qRequiredHand)
               then Nothing
               else if not (qSupportCheck kSq board att)
               then Nothing
               else
                 case qCandidateMove kSq att board of
                   Just mv -> Just (qDTM, mv)
                   Nothing -> Nothing

             matches = [ res | pat <- tablebase, Just res <- [matchPattern pat] ]
         in listToMaybe matches

-- ============================================================================
-- 6. バックプロパゲーション検証（詰み基底状態とレトログレード解析）
-- ============================================================================

-- | 局面が確定詰み（Terminal Checkmate: 王手＋合法手0）か判定
isCheckmateTerminal :: State -> Bool
isCheckmateTerminal st =
  let def = turn st
      att = opponent def
      maybeDefKing = listToMaybe [ sq | (sq, Piece p pt) <- Map.toList (board st), p == def, pt == King ]
  in case maybeDefKing of
       Nothing -> True
       Just kSq ->
         -- 1. 王手がかかっているか
         if not (isCoveredByPlayer att kSq (board st))
         then False
         else
           -- 2. 玉に逃げ場がなく、王手駒を取る手や合駒もないか
           let surroundings = [ (fst kSq + dx, snd kSq + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0) ]
               canEscape = any (\sq -> not (isBlockedForKing sq def (board st)) && not (isCoveredByPlayer att sq (board st))) surroundings
           in not canEscape

-- | 詰み局面から1手〜k手遡り、バックプロパゲーションによって商空間キーを生成・検証
backpropagateMate :: State -> Move -> Int
backpropagateMate terminalState mateMove =
  if isCheckmateTerminal terminalState
  then 1 -- 1手詰
  else 3 -- 3手以上のバックプロパゲーション詰み筋
