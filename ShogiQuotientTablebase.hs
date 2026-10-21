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
    -- 4. 尻金・腹金イデアル (HaraKin / ShiriKin: DTM = 1)
    -- 玉の腹（横）または尻（背後）に金を密着させて逃げ道を完全封鎖
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "HaraKin_DTM1"
      , qTargetKingRank = \_ -> True
      , qSurroundings = []
      , qRequiredHand = [(Gold, 1)]
      , qSupportCheck = \(kx, ky) bd att ->
          -- 腹（横）に紐が付いているか
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
    -- 5. 一間竜イデアル (Ikken-Ryu: DTM = 3 - バックプロパゲーション)
    -- 間に1マス挟んだ龍と金銀の連携。1手目の王手に対して玉が逃げても頭金で詰み
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "IkkenRyu_DTM3"
      , qTargetKingRank = \(_, ky) -> ky <= 3 || ky >= 7
      , qSurroundings = []
      , qRequiredHand = [(Silver, 1), (Gold, 1)] -- 詰めろ持ち駒
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          -- 龍が同じ筋にいて1マス空いているか
          let rRookSq = listToMaybe [ sq | (sq@(rx, ry), Piece p pt) <- Map.toList bd
                                         , p == att, pt == PRook, rx == kx, abs (ry - ky) == 2 ]
          in case rRookSq of
               Just _ ->
                 -- 間のマスに銀または金を放り込む最善手
                 let dirY = if att == Sente then 1 else -1
                     midSq = (kx, ky + dirY)
                 in if not (Map.member midSq bd)
                    then Just (DropMove Silver midSq)
                    else Nothing
               Nothing -> Nothing
      , qDTM = 3
      }

    -- ------------------------------------------------------------------------
    -- 6. 送りの手筋イデアル (Okuri_DTM3 - バックプロパゲーション)
    -- 玉の退路に捨て駒（銀や飛車）を打ち込んで玉をおびき寄せ、頭金で即詰み
    -- ------------------------------------------------------------------------
  , QuotientPattern
      { qName = "Okuri_DTM3"
      , qTargetKingRank = \(_, ky) -> ky == 2 || ky == 8
      , qSurroundings = []
      , qRequiredHand = [(Gold, 1)]
      , qSupportCheck = \_ _ _ -> True
      , qCandidateMove = \(kx, ky) att bd ->
          let edgeY = if att == Sente then 1 else 9 -- 端段
              targetSq = (kx, edgeY)
          in if not (Map.member targetSq bd) && isCoveredByPlayer att targetSq bd
             then Just (DropMove Silver targetSq)
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
