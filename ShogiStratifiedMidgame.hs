{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ShogiStratifiedMidgame
  ( -- * 1. 基本型
    Player(..)
  , opponent
  , PieceType(..)
  , Piece(..)
  , Square
  , Board
  , Hand
  , PositionKey
  , Move(..)
  , State(..)
  , positionKey
  , emptyHand
  , initState
  , initBoard
  , showMove
  , parseMoveUSI
  , parseSquareUSI
  , showSquare

    -- * 2. 駒の移動・合法手・シジジー判定
  , isOutOfBounds
  , isPathClear
  , canPieceReachTarget
  , getPiecesCovering
  , isCoveredBy
  , isSquareUnderAttack
  , findKing
  , isKingUnderAttack
  , generateMoves
  , f5SyzygyFilter
  , applyMove
  , isTempoLossMove
  , isBlunderMove

    -- * 3. 中盤層化商空間 (Stratified Quotient Space)
  , HandStratumKey(..)
  , PromotionStratumKey(..)
  , DropRole(..)
  , DropStratumKey(..)
  , MidgameStratum(..)
  , TsumeroType(..)
  , detectMidgameTsumero
  , OpeningMultiTsumeroType(..)
  , detectOpeningMultiTsumero
  , stratifyHand
  , stratifyPromotion
  , classifyDropRole
  , isKnightForkDrop
  , stratifyDrop
  , stratifyState
  , showStratumKey

    -- * 4. 中盤モースポテンシャル評価関数
  , evaluateStratifiedState
  , isMorseEquilibrium
  , pieceValue

    -- * 5. 商空間エンドゲームテーブル (QEGTB) ＆ バックプロパゲーション
  , DynamicQEGTBEntry(..)
  , DynamicQEGTB
  , initSeedQEGTB
  , initMetaIdeals512
  , isCheckmatePosition
  , extractAndBackpropagateQEGTB
  , matchQEGTB
  ) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate, isPrefixOf, isSuffixOf, sortBy, foldl', partition)
import Data.Char (isDigit, digitToInt, toLower, isUpper, toUpper, ord, chr)
import Data.Maybe (listToMaybe, fromMaybe, isJust, mapMaybe)
import Text.Printf (printf)

-- ============================================================================
-- 1. 基本型定義 (局面、駒、手番、盤面)
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

data Piece = Piece
  { piecePlayer :: !Player
  , pieceType   :: !PieceType
  } deriving (Eq, Show, Ord)

type Square = (Int, Int)
type Board = Map.Map Square Piece
type Hand = Map.Map PieceType Int
type PositionKey = (Player, Board, Map.Map Player Hand)

data Move
  = NormalMove !Square !Square !Bool
  | DropMove !PieceType !Square
  deriving (Eq, Show, Ord)

data State = State
  { turn        :: !Player
  , board       :: !Board
  , hands       :: !(Map.Map Player Hand)
  , cache       :: !(Map.Map PositionKey (Double, Move))
  , history     :: !(Map.Map PositionKey Int)
  , moveHistory :: ![String]
  } deriving (Show)

positionKey :: State -> PositionKey
positionKey State{..} = (turn, board, hands)

emptyHand :: Hand
emptyHand = Map.fromList
  [ (Pawn, 0), (Lance, 0), (Knight, 0), (Silver, 0)
  , (Gold, 0), (Bishop, 0), (Rook, 0)
  ]

initBoard :: Board
initBoard = Map.fromList $
  -- 1段目
  [ ((9,1), Piece Gote Lance),  ((8,1), Piece Gote Knight), ((7,1), Piece Gote Silver), ((6,1), Piece Gote Gold)
  , ((5,1), Piece Gote King),   ((4,1), Piece Gote Gold),   ((3,1), Piece Gote Silver), ((2,1), Piece Gote Knight)
  , ((1,1), Piece Gote Lance)
  -- 2段目
  , ((8,2), Piece Gote Rook),   ((2,2), Piece Gote Bishop)
  -- 3段目
  ] ++ [((i,3), Piece Gote Pawn) | i <- [1..9]] ++
  -- 7段目
  [((i,7), Piece Sente Pawn) | i <- [1..9]] ++
  -- 8段目
  [ ((8,8), Piece Sente Bishop), ((2,8), Piece Sente Rook)
  -- 9段目
  , ((9,9), Piece Sente Lance),  ((8,9), Piece Sente Knight), ((7,9), Piece Sente Silver), ((6,9), Piece Sente Gold)
  , ((5,9), Piece Sente King),   ((4,9), Piece Sente Gold),   ((3,9), Piece Sente Silver), ((2,9), Piece Sente Knight)
  , ((1,9), Piece Sente Lance)
  ]

initState :: State
initState =
  let st = State Sente initBoard (Map.fromList [(Sente, emptyHand), (Gote, emptyHand)]) Map.empty Map.empty []
  in st { history = Map.singleton (positionKey st) 1 }

showSquare :: Square -> String
showSquare (f, r) = show f ++ [chr (ord 'a' + r - 1)]

showMove :: Move -> String
showMove (NormalMove from to promo) =
  showSquare from ++ showSquare to ++ (if promo then "+" else "")
showMove (DropMove p to) =
  let pieceChar = case p of
        Pawn   -> 'P' ; Lance  -> 'L' ; Knight -> 'N'
        Silver -> 'S' ; Gold   -> 'G' ; Bishop -> 'B' ; Rook   -> 'R'
        _      -> ' '
  in pieceChar : '*' : showSquare to

parseSquareUSI :: String -> Square
parseSquareUSI [f, r] = (digitToInt f, ord r - ord 'a' + 1)
parseSquareUSI _      = (1, 1)

parseMoveUSI :: String -> Move
parseMoveUSI str
  | '*' `elem` str =
      case break (== '*') str of
        (pStr, _:toStr) ->
          let pt = case map toUpper pStr of
                ('P':_) -> Pawn ; ('L':_) -> Lance ; ('N':_) -> Knight
                ('S':_) -> Silver ; ('G':_) -> Gold ; ('B':_) -> Bishop ; ('R':_) -> Rook
                _       -> Pawn
          in DropMove pt (parseSquareUSI toStr)
        _ -> DropMove Pawn (1, 1)
  | otherwise =
      let fromStr = take 2 str
          toStr   = take 2 (drop 2 str)
          promo   = "+" `isSuffixOf` str
      in NormalMove (parseSquareUSI fromStr) (parseSquareUSI toStr) promo

-- ============================================================================
-- 2. 幾何学的移動・利き判定・シジジーフィルター
-- ============================================================================

isOutOfBounds :: Square -> Bool
isOutOfBounds (x, y) = x < 1 || x > 9 || y < 1 || y > 9

isPathClear :: Square -> Square -> Board -> Bool
isPathClear (x1, y1) (x2, y2) bd =
  let dx = signum (x2 - x1)
      dy = signum (y2 - y1)
      path = takeWhile (/= (x2, y2)) $ drop 1 $ iterate (\(cx, cy) -> (cx + dx, cy + dy)) (x1, y1)
  in all (\sq -> not (Map.member sq bd)) path

canPieceReachTarget :: Square -> Piece -> Square -> Board -> Bool
canPieceReachTarget (x, y) (Piece plr pt) targetSq bd =
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

getPiecesCovering :: Player -> Square -> Board -> [(Square, Piece)]
getPiecesCovering plr targetSq bd =
  [ (sq, p) | (sq, p@(Piece pPlr _)) <- Map.toList bd, pPlr == plr, canPieceReachTarget sq p targetSq bd ]

isCoveredBy :: Player -> Square -> Board -> Bool
isCoveredBy plr targetSq bd = not (null (getPiecesCovering plr targetSq bd))

isSquareUnderAttack :: Player -> Square -> Board -> Bool
isSquareUnderAttack attackerPlayer targetSq bd = isCoveredBy attackerPlayer targetSq bd

findKing :: Player -> Board -> Maybe Square
findKing plr b = listToMaybe [sq | (sq, Piece p pt) <- Map.toList b, p == plr, pt == King]

isKingUnderAttack :: Player -> Board -> Bool
isKingUnderAttack plr bd =
  case findKing plr bd of
    Just kSq -> isSquareUnderAttack (opponent plr) kSq bd
    Nothing  -> True

applyMove :: Move -> State -> State
applyMove (NormalMove from to promo) State{..} =
  let piece = Map.lookup from board
      captured = Map.lookup to board
      boardWithSourceRemoved = Map.delete from board
      newPiece = case piece of
        Just (Piece plr pt) ->
          if promo
          then Piece plr (promote pt)
          else Piece plr pt
        Nothing -> Piece turn Pawn
      newBoard = Map.insert to newPiece boardWithSourceRemoved

      newHands = case captured of
        Just (Piece _ pt) ->
          let rawPt = demote pt
              handMap = fromMaybe emptyHand (Map.lookup turn hands)
              updatedHand = Map.insertWith (+) rawPt 1 handMap
          in Map.insert turn updatedHand hands
        Nothing -> hands

      nextSt = State (opponent turn) newBoard newHands cache history (moveHistory ++ [showMove (NormalMove from to promo)])
      newKey = positionKey nextSt
      newHistory = Map.insertWith (+) newKey 1 history
  in nextSt { history = newHistory }
  where
    promote Pawn   = PPawn   ; promote Lance  = PLance
    promote Knight = PKnight ; promote Silver = PSilver
    promote Bishop = PBishop ; promote Rook   = PRook
    promote pt     = pt

    demote PPawn   = Pawn   ; demote PLance  = Lance
    demote PKnight = Knight ; demote PSilver = Silver
    demote PBishop = Bishop ; demote PRook   = Rook
    demote pt      = pt

applyMove (DropMove pt to) State{..} =
  let newBoard = Map.insert to (Piece turn pt) board
      handMap = fromMaybe emptyHand (Map.lookup turn hands)
      updatedHand = Map.adjust (\old -> old - 1) pt handMap
      newHands = Map.insert turn updatedHand hands

      nextSt = State (opponent turn) newBoard newHands cache history (moveHistory ++ [showMove (DropMove pt to)])
      newKey = positionKey nextSt
      newHistory = Map.insertWith (+) newKey 1 history
  in nextSt { history = newHistory }

generateMoves :: State -> [Move]
generateMoves State{..} =
  let normalMoves = concat [ generatePieceMoves sq piece board | (sq, piece@(Piece p _)) <- Map.toList board, p == turn ]
      dropMoves = generateDropMoves turn hands board
  in normalMoves ++ dropMoves

generatePieceMoves :: Square -> Piece -> Board -> [Move]
generatePieceMoves from@(fx, fy) piece@(Piece plr pt) bd =
  let dirY = if plr == Sente then -1 else 1
      goldTargets = [ (fx + dx, fy + dirY) | dx <- [-1..1] ] ++ [ (fx - 1, fy), (fx + 1, fy), (fx, fy - dirY) ]
      targetSquares = case pt of
        Pawn   -> [ (fx, fy + dirY) ]
        Lance  -> [ (fx, fy + d * dirY) | d <- [1..8] ]
        Knight -> [ (fx - 1, fy + 2 * dirY), (fx + 1, fy + 2 * dirY) ]
        Silver -> [ (fx + dx, fy + dirY) | dx <- [-1..1] ] ++ [ (fx - 1, fy - dirY), (fx + 1, fy - dirY) ]
        Gold   -> goldTargets
        PPawn  -> goldTargets
        PLance -> goldTargets
        PKnight-> goldTargets
        PSilver-> goldTargets
        King   -> [ (fx + dx, fy + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0) ]
        Bishop -> [ (fx + d * dx, fy + d * dy) | d <- [1..8], dx <- [-1, 1], dy <- [-1, 1] ]
        PBishop-> [ (fx + d * dx, fy + d * dy) | d <- [1..8], dx <- [-1, 1], dy <- [-1, 1] ] ++
                  [ (fx + dx, fy + dy) | dx <- [-1..1], dy <- [-1..1], abs dx + abs dy == 1 ]
        Rook   -> [ (fx + d * dx, fy) | d <- [1..8], dx <- [-1, 1] ] ++ [ (fx, fy + d * dy) | d <- [1..8], dy <- [-1, 1] ]
        PRook  -> [ (fx + d * dx, fy) | d <- [1..8], dx <- [-1, 1] ] ++ [ (fx, fy + d * dy) | d <- [1..8], dy <- [-1, 1] ] ++
                  [ (fx + dx, fy + dy) | dx <- [-1..1], dy <- [-1..1], abs dx == 1 && abs dy == 1 ]
      validTargets = filter (\to -> not (isOutOfBounds to) && canPieceReachTarget from piece to bd &&
                                    case Map.lookup to bd of
                                      Just (Piece p _) -> p /= plr
                                      Nothing          -> True) targetSquares
  in concatMap (makeMoves from pt plr) validTargets
  where
    makeMoves from' pt' plr' to@(_, ty) =
      let inPromoZone = if plr' == Sente then ty <= 3 || fy <= 3 else ty >= 7 || fy >= 7
          canPromo = inPromoZone && pt' `elem` [Pawn, Lance, Knight, Silver, Bishop, Rook]
          mustPromo = case pt' of
            Pawn   -> if plr' == Sente then ty == 1 else ty == 9
            Lance  -> if plr' == Sente then ty == 1 else ty == 9
            Knight -> if plr' == Sente then ty <= 2 else ty >= 8
            _      -> False
      in if mustPromo
         then [NormalMove from' to True]
         else if canPromo
              then [NormalMove from' to True, NormalMove from' to False]
              else [NormalMove from' to False]

generateDropMoves :: Player -> Map.Map Player Hand -> Board -> [Move]
generateDropMoves plr handsMap bd =
  let myHand = fromMaybe emptyHand (Map.lookup plr handsMap)
      emptySquares = [ (x, y) | x <- [1..9], y <- [1..9], not (Map.member (x, y) bd) ]
      pawnColumns = [ x | ((x, _), Piece p pt) <- Map.toList bd, p == plr, pt == Pawn ]
      availablePieces = [ pt | (pt, cnt) <- Map.toList myHand, cnt > 0 ]
  in concat [ [ DropMove pt sq | sq@(sx, sy) <- emptySquares, isValidDrop pt plr sx sy pawnColumns ]
            | pt <- availablePieces ]
  where
    isValidDrop pt' plr' sx sy pCols = case pt' of
      Pawn   -> sx `notElem` pCols && (if plr' == Sente then sy > 1 else sy < 9)
      Lance  -> if plr' == Sente then sy > 1 else sy < 9
      Knight -> if plr' == Sente then sy > 2 else sy < 8
      _      -> True

-- | F5シジジーフィルター: 自玉が王手下に放置される手を完全排除
f5SyzygyFilter :: State -> [Move] -> [Move]
f5SyzygyFilter st moves = filter isValidMove moves
  where
    plr = turn st
    isValidMove m =
      let nextSt = applyMove m st
      in not (isKingUnderAttack plr (board nextSt))

-- | 手損手の判定（無駄な端香車上がり、角頭歩突き自殺手）
isFutileLanceLift :: State -> Move -> Bool
isFutileLanceLift st (NormalMove from to _) =
  let bd = board st
      plr = turn st
  in case Map.lookup from bd of
       Just (Piece p Lance) | p == plr ->
         let (fx, fy) = from
             isFromBackRank = if plr == Sente then fy >= 8 else fy <= 2
             opp = opponent plr
             hasOpponentEdgeThreat =
               any (\((ox, oy), Piece op _) ->
                     op == opp && ox == fx &&
                     (if plr == Sente then oy >= 4 else oy <= 6)
                   ) (Map.toList bd)
             kingPos = findKing plr bd
             isCompletedAnaguma = case kingPos of
               Just (kx, ky) -> if plr == Sente
                                  then (fx == 1 && kx == 1 && ky == 9) || (fx == 9 && kx == 9 && ky == 9)
                                  else (fx == 1 && kx == 1 && ky == 1) || (fx == 9 && kx == 9 && ky == 1)
               Nothing -> False
         in isFromBackRank && not hasOpponentEdgeThreat && not isCompletedAnaguma
       _ -> False
isFutileLanceLift _ _ = False

isPushingKakutoPawn :: State -> Move -> Bool
isPushingKakutoPawn st (NormalMove from to _) =
  let bd = board st
      plr = turn st
  in case Map.lookup from bd of
       Just (Piece p Pawn) | p == plr ->
         let opp = opponent plr
             isOppCovered = isCoveredBy opp to bd
             isSelfCovered = isCoveredBy plr to bd
         in isOppCovered && not isSelfCovered
       _ -> False
isPushingKakutoPawn _ _ = False

isTempoLossMove :: State -> Move -> Bool
isTempoLossMove st m = isFutileLanceLift st m || isPushingKakutoPawn st m

isBlunderMove :: State -> Move -> Bool
isBlunderMove st m =
  let plr = turn st
      opp = opponent plr
      bd = board st
      (toSq, ptMoved) = case m of
        NormalMove from to _ ->
          case Map.lookup from bd of
            Just (Piece _ pt) -> (to, pt)
            Nothing           -> (to, Pawn)
        DropMove pt to       -> (to, pt)
      oppAttackers = [ (sq, pt) | (sq, Piece p pt) <- Map.toList bd, p == opp, canPieceReachTarget sq (Piece opp pt) toSq bd ]
      nextBd = board (applyMove m st)
      friendlyDefenders = [ fromSq | (fromSq, Piece p _) <- Map.toList nextBd
                                   , p == plr, fromSq /= toSq
                                   , canPieceReachTarget fromSq (Piece plr ptMoved) toSq nextBd ]
  in if null oppAttackers
     then False
     else
       let minOppVal = minimum [ pieceValue oppPt | (_, oppPt) <- oppAttackers ]
           movedVal  = pieceValue ptMoved
       in if null friendlyDefenders
          then ptMoved /= Pawn
          else movedVal > minOppVal + 250.0

-- ============================================================================
-- 3. 中盤層化商空間 (Stratified Quotient Space) の数理モデル
-- ============================================================================

-- | A. 持ち駒層化 (Hand Stratum Invariant)
-- 持ち駒の枚数・質的バランス（大駒差、金銀差、急襲駒差、歩の所持段階、詰み持ち駒有無）
data HandStratumKey = HandStratumKey
  { hsMajorDiff      :: !Int          -- 飛角の持ち駒差 (Sente - Gote)
  , hsGoldSilverDiff :: !Int          -- 金銀の持ち駒差 (Sente - Gote)
  , hsMinorDiff      :: !Int          -- 桂香の持ち駒差 (Sente - Gote)
  , hsPawnTier       :: !(Int, Int)   -- (先手歩カテゴリ: 0枚, 1枚, 2枚+, 後手同)
  , hsHasMateKey     :: !(Bool, Bool) -- (先手が金1枚以上持つか, 後手同)
  } deriving (Eq, Ord, Show)

stratifyHand :: Map.Map Player Hand -> HandStratumKey
stratifyHand hnds =
  let sHand = fromMaybe emptyHand (Map.lookup Sente hnds)
      gHand = fromMaybe emptyHand (Map.lookup Gote hnds)
      getH h pt = Map.findWithDefault 0 pt h
      
      majorDiff = (getH sHand Rook + getH sHand Bishop) - (getH gHand Rook + getH gHand Bishop)
      gsDiff    = (getH sHand Gold + getH sHand Silver) - (getH gHand Gold + getH gHand Silver)
      minDiff   = (getH sHand Knight + getH sHand Lance) - (getH gHand Knight + getH gHand Lance)
      
      pTier cnt | cnt == 0  = 0
                | cnt == 1  = 1
                | otherwise = 2
      pawnTier = (pTier (getH sHand Pawn), pTier (getH gHand Pawn))
      mateKey  = (getH sHand Gold >= 1, getH gHand Gold >= 1)
  in HandStratumKey majorDiff gsDiff minDiff pawnTier mateKey

-- | B. 成り駒層化 (Promotion Stratum Invariant)
-- 龍・馬の制空権と、敵玉近傍への成駒侵入度
data PromotionStratumKey = PromotionStratumKey
  { psDragonStatus :: !(Bool, Bool) -- (先手龍の有無, 後手龍の有無)
  , psHorseStatus  :: !(Bool, Bool) -- (先手馬の有無, 後手馬の有無)
  , psInfiltration :: !(Int, Int)   -- (先手成駒の敵陣侵入度, 後手成駒の敵陣侵入度)
  , psTotalPromoted:: !(Int, Int)   -- (先手成駒数, 後手成駒数)
  } deriving (Eq, Ord, Show)

stratifyPromotion :: Board -> PromotionStratumKey
stratifyPromotion bd =
  let pieces = Map.toList bd
      isSentePromo (Piece Sente pt) = pt `elem` [PPawn, PLance, PKnight, PSilver, PBishop, PRook]
      isSentePromo _ = False
      isGotePromo (Piece Gote pt)   = pt `elem` [PPawn, PLance, PKnight, PSilver, PBishop, PRook]
      isGotePromo _ = False

      sPromos = [ (sq, pt) | (sq, Piece Sente pt) <- pieces, pt `elem` [PPawn, PLance, PKnight, PSilver, PBishop, PRook] ]
      gPromos = [ (sq, pt) | (sq, Piece Gote pt)   <- pieces, pt `elem` [PPawn, PLance, PKnight, PSilver, PBishop, PRook] ]

      hasDragon p = any (\(_, pt) -> pt == PRook) (if p == Sente then sPromos else gPromos)
      hasHorse p  = any (\(_, pt) -> pt == PBishop) (if p == Sente then sPromos else gPromos)

      -- 敵陣（先手は y <= 3, 後手は y >= 7）への成駒浸透数
      sInfil = length [ sq | (sq@(_, y), _) <- sPromos, y <= 3 ]
      gInfil = length [ sq | (sq@(_, y), _) <- gPromos, y >= 7 ]
  in PromotionStratumKey
       (hasDragon Sente, hasDragon Gote)
       (hasHorse Sente, hasHorse Gote)
       (sInfil, gInfil)
       (length sPromos, length gPromos)

-- | C. 打ち駒層化 (Drop Stratum Invariant)
-- 打ち駒の幾何学的役割と盤上の楔（くさび）拠点構造および桂打ちフォーク
data DropRole
  = WedgeOutpost     -- 敵玉頭・小鬢への楔打ち（垂れ歩、拠点銀、桂頭打ち）
  | KnightForkDrop   -- 桂打ちフォーク（ふんどしの桂、王手両取り・金銀大駒両取りの跳躍急襲）
  | RayFork          -- 割り打ち・両取り（角打ち、金銀フォーク）
  | RayInterposition -- 合い駒・防壁打ち（飛角香の射線遮断）
  | CheckDrop        -- 王手打ち
  | NormalDrop       -- 通常の駒打ち
  deriving (Eq, Ord, Show)

-- | 桂打ちフォークの厳密判定 (指定マス to に桂馬を打った場合に2枚以上の敵重要駒を捉えるか)
isKnightForkDrop :: State -> Square -> Bool
isKnightForkDrop st to@(tx, ty) =
  let plr = turn st
      opp = opponent plr
      dirY = if plr == Sente then -1 else 1
      tSq1 = (tx - 1, ty + 2 * dirY)
      tSq2 = (tx + 1, ty + 2 * dirY)
      bd = board st
      isHighValue (Piece p pt) = p == opp && pt `elem` [King, Rook, Bishop, Gold, Silver, PRook, PBishop]
      targets = [ p | sq <- [tSq1, tSq2], not (isOutOfBounds sq), Just p <- [Map.lookup sq bd], isHighValue p ]
  in length targets >= 2 || any (\(Piece _ pt) -> pt == King) targets && not (null targets)

classifyDropRole :: State -> Move -> DropRole
classifyDropRole st (DropMove pt to) =
  let plr = turn st
      opp = opponent plr
      oppKing = findKing opp (board st)
      nextSt = applyMove (DropMove pt to) st
  in if pt == Knight && isKnightForkDrop st to
     then KnightForkDrop
     else if isKingUnderAttack opp (board nextSt)
     then CheckDrop
     else case oppKing of
       Just (kx, ky) ->
         let dist = abs (fst to - kx) + abs (snd to - ky)
         in if dist <= 2
            then WedgeOutpost
            else if pt `elem` [Bishop, Knight, Silver] && length (getForkTargets plr to (board nextSt)) >= 2
                 then RayFork
                 else NormalDrop
       Nothing -> NormalDrop
  where
    getForkTargets p sq b =
      [ tSq | (tSq, Piece op _) <- Map.toList b, op == opponent p, canPieceReachTarget sq (Piece p pt) tSq b ]
classifyDropRole _ _ = NormalDrop

data DropStratumKey = DropStratumKey
  { dsWedgeCount     :: !(Int, Int)        -- (先手楔拠点数, 後手楔拠点数)
  , dsKnightForkPot  :: !(Int, Int)        -- (先手桂打ちフォーク数, 後手桂打ちフォーク数)
  , dsRecentDropRole :: !(Maybe DropRole)  -- 直近の指し手が打ち駒である場合の機能分類
  } deriving (Eq, Ord, Show)

stratifyDrop :: State -> DropStratumKey
stratifyDrop st =
  let bd = board st
      sKing = findKing Sente bd
      gKing = findKing Gote bd
      -- 先手の楔（後手玉から距離2以内の先手駒）
      sWedges = case gKing of
        Just (kx, ky) -> length [ sq | (sq, Piece Sente _) <- Map.toList bd, abs (fst sq - kx) + abs (snd sq - ky) <= 2 ]
        Nothing -> 0
      gWedges = case sKing of
        Just (kx, ky) -> length [ sq | (sq, Piece Gote _) <- Map.toList bd, abs (fst sq - kx) + abs (snd sq - ky) <= 2 ]
        Nothing -> 0

      -- 桂打ちフォークの潜在可能数
      countForks plr =
        let myHand = fromMaybe emptyHand (Map.lookup plr (hands st))
            hasKnight = Map.findWithDefault 0 Knight myHand > 0
            emptySqs = [ (x, y) | x <- [1..9], y <- (if plr == Sente then [3..9] else [1..7]), not (Map.member (x, y) bd) ]
            tempSt = st { turn = plr }
        in if hasKnight
           then length (filter (isKnightForkDrop tempSt) emptySqs)
           else 0
      
      sForks = countForks Sente
      gForks = countForks Gote
      
      recentRole = case moveHistory st of
        [] -> Nothing
        (lastMvStr:_) ->
          if "N*" `isPrefixOf` lastMvStr
          then Just KnightForkDrop
          else if '*' `elem` lastMvStr
          then Just WedgeOutpost
          else Nothing
  in DropStratumKey (sWedges, gWedges) (sForks, gForks) recentRole

-- | D. 中盤詰めろイデアル (Midgame Tsumero Ideals: MTI)
-- 次の一手でQEGTB即詰み（DTM 1 または 3）をもたらす網羅的攻防手筋
data TsumeroType
  = HeadSupportTsumero    -- 玉頭紐付け詰めろ (飛角銀歩桂の利きを敵玉頭マスに創出)
  | SpaceClearanceTsumero -- 空間創出詰めろ (邪魔駒を前進・成る・捨てて金打ちスペース確保)
  | TarekomaPawnTsumero   -- 垂れ歩詰めろ (敵玉頭2段前に歩を垂らし「と金」即詰みを狙う)
  | DragonEncroachTsumero -- 龍のすり寄り詰めろ (龍を玉近傍に寄せて頭飛び込み・横すり寄りを狙う)
  | IkkenRyuSetupTsumero  -- 一間竜セットアップ詰めろ (龍を1マス空きに据えて中間銀打ちを狙う)
  | HorseAnchorTsumero    -- 馬の引き付け・斜線封鎖詰めろ (馬で脱出路を消して頭金を狙う)
  | CrossRookSetupTsumero -- 十字飛車セットアップ詰めろ (縦横直交ラインで封鎖を完成)
  | TsurushiSetupTsumero  -- 吊るし桂セットアップ詰めろ (前面壁玉に対して桂跳躍を狙う)
  | KnightForkTsumero     -- 桂打ちフォーク詰めろ (玉王手と守備駒取りの二重威嚇)
  | SilverForkTsumero     -- 割り打ちの銀詰めろ (玉と金、2枚の金の間に割り込む)
  | BellySilverTsumero    -- 腹銀・尻銀詰めろ (玉の横や斜め後ろに銀を密着)
  | TatatakiPawnTsumero   -- 叩きの歩詰めろ (玉頭や守備金頭に歩を叩いて崩す)
  | FocalSacrificeTsumero -- 焦点の捨て駒詰めろ (敵の利きの交点に放り込む)
  | GuardStrippingTsumero -- 守備金銀剥がし詰めろ (玉を守る金銀を吊り上げ/捕獲)
  | EdgeHerdingTsumero    -- 端玉追い込み詰めろ (端歩・香で玉を端へ追い詰める)
  | InterferenceTsumero   -- 邪魔駒打ち込み詰めろ (唯一の逃げ道に駒を打ち込んで退路塞ぎ)
  | DoubleTsumero         -- 両詰めろ (2方向から同時にかかり受ける手がない絶対詰めろ)
  | EscapeCounterTsumero  -- 詰めろ逃れの詰めろ (自玉の脅威を回避しつつ相手に詰めろ)
  | PincerBrinkmateTsumero-- 挟撃必至前段詰めろ (次の一手で受けなし必至を完成)
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | 局面において、攻め手が「詰めろ（相手が受けなければ次手QEGTB即詰み）」をかけているかを幾何学的に判定
detectMidgameTsumero :: Player -> State -> DynamicQEGTB -> Maybe (TsumeroType, Move)
detectMidgameTsumero att st qegtb =
  let def = opponent att
      bd = board st
      maybeDefK = findKing def bd
  in case maybeDefK of
       Nothing -> Nothing
       Just kSq@(kx, ky) ->
         -- 相手がパス（手番スキップ）したと仮定した局面で、自分が即座にQEGTB即詰みを出せるか照合
         let stHypotheticalMyTurn = st { turn = att }
         in case matchQEGTB stHypotheticalMyTurn qegtb of
              Just (_, mateMv) ->
                -- 詰めろ成立！その幾何学的パターンを手筋ごとに分類同定
                let myHands = fromMaybe emptyHand (Map.lookup att (hands st))
                    dirY = if att == Sente then 1 else -1
                    headSq = (kx, ky + dirY)
                    
                    tsumeroClass
                      | Map.findWithDefault 0 Gold myHands >= 1 && isSquareUnderAttack att headSq bd
                        = HeadSupportTsumero
                      | any (\(_, Piece p pt) -> p == att && pt == PRook) (Map.toList bd) && abs (kx - 5) <= 2
                        = DragonEncroachTsumero
                      | any (\(_, Piece p pt) -> p == att && pt == PBishop) (Map.toList bd)
                        = HorseAnchorTsumero
                      | Map.findWithDefault 0 Knight myHands >= 1 && isKnightForkDrop st (kx, ky + 2 * dirY)
                        = KnightForkTsumero
                      | Map.findWithDefault 0 Pawn myHands >= 1 && not (Map.member (kx, ky + 2 * dirY) bd)
                        = TarekomaPawnTsumero
                      | Map.findWithDefault 0 Silver myHands >= 1
                        = SilverForkTsumero
                      | Map.findWithDefault 0 Gold myHands >= 2
                        = PincerBrinkmateTsumero
                      | otherwise
                        = SpaceClearanceTsumero
                in Just (tsumeroClass, mateMv)
              Nothing -> Nothing

-- | 序盤多重詰めろイデアル (Opening Multiple-Tsumero Ideals: OMI)
-- 序盤（手数 <= 35）において、1手で2つ以上の独立した即詰み筋（または受ける手がない両詰めろ）を生じさせる決定打
data OpeningMultiTsumeroType
  -- 1. 大駒斜線放射型 (Bifurcated Bishop Ray)
  = Yokofu45BishopMultiTsumero      -- 横歩取り△4五角型 (飛車取り＋玉頭急襲＋2方向即詰み)
  | SujichigaiBishopMultiTsumero    -- 筋違い角型 (コビン直撃＋中央頭金両詰めろ)
  | EarlySilverBishopMultiTsumero   -- 角換わり早繰り銀・遠見角型 (端攻め香連動＋前面金打ち両詰めろ)
  | PacmanBishopMultiTsumero        -- パックマン中央角型 (中央制圧＋左右挟撃両詰めろ)
  -- 2. 飛車直交突破型 (Orthogonal Rook Penetration)
  | AigakariClimbingRookMultiTsumero-- 相掛かり棒銀突破・龍侵入型 (玉頭突入＋下段横すり寄り両詰めろ)
  | SodebishaDragonMultiTsumero     -- 袖飛車・早仕掛け龍直撃型 (頭金＋一間竜両詰めろ)
  | NakabishaCentralRookMultiTsumero-- 原始中飛車中央直通龍型 (5筋直通＋左右金打ち両詰めろ)
  | MigiShikenCrossRookMultiTsumero -- 右四間腰掛け銀・十字飛車型 (コビン・横腹直通＋銀桂両詰めろ)
  -- 3. 桂馬奇襲跳躍型 (Leaping Knight Double-Strike)
  | DemonKillerKnightMultiTsumero   -- 鬼殺し桂跳躍型 (玉頭爆破＋端香取り両詰めろ)
  | UreshinoKnightForkMultiTsumero  -- 嬉野流・鳥刺し急戦桂打ち型 (玉・金銀両睨み＋脱出路封鎖両詰めろ)
  | ElmoCrushKnightMultiTsumero     -- 右四間対エルモ/舟囲い4五桂型 (金頭成り捨て＋角頭直撃両詰めろ)
  -- 4. 歩の手筋・クサビ・焦点型 (Wedge & Sacrificial Pawn Synergy)
  | DoubleTatakiPawnMultiTsumero    -- 序盤連打・叩き歩玉浮かせ型 (玉浮かせ＋左右挟み撃ち両詰めろ)
  | DoubleTarekomaPawnMultiTsumero  -- 継ぎ歩＋垂れ歩ダブルと金型 (2筋・4筋ダブル即詰み)
  | FocalPawnSacrificeMultiTsumero  -- 焦点の歩捨て型 (交点爆破＋守備壁穴あけ両詰めろ)
  -- 5. 大駒切り捨て強襲型 (Major Piece Sacrifice Decisive)
  | EarlyRookSacrificeMultiTsumero  -- 序盤飛車切り捨て型 (退路ゼロ化＋左右金打ち両詰めろ)
  | DiscoveredBishopBatteryMultiTsumero -- 角切り空き王手バッテリー型 (直射ライン開放＋別マス即詰み両詰めろ)
  -- 6. 囲い欠陥直撃型 (Structural Castle Flaw Exploitation)
  | SittingKingKobinMultiTsumero    -- 居玉コビン割り型 (初期玉弱点直撃＋左右挟撃両詰めろ)
  | WallPieceTrapMultiTsumero       -- 壁銀・壁桂誘発型 (退路自滅＋前面・逆サイド両詰めろ)
  | FunagakoiFlawMultiTsumero       -- 舟囲いコビン崩し型 (角桂連動＋上下退路遮断両詰めろ)
  | AnagumaDefectMultiTsumero       -- 穴熊未完成急襲型 (端攻め＋中央直撃両詰めろ)
  -- 7. 序盤超局所イデアル
  | UchifudumeEscapeMultiTsumero    -- 打ち歩詰め回避不能・禁手誘発型両詰めろ
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | 序盤における複数の詰めろ（両詰めろ・多重即詰み筋）が生じる手を検出
detectOpeningMultiTsumero :: Player -> State -> DynamicQEGTB -> Maybe (OpeningMultiTsumeroType, [Move])
detectOpeningMultiTsumero att st qegtb =
  let ply = length (moveHistory st)
  in if ply > 35 -- 序盤の境界条件
     then Nothing
     else
       let def = opponent att
           bd = board st
           maybeDefK = findKing def bd
       in case maybeDefK of
            Nothing -> Nothing
            Just kSq@(kx, ky) ->
              let stHypo = st { turn = att }
                  legalMoves = f5SyzygyFilter stHypo (generateMoves stHypo)
                  mateMoves = [ mv
                              | mv <- legalMoves
                              , let nextSt = applyMove mv stHypo
                              , isCheckmatePosition nextSt || isJust (matchQEGTB nextSt qegtb)
                              ]
              in if length mateMoves >= 2
                 then
                   let myHands = fromMaybe emptyHand (Map.lookup att (hands st))
                       hasDropB = Map.findWithDefault 0 Bishop myHands > 0
                       hasDropN = Map.findWithDefault 0 Knight myHands > 0
                       hasDropR = Map.findWithDefault 0 Rook myHands > 0
                       hasPRook = any (\(_, Piece p pt) -> p == att && pt == PRook) (Map.toList bd)
                       isOppSitting = kx == 5 && (ky == 1 || ky == 9)
                       
                       multiClass
                         | hasDropB && (kx >= 4 && kx <= 6) = Yokofu45BishopMultiTsumero
                         | hasDropN = DemonKillerKnightMultiTsumero
                         | hasPRook = AigakariClimbingRookMultiTsumero
                         | isOppSitting = SittingKingKobinMultiTsumero
                         | hasDropR = EarlyRookSacrificeMultiTsumero
                         | otherwise = DoubleTatakiPawnMultiTsumero
                   in Just (multiClass, mateMoves)
                 else Nothing

-- | E. 統合中盤層化商空間 (Midgame Stratum)
data MidgameStratum = MidgameStratum
  { msTurn      :: !Player
  , msHand      :: !HandStratumKey
  , msPromotion :: !PromotionStratumKey
  , msDrop      :: !DropStratumKey
  , msTsumero   :: !(Maybe TsumeroType) -- 中盤詰めろイデアルの種類
  , msMorseTier :: !Int -- -2: 後手優勢, -1: 後手微利, 0: 互角サドル点, +1: 先手微利, +2: 先手優勢
  } deriving (Eq, Ord, Show)

stratifyState :: State -> MidgameStratum
stratifyState st =
  let hKey = stratifyHand (hands st)
      pKey = stratifyPromotion (board st)
      dKey = stratifyDrop st
      tsumeroOpt = fmap fst (detectMidgameTsumero (turn st) st initSeedQEGTB)
      eval = evaluateStratifiedState Sente st
      tier | eval >= 300.0  = 2
           | eval >= 100.0  = 1
           | eval <= -300.0 = -2
           | eval <= -100.0 = -1
           | otherwise      = 0
  in MidgameStratum (turn st) hKey pKey dKey tsumeroOpt tier

showStratumKey :: MidgameStratum -> String
showStratumKey MidgameStratum{..} =
  printf "Stratum[T:%s|MajDiff:%+d|GS:%+d|Pawn:(%d,%d)|Drag:(%d,%d)|Wedge:(%d,%d)|Fork:(%d,%d)|Tsumero:%s|Tier:%+d]"
    (show msTurn)
    (hsMajorDiff msHand)
    (hsGoldSilverDiff msHand)
    (fst (hsPawnTier msHand)) (snd (hsPawnTier msHand))
    (if fst (psDragonStatus msPromotion) then (1::Int) else 0)
    (if snd (psDragonStatus msPromotion) then (1::Int) else 0)
    (fst (dsWedgeCount msDrop)) (snd (dsWedgeCount msDrop))
    (fst (dsKnightForkPot msDrop)) (snd (dsKnightForkPot msDrop))
    (case msTsumero of Just t -> show t ; Nothing -> "None")
    msMorseTier

-- ============================================================================
-- 4. 中盤層化モースポテンシャル評価関数
-- ============================================================================

pieceValue :: PieceType -> Double
pieceValue Pawn    = 100.0
pieceValue Lance   = 300.0
pieceValue Knight  = 350.0
pieceValue Silver  = 500.0
pieceValue Gold    = 600.0
pieceValue Bishop  = 850.0
pieceValue Rook    = 1000.0
pieceValue King    = 15000.0
pieceValue PPawn   = 550.0
pieceValue PLance  = 500.0
pieceValue PKnight = 500.0
pieceValue PSilver = 550.0
pieceValue PBishop = 1150.0
pieceValue PRook   = 1350.0

evaluateStratifiedState :: Player -> State -> Double
evaluateStratifiedState plr st =
  let bd = board st
      hnds = hands st
      
      -- 1. 盤上駒素材スコア
      boardScore (Piece p pt) = (if p == plr then 1.0 else -1.0) * pieceValue pt
      bdTotal = sum [ boardScore piece | (_, piece) <- Map.toList bd ]
      
      -- 2. 持ち駒スコア
      myHand = fromMaybe emptyHand (Map.lookup plr hnds)
      enemyHand = fromMaybe emptyHand (Map.lookup (opponent plr) hnds)
      myHandTotal = sum [ fromIntegral cnt * pieceValue pt | (pt, cnt) <- Map.toList myHand ]
      enemyHandTotal = sum [ fromIntegral cnt * pieceValue pt | (pt, cnt) <- Map.toList enemyHand ]
      
      -- 3. 成駒浸透度ボーナス (Promotion Infiltration)
      myKingPos = findKing plr bd
      oppKingPos = findKing (opponent plr) bd
      
      promoBonus (Piece p pt) (Just (kx, ky)) =
        if pt `elem` [PPawn, PLance, PKnight, PSilver, PBishop, PRook]
        then if p == plr
             then 60.0 / (fromIntegral (abs (kx - 5) + abs (ky - 5)) + 1.0)
             else -60.0
        else 0.0
      promoBonus _ _ = 0.0
      totalPromoBonus = sum [ promoBonus p oppKingPos | (_, p) <- Map.toList bd ]
      
      -- 4. 打ち駒・楔拠点スコア (Drop Outposts)
      wedgeBonus (Just (okx, oky)) =
        let nearbyFriendly = [ sq | (sq, Piece p _) <- Map.toList bd, p == plr, abs (fst sq - okx) + abs (snd sq - oky) <= 2 ]
        in fromIntegral (length nearbyFriendly) * 35.0
      wedgeBonus Nothing = 0.0

      -- 4.5 桂打ちフォーク急襲ボーナス (Knight Fork Tactical Threat)
      dKey = stratifyDrop st
      (myForks, oppForks) = if plr == Sente
                            then (fst (dsKnightForkPot dKey), snd (dsKnightForkPot dKey))
                            else (snd (dsKnightForkPot dKey), fst (dsKnightForkPot dKey))
      knightForkBonus = fromIntegral (myForks - oppForks) * 160.0
      
      -- 直近の手が桂打ちフォークである場合の強烈な即時インセンティブ
      recentForkBonus = case dsRecentDropRole dKey of
        Just KnightForkDrop -> if turn st /= plr then 420.0 else -420.0
        _                   -> 0.0

      -- 4.8 中盤詰めろイデアルボーナス (Midgame Tsumero Advantage)
      myTsumero = detectMidgameTsumero plr st initSeedQEGTB
      oppTsumero = detectMidgameTsumero (opponent plr) st initSeedQEGTB
      
      tsumeroScore (Just (tType, _)) = case tType of
        EscapeCounterTsumero   -> 1500.0
        DoubleTsumero          -> 1200.0
        PincerBrinkmateTsumero -> 1100.0
        HeadSupportTsumero     -> 850.0
        DragonEncroachTsumero  -> 850.0
        KnightForkTsumero      -> 800.0
        TarekomaPawnTsumero    -> 750.0
        _                      -> 700.0
      tsumeroScore Nothing = 0.0
      
      tsumeroNetBonus = tsumeroScore myTsumero - tsumeroScore oppTsumero
      
      -- 4.9 序盤多重詰めろイデアルボーナス (Opening Multiple-Tsumero Bonus)
      myOpeningMulti = detectOpeningMultiTsumero plr st initSeedQEGTB
      oppOpeningMulti = detectOpeningMultiTsumero (opponent plr) st initSeedQEGTB
      openingMultiScore (Just _) = 2000.0
      openingMultiScore Nothing  = 0.0
      openingMultiBonus = openingMultiScore myOpeningMulti - openingMultiScore oppOpeningMulti
      
      -- 5. モース玉危険度ポテンシャル
      morsePot (Just (kx, ky)) pTarget =
        let enemies = Map.toList $ Map.filter (\(Piece p _) -> p /= pTarget) bd
        in sum [ 10.0 / (fromIntegral (abs (kx - sx) + abs (ky - sy)) + 0.1) | ((sx, sy), _) <- enemies ]
      morsePot Nothing _ = 10000.0
      morseDiff = (morsePot oppKingPos (opponent plr) - morsePot myKingPos plr) * 6.0
  in bdTotal + myHandTotal - enemyHandTotal + totalPromoBonus + wedgeBonus oppKingPos + knightForkBonus + recentForkBonus + tsumeroNetBonus + openingMultiBonus + morseDiff

isMorseEquilibrium :: State -> Bool
isMorseEquilibrium st = abs (evaluateStratifiedState Sente st) <= 250.0

-- ============================================================================
-- 5. 商空間エンドゲームテーブル (QEGTB) ＆ 詰みバックプロパゲーション
-- ============================================================================

data DynamicQEGTBEntry = DynamicQEGTBEntry
  { dqeId           :: !String
  , dqeKingSq       :: !Square
  , dqeAttacker     :: !Player
  , dqeDTM          :: !Int
  , dqeBestMove     :: !Move
  , dqeRequiredHand :: ![(PieceType, Int)]
  } deriving (Show, Eq)

type DynamicQEGTB = [DynamicQEGTBEntry]

-- ============================================================================
-- 5. 512階のメタイデアル体系 (512th-Order Stratified Meta-Ideals)
-- 8つの主要層化フェーズ × 64階層 = 512階の完全直交商空間アトラクター
-- ============================================================================

initMetaIdeals512 :: DynamicQEGTB
initMetaIdeals512 = concat
  [ phase1_TerminalMates        -- 第   1〜 64 階: 即詰み・必至メタイデアル
  , phase2_MidgameTsumeros      -- 第  65〜128 階: 中盤詰めろ・空間切断メタイデアル
  , phase3_PiecePenetration     -- 第 129〜192 階: 大駒浸透・竜馬連携メタイデアル
  , phase4_TacticalForks        -- 第 193〜256 階: 桂香フォーク・串刺しメタイデアル
  , phase5_GuardStripping       -- 第 257〜320 階: 守備駒剥がし・露出玉メタイデアル
  , phase6_CastleDestruction    -- 第 321〜384 階: 囲い崩壊・急所爆破メタイデアル
  , phase7_MaterialSupremacy    -- 第 385〜448 階: 駒得累積・手番支配メタイデアル
  , phase8_AsymptoticSinks      -- 第 449〜512 階: 漸近的不変モース勾配流シンク
  ]

phase1_TerminalMates :: [DynamicQEGTBEntry]
phase1_TerminalMates =
  [ DynamicQEGTBEntry "MetaIdeal_001_AtamaKin_51" (5, 1) Sente 1 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_002_ShiriKin_51" (5, 1) Sente 1 (DropMove Gold (5, 1)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_003_BellySilver_51" (5, 1) Sente 1 (DropMove Silver (4, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_004_IkkenRyu_51" (5, 1) Sente 3 (DropMove Silver (5, 2)) [(Silver, 1), (Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_005_TsurushiKei_51" (5, 1) Sente 1 (DropMove Knight (4, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_006_BishopDive_51" (5, 1) Sente 1 (DropMove Bishop (6, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_007_TatatakiPawn_51" (5, 1) Sente 1 (DropMove Pawn (5, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_008_AtamaKin_41" (4, 1) Sente 1 (DropMove Gold (4, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_009_ShiriKin_41" (4, 1) Sente 1 (DropMove Gold (4, 1)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_010_BellySilver_41" (4, 1) Sente 1 (DropMove Silver (3, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_011_IkkenRyu_41" (4, 1) Sente 3 (DropMove Silver (4, 2)) [(Silver, 1), (Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_012_TsurushiKei_41" (4, 1) Sente 1 (DropMove Knight (3, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_013_BishopDive_41" (4, 1) Sente 1 (DropMove Bishop (5, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_014_TatatakiPawn_41" (4, 1) Sente 1 (DropMove Pawn (4, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_015_AtamaKin_61" (6, 1) Sente 1 (DropMove Gold (6, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_016_ShiriKin_61" (6, 1) Sente 1 (DropMove Gold (6, 1)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_017_BellySilver_61" (6, 1) Sente 1 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_018_IkkenRyu_61" (6, 1) Sente 3 (DropMove Silver (6, 2)) [(Silver, 1), (Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_019_TsurushiKei_61" (6, 1) Sente 1 (DropMove Knight (5, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_020_BishopDive_61" (6, 1) Sente 1 (DropMove Bishop (7, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_021_TatatakiPawn_61" (6, 1) Sente 1 (DropMove Pawn (6, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_022_AtamaKin_31" (3, 1) Sente 1 (DropMove Gold (3, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_023_ShiriKin_31" (3, 1) Sente 1 (DropMove Gold (3, 1)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_024_BellySilver_31" (3, 1) Sente 1 (DropMove Silver (2, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_025_IkkenRyu_31" (3, 1) Sente 3 (DropMove Silver (3, 2)) [(Silver, 1), (Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_026_TsurushiKei_31" (3, 1) Sente 1 (DropMove Knight (2, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_027_BishopDive_31" (3, 1) Sente 1 (DropMove Bishop (4, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_028_TatatakiPawn_31" (3, 1) Sente 1 (DropMove Pawn (3, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_029_AtamaKin_71" (7, 1) Sente 1 (DropMove Gold (7, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_030_ShiriKin_71" (7, 1) Sente 1 (DropMove Gold (7, 1)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_031_BellySilver_71" (7, 1) Sente 1 (DropMove Silver (6, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_032_IkkenRyu_71" (7, 1) Sente 3 (DropMove Silver (7, 2)) [(Silver, 1), (Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_033_TsurushiKei_71" (7, 1) Sente 1 (DropMove Knight (6, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_034_BishopDive_71" (7, 1) Sente 1 (DropMove Bishop (8, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_035_TatatakiPawn_71" (7, 1) Sente 1 (DropMove Pawn (7, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_036_AtamaKin_21" (2, 1) Sente 1 (DropMove Gold (2, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_037_ShiriKin_21" (2, 1) Sente 1 (DropMove Gold (2, 1)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_038_BellySilver_21" (2, 1) Sente 1 (DropMove Silver (1, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_039_IkkenRyu_21" (2, 1) Sente 3 (DropMove Silver (2, 2)) [(Silver, 1), (Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_040_TsurushiKei_21" (2, 1) Sente 1 (DropMove Knight (1, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_041_BishopDive_21" (2, 1) Sente 1 (DropMove Bishop (3, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_042_TatatakiPawn_21" (2, 1) Sente 1 (DropMove Pawn (2, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_043_AtamaKin_81" (8, 1) Sente 1 (DropMove Gold (8, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_044_ShiriKin_81" (8, 1) Sente 1 (DropMove Gold (8, 1)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_045_BellySilver_81" (8, 1) Sente 1 (DropMove Silver (7, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_046_IkkenRyu_81" (8, 1) Sente 3 (DropMove Silver (8, 2)) [(Silver, 1), (Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_047_TsurushiKei_81" (8, 1) Sente 1 (DropMove Knight (7, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_048_BishopDive_81" (8, 1) Sente 1 (DropMove Bishop (9, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_049_TatatakiPawn_81" (8, 1) Sente 1 (DropMove Pawn (8, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_050_AtamaKin_11" (1, 1) Sente 1 (DropMove Gold (1, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_051_ShiriKin_11" (1, 1) Sente 1 (DropMove Gold (1, 1)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_052_BellySilver_11" (1, 1) Sente 1 (DropMove Silver (1, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_053_IkkenRyu_11" (1, 1) Sente 3 (DropMove Silver (1, 2)) [(Silver, 1), (Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_054_TsurushiKei_11" (1, 1) Sente 1 (DropMove Knight (2, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_055_BishopDive_11" (1, 1) Sente 1 (DropMove Bishop (2, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_056_TatatakiPawn_11" (1, 1) Sente 1 (DropMove Pawn (1, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_057_AtamaKin_91" (9, 1) Sente 1 (DropMove Gold (9, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_058_ShiriKin_91" (9, 1) Sente 1 (DropMove Gold (9, 1)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_059_BellySilver_91" (9, 1) Sente 1 (DropMove Silver (8, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_060_IkkenRyu_91" (9, 1) Sente 3 (DropMove Silver (9, 2)) [(Silver, 1), (Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_061_TsurushiKei_91" (9, 1) Sente 1 (DropMove Knight (8, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_062_BishopDive_91" (9, 1) Sente 1 (DropMove Bishop (8, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_063_TatatakiPawn_91" (9, 1) Sente 1 (DropMove Pawn (9, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_064_MiyakoCenter_55" (5, 5) Sente 1 (DropMove Gold (5, 4)) [(Gold, 1)]
  ]

phase2_MidgameTsumeros :: [DynamicQEGTBEntry]
phase2_MidgameTsumeros =
  [ DynamicQEGTBEntry "MetaIdeal_065_HeadSupportTsumero_51" (5, 1) Sente 3 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_066_TarekomaPawnTsumero_51" (5, 1) Sente 3 (DropMove Pawn (5, 3)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_067_DragonEncroach_51" (5, 1) Sente 3 (DropMove Rook (5, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_068_HorseAnchor_51" (5, 1) Sente 3 (DropMove Bishop (5, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_069_PincerBrinkmate_51" (5, 1) Sente 3 (DropMove Gold (4, 2)) [(Gold, 2)]
  , DynamicQEGTBEntry "MetaIdeal_070_KnightForkTsumero_51" (5, 1) Sente 3 (DropMove Knight (4, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_071_SilverForkTsumero_51" (5, 1) Sente 3 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_072_HeadSupportTsumero_41" (4, 1) Sente 3 (DropMove Gold (4, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_073_TarekomaPawnTsumero_41" (4, 1) Sente 3 (DropMove Pawn (4, 3)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_074_DragonEncroach_41" (4, 1) Sente 3 (DropMove Rook (4, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_075_HorseAnchor_41" (4, 1) Sente 3 (DropMove Bishop (4, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_076_PincerBrinkmate_41" (4, 1) Sente 3 (DropMove Gold (3, 2)) [(Gold, 2)]
  , DynamicQEGTBEntry "MetaIdeal_077_KnightForkTsumero_41" (4, 1) Sente 3 (DropMove Knight (3, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_078_SilverForkTsumero_41" (4, 1) Sente 3 (DropMove Silver (4, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_079_HeadSupportTsumero_61" (6, 1) Sente 3 (DropMove Gold (6, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_080_TarekomaPawnTsumero_61" (6, 1) Sente 3 (DropMove Pawn (6, 3)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_081_DragonEncroach_61" (6, 1) Sente 3 (DropMove Rook (6, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_082_HorseAnchor_61" (6, 1) Sente 3 (DropMove Bishop (6, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_083_PincerBrinkmate_61" (6, 1) Sente 3 (DropMove Gold (5, 2)) [(Gold, 2)]
  , DynamicQEGTBEntry "MetaIdeal_084_KnightForkTsumero_61" (6, 1) Sente 3 (DropMove Knight (5, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_085_SilverForkTsumero_61" (6, 1) Sente 3 (DropMove Silver (6, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_086_HeadSupportTsumero_31" (3, 1) Sente 3 (DropMove Gold (3, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_087_TarekomaPawnTsumero_31" (3, 1) Sente 3 (DropMove Pawn (3, 3)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_088_DragonEncroach_31" (3, 1) Sente 3 (DropMove Rook (3, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_089_HorseAnchor_31" (3, 1) Sente 3 (DropMove Bishop (3, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_090_PincerBrinkmate_31" (3, 1) Sente 3 (DropMove Gold (2, 2)) [(Gold, 2)]
  , DynamicQEGTBEntry "MetaIdeal_091_KnightForkTsumero_31" (3, 1) Sente 3 (DropMove Knight (2, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_092_SilverForkTsumero_31" (3, 1) Sente 3 (DropMove Silver (3, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_093_HeadSupportTsumero_71" (7, 1) Sente 3 (DropMove Gold (7, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_094_TarekomaPawnTsumero_71" (7, 1) Sente 3 (DropMove Pawn (7, 3)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_095_DragonEncroach_71" (7, 1) Sente 3 (DropMove Rook (7, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_096_HorseAnchor_71" (7, 1) Sente 3 (DropMove Bishop (7, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_097_PincerBrinkmate_71" (7, 1) Sente 3 (DropMove Gold (6, 2)) [(Gold, 2)]
  , DynamicQEGTBEntry "MetaIdeal_098_KnightForkTsumero_71" (7, 1) Sente 3 (DropMove Knight (6, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_099_SilverForkTsumero_71" (7, 1) Sente 3 (DropMove Silver (7, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_100_HeadSupportTsumero_21" (2, 1) Sente 3 (DropMove Gold (2, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_101_TarekomaPawnTsumero_21" (2, 1) Sente 3 (DropMove Pawn (2, 3)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_102_DragonEncroach_21" (2, 1) Sente 3 (DropMove Rook (2, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_103_HorseAnchor_21" (2, 1) Sente 3 (DropMove Bishop (2, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_104_PincerBrinkmate_21" (2, 1) Sente 3 (DropMove Gold (1, 2)) [(Gold, 2)]
  , DynamicQEGTBEntry "MetaIdeal_105_KnightForkTsumero_21" (2, 1) Sente 3 (DropMove Knight (1, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_106_SilverForkTsumero_21" (2, 1) Sente 3 (DropMove Silver (2, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_107_HeadSupportTsumero_81" (8, 1) Sente 3 (DropMove Gold (8, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_108_TarekomaPawnTsumero_81" (8, 1) Sente 3 (DropMove Pawn (8, 3)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_109_DragonEncroach_81" (8, 1) Sente 3 (DropMove Rook (8, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_110_HorseAnchor_81" (8, 1) Sente 3 (DropMove Bishop (8, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_111_PincerBrinkmate_81" (8, 1) Sente 3 (DropMove Gold (7, 2)) [(Gold, 2)]
  , DynamicQEGTBEntry "MetaIdeal_112_KnightForkTsumero_81" (8, 1) Sente 3 (DropMove Knight (7, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_113_SilverForkTsumero_81" (8, 1) Sente 3 (DropMove Silver (8, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_114_HeadSupportTsumero_11" (1, 1) Sente 3 (DropMove Gold (1, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_115_TarekomaPawnTsumero_11" (1, 1) Sente 3 (DropMove Pawn (1, 3)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_116_DragonEncroach_11" (1, 1) Sente 3 (DropMove Rook (1, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_117_HorseAnchor_11" (1, 1) Sente 3 (DropMove Bishop (1, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_118_PincerBrinkmate_11" (1, 1) Sente 3 (DropMove Gold (2, 2)) [(Gold, 2)]
  , DynamicQEGTBEntry "MetaIdeal_119_KnightForkTsumero_11" (1, 1) Sente 3 (DropMove Knight (2, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_120_SilverForkTsumero_11" (1, 1) Sente 3 (DropMove Silver (1, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_121_HeadSupportTsumero_91" (9, 1) Sente 3 (DropMove Gold (9, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_122_TarekomaPawnTsumero_91" (9, 1) Sente 3 (DropMove Pawn (9, 3)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_123_DragonEncroach_91" (9, 1) Sente 3 (DropMove Rook (9, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_124_HorseAnchor_91" (9, 1) Sente 3 (DropMove Bishop (9, 2)) [(Bishop, 1)]
  , DynamicQEGTBEntry "MetaIdeal_125_PincerBrinkmate_91" (9, 1) Sente 3 (DropMove Gold (8, 2)) [(Gold, 2)]
  , DynamicQEGTBEntry "MetaIdeal_126_KnightForkTsumero_91" (9, 1) Sente 3 (DropMove Knight (8, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_127_SilverForkTsumero_91" (9, 1) Sente 3 (DropMove Silver (9, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_128_EscapeCounter_51" (5, 1) Sente 3 (DropMove Gold (5, 2)) [(Gold, 1)]
  ]

phase3_PiecePenetration :: [DynamicQEGTBEntry]
phase3_PiecePenetration =
  [ DynamicQEGTBEntry "MetaIdeal_129_DragonInfiltration_11" (1, 1) Sente 5 (DropMove Rook (1, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_130_DragonInfiltration_12" (1, 2) Sente 5 (DropMove Rook (1, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_131_DragonInfiltration_13" (1, 3) Sente 5 (DropMove Rook (1, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_132_DragonInfiltration_14" (1, 4) Sente 5 (DropMove Rook (1, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_133_DragonInfiltration_15" (1, 5) Sente 5 (DropMove Rook (1, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_134_DragonInfiltration_16" (1, 6) Sente 5 (DropMove Rook (1, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_135_DragonInfiltration_17" (1, 7) Sente 5 (DropMove Rook (1, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_136_DragonInfiltration_21" (2, 1) Sente 5 (DropMove Rook (2, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_137_DragonInfiltration_22" (2, 2) Sente 5 (DropMove Rook (2, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_138_DragonInfiltration_23" (2, 3) Sente 5 (DropMove Rook (2, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_139_DragonInfiltration_24" (2, 4) Sente 5 (DropMove Rook (2, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_140_DragonInfiltration_25" (2, 5) Sente 5 (DropMove Rook (2, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_141_DragonInfiltration_26" (2, 6) Sente 5 (DropMove Rook (2, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_142_DragonInfiltration_27" (2, 7) Sente 5 (DropMove Rook (2, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_143_DragonInfiltration_31" (3, 1) Sente 5 (DropMove Rook (3, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_144_DragonInfiltration_32" (3, 2) Sente 5 (DropMove Rook (3, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_145_DragonInfiltration_33" (3, 3) Sente 5 (DropMove Rook (3, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_146_DragonInfiltration_34" (3, 4) Sente 5 (DropMove Rook (3, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_147_DragonInfiltration_35" (3, 5) Sente 5 (DropMove Rook (3, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_148_DragonInfiltration_36" (3, 6) Sente 5 (DropMove Rook (3, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_149_DragonInfiltration_37" (3, 7) Sente 5 (DropMove Rook (3, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_150_DragonInfiltration_41" (4, 1) Sente 5 (DropMove Rook (4, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_151_DragonInfiltration_42" (4, 2) Sente 5 (DropMove Rook (4, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_152_DragonInfiltration_43" (4, 3) Sente 5 (DropMove Rook (4, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_153_DragonInfiltration_44" (4, 4) Sente 5 (DropMove Rook (4, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_154_DragonInfiltration_45" (4, 5) Sente 5 (DropMove Rook (4, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_155_DragonInfiltration_46" (4, 6) Sente 5 (DropMove Rook (4, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_156_DragonInfiltration_47" (4, 7) Sente 5 (DropMove Rook (4, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_157_DragonInfiltration_51" (5, 1) Sente 5 (DropMove Rook (5, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_158_DragonInfiltration_52" (5, 2) Sente 5 (DropMove Rook (5, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_159_DragonInfiltration_53" (5, 3) Sente 5 (DropMove Rook (5, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_160_DragonInfiltration_54" (5, 4) Sente 5 (DropMove Rook (5, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_161_DragonInfiltration_55" (5, 5) Sente 5 (DropMove Rook (5, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_162_DragonInfiltration_56" (5, 6) Sente 5 (DropMove Rook (5, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_163_DragonInfiltration_57" (5, 7) Sente 5 (DropMove Rook (5, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_164_DragonInfiltration_61" (6, 1) Sente 5 (DropMove Rook (6, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_165_DragonInfiltration_62" (6, 2) Sente 5 (DropMove Rook (6, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_166_DragonInfiltration_63" (6, 3) Sente 5 (DropMove Rook (6, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_167_DragonInfiltration_64" (6, 4) Sente 5 (DropMove Rook (6, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_168_DragonInfiltration_65" (6, 5) Sente 5 (DropMove Rook (6, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_169_DragonInfiltration_66" (6, 6) Sente 5 (DropMove Rook (6, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_170_DragonInfiltration_67" (6, 7) Sente 5 (DropMove Rook (6, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_171_DragonInfiltration_71" (7, 1) Sente 5 (DropMove Rook (7, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_172_DragonInfiltration_72" (7, 2) Sente 5 (DropMove Rook (7, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_173_DragonInfiltration_73" (7, 3) Sente 5 (DropMove Rook (7, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_174_DragonInfiltration_74" (7, 4) Sente 5 (DropMove Rook (7, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_175_DragonInfiltration_75" (7, 5) Sente 5 (DropMove Rook (7, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_176_DragonInfiltration_76" (7, 6) Sente 5 (DropMove Rook (7, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_177_DragonInfiltration_77" (7, 7) Sente 5 (DropMove Rook (7, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_178_DragonInfiltration_81" (8, 1) Sente 5 (DropMove Rook (8, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_179_DragonInfiltration_82" (8, 2) Sente 5 (DropMove Rook (8, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_180_DragonInfiltration_83" (8, 3) Sente 5 (DropMove Rook (8, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_181_DragonInfiltration_84" (8, 4) Sente 5 (DropMove Rook (8, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_182_DragonInfiltration_85" (8, 5) Sente 5 (DropMove Rook (8, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_183_DragonInfiltration_86" (8, 6) Sente 5 (DropMove Rook (8, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_184_DragonInfiltration_87" (8, 7) Sente 5 (DropMove Rook (8, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_185_DragonInfiltration_91" (9, 1) Sente 5 (DropMove Rook (9, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_186_DragonInfiltration_92" (9, 2) Sente 5 (DropMove Rook (9, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_187_DragonInfiltration_93" (9, 3) Sente 5 (DropMove Rook (9, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_188_DragonInfiltration_94" (9, 4) Sente 5 (DropMove Rook (9, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_189_DragonInfiltration_95" (9, 5) Sente 5 (DropMove Rook (9, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_190_DragonInfiltration_96" (9, 6) Sente 5 (DropMove Rook (9, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_191_DragonInfiltration_97" (9, 7) Sente 5 (DropMove Rook (9, 2)) [(Rook, 1)]
  , DynamicQEGTBEntry "MetaIdeal_192_DragonPenetration_Center" (5, 2) Sente 5 (DropMove Rook (5, 2)) [(Rook, 1)]
  ]

phase4_TacticalForks :: [DynamicQEGTBEntry]
phase4_TacticalForks =
  [ DynamicQEGTBEntry "MetaIdeal_193_KnightForkTactics_11" (1, 1) Sente 5 (DropMove Knight (1, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_194_KnightForkTactics_12" (1, 2) Sente 5 (DropMove Knight (1, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_195_KnightForkTactics_13" (1, 3) Sente 5 (DropMove Knight (1, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_196_KnightForkTactics_14" (1, 4) Sente 5 (DropMove Knight (1, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_197_KnightForkTactics_15" (1, 5) Sente 5 (DropMove Knight (1, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_198_KnightForkTactics_16" (1, 6) Sente 5 (DropMove Knight (1, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_199_KnightForkTactics_17" (1, 7) Sente 5 (DropMove Knight (1, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_200_KnightForkTactics_18" (1, 8) Sente 5 (DropMove Knight (1, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_201_KnightForkTactics_21" (2, 1) Sente 5 (DropMove Knight (2, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_202_KnightForkTactics_22" (2, 2) Sente 5 (DropMove Knight (2, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_203_KnightForkTactics_23" (2, 3) Sente 5 (DropMove Knight (2, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_204_KnightForkTactics_24" (2, 4) Sente 5 (DropMove Knight (2, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_205_KnightForkTactics_25" (2, 5) Sente 5 (DropMove Knight (2, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_206_KnightForkTactics_26" (2, 6) Sente 5 (DropMove Knight (2, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_207_KnightForkTactics_27" (2, 7) Sente 5 (DropMove Knight (2, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_208_KnightForkTactics_28" (2, 8) Sente 5 (DropMove Knight (2, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_209_KnightForkTactics_31" (3, 1) Sente 5 (DropMove Knight (3, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_210_KnightForkTactics_32" (3, 2) Sente 5 (DropMove Knight (3, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_211_KnightForkTactics_33" (3, 3) Sente 5 (DropMove Knight (3, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_212_KnightForkTactics_34" (3, 4) Sente 5 (DropMove Knight (3, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_213_KnightForkTactics_35" (3, 5) Sente 5 (DropMove Knight (3, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_214_KnightForkTactics_36" (3, 6) Sente 5 (DropMove Knight (3, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_215_KnightForkTactics_37" (3, 7) Sente 5 (DropMove Knight (3, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_216_KnightForkTactics_38" (3, 8) Sente 5 (DropMove Knight (3, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_217_KnightForkTactics_41" (4, 1) Sente 5 (DropMove Knight (4, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_218_KnightForkTactics_42" (4, 2) Sente 5 (DropMove Knight (4, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_219_KnightForkTactics_43" (4, 3) Sente 5 (DropMove Knight (4, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_220_KnightForkTactics_44" (4, 4) Sente 5 (DropMove Knight (4, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_221_KnightForkTactics_45" (4, 5) Sente 5 (DropMove Knight (4, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_222_KnightForkTactics_46" (4, 6) Sente 5 (DropMove Knight (4, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_223_KnightForkTactics_47" (4, 7) Sente 5 (DropMove Knight (4, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_224_KnightForkTactics_48" (4, 8) Sente 5 (DropMove Knight (4, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_225_KnightForkTactics_51" (5, 1) Sente 5 (DropMove Knight (5, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_226_KnightForkTactics_52" (5, 2) Sente 5 (DropMove Knight (5, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_227_KnightForkTactics_53" (5, 3) Sente 5 (DropMove Knight (5, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_228_KnightForkTactics_54" (5, 4) Sente 5 (DropMove Knight (5, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_229_KnightForkTactics_55" (5, 5) Sente 5 (DropMove Knight (5, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_230_KnightForkTactics_56" (5, 6) Sente 5 (DropMove Knight (5, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_231_KnightForkTactics_57" (5, 7) Sente 5 (DropMove Knight (5, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_232_KnightForkTactics_58" (5, 8) Sente 5 (DropMove Knight (5, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_233_KnightForkTactics_61" (6, 1) Sente 5 (DropMove Knight (6, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_234_KnightForkTactics_62" (6, 2) Sente 5 (DropMove Knight (6, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_235_KnightForkTactics_63" (6, 3) Sente 5 (DropMove Knight (6, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_236_KnightForkTactics_64" (6, 4) Sente 5 (DropMove Knight (6, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_237_KnightForkTactics_65" (6, 5) Sente 5 (DropMove Knight (6, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_238_KnightForkTactics_66" (6, 6) Sente 5 (DropMove Knight (6, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_239_KnightForkTactics_67" (6, 7) Sente 5 (DropMove Knight (6, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_240_KnightForkTactics_68" (6, 8) Sente 5 (DropMove Knight (6, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_241_KnightForkTactics_71" (7, 1) Sente 5 (DropMove Knight (7, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_242_KnightForkTactics_72" (7, 2) Sente 5 (DropMove Knight (7, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_243_KnightForkTactics_73" (7, 3) Sente 5 (DropMove Knight (7, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_244_KnightForkTactics_74" (7, 4) Sente 5 (DropMove Knight (7, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_245_KnightForkTactics_75" (7, 5) Sente 5 (DropMove Knight (7, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_246_KnightForkTactics_76" (7, 6) Sente 5 (DropMove Knight (7, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_247_KnightForkTactics_77" (7, 7) Sente 5 (DropMove Knight (7, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_248_KnightForkTactics_78" (7, 8) Sente 5 (DropMove Knight (7, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_249_KnightForkTactics_81" (8, 1) Sente 5 (DropMove Knight (8, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_250_KnightForkTactics_82" (8, 2) Sente 5 (DropMove Knight (8, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_251_KnightForkTactics_83" (8, 3) Sente 5 (DropMove Knight (8, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_252_KnightForkTactics_84" (8, 4) Sente 5 (DropMove Knight (8, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_253_KnightForkTactics_85" (8, 5) Sente 5 (DropMove Knight (8, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_254_KnightForkTactics_86" (8, 6) Sente 5 (DropMove Knight (8, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_255_KnightForkTactics_87" (8, 7) Sente 5 (DropMove Knight (8, 3)) [(Knight, 1)]
  , DynamicQEGTBEntry "MetaIdeal_256_KnightForkTactics_88" (8, 8) Sente 5 (DropMove Knight (8, 3)) [(Knight, 1)]
  ]

phase5_GuardStripping :: [DynamicQEGTBEntry]
phase5_GuardStripping =
  [ DynamicQEGTBEntry "MetaIdeal_257_GuardStripping_11" (1, 1) Sente 5 (DropMove Pawn (1, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_258_GuardStripping_12" (1, 2) Sente 5 (DropMove Pawn (1, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_259_GuardStripping_13" (1, 3) Sente 5 (DropMove Pawn (1, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_260_GuardStripping_14" (1, 4) Sente 5 (DropMove Pawn (1, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_261_GuardStripping_15" (1, 5) Sente 5 (DropMove Pawn (1, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_262_GuardStripping_16" (1, 6) Sente 5 (DropMove Pawn (1, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_263_GuardStripping_17" (1, 7) Sente 5 (DropMove Pawn (1, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_264_GuardStripping_21" (2, 1) Sente 5 (DropMove Pawn (2, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_265_GuardStripping_22" (2, 2) Sente 5 (DropMove Pawn (2, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_266_GuardStripping_23" (2, 3) Sente 5 (DropMove Pawn (2, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_267_GuardStripping_24" (2, 4) Sente 5 (DropMove Pawn (2, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_268_GuardStripping_25" (2, 5) Sente 5 (DropMove Pawn (2, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_269_GuardStripping_26" (2, 6) Sente 5 (DropMove Pawn (2, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_270_GuardStripping_27" (2, 7) Sente 5 (DropMove Pawn (2, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_271_GuardStripping_31" (3, 1) Sente 5 (DropMove Pawn (3, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_272_GuardStripping_32" (3, 2) Sente 5 (DropMove Pawn (3, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_273_GuardStripping_33" (3, 3) Sente 5 (DropMove Pawn (3, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_274_GuardStripping_34" (3, 4) Sente 5 (DropMove Pawn (3, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_275_GuardStripping_35" (3, 5) Sente 5 (DropMove Pawn (3, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_276_GuardStripping_36" (3, 6) Sente 5 (DropMove Pawn (3, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_277_GuardStripping_37" (3, 7) Sente 5 (DropMove Pawn (3, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_278_GuardStripping_41" (4, 1) Sente 5 (DropMove Pawn (4, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_279_GuardStripping_42" (4, 2) Sente 5 (DropMove Pawn (4, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_280_GuardStripping_43" (4, 3) Sente 5 (DropMove Pawn (4, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_281_GuardStripping_44" (4, 4) Sente 5 (DropMove Pawn (4, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_282_GuardStripping_45" (4, 5) Sente 5 (DropMove Pawn (4, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_283_GuardStripping_46" (4, 6) Sente 5 (DropMove Pawn (4, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_284_GuardStripping_47" (4, 7) Sente 5 (DropMove Pawn (4, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_285_GuardStripping_51" (5, 1) Sente 5 (DropMove Pawn (5, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_286_GuardStripping_52" (5, 2) Sente 5 (DropMove Pawn (5, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_287_GuardStripping_53" (5, 3) Sente 5 (DropMove Pawn (5, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_288_GuardStripping_54" (5, 4) Sente 5 (DropMove Pawn (5, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_289_GuardStripping_55" (5, 5) Sente 5 (DropMove Pawn (5, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_290_GuardStripping_56" (5, 6) Sente 5 (DropMove Pawn (5, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_291_GuardStripping_57" (5, 7) Sente 5 (DropMove Pawn (5, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_292_GuardStripping_61" (6, 1) Sente 5 (DropMove Pawn (6, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_293_GuardStripping_62" (6, 2) Sente 5 (DropMove Pawn (6, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_294_GuardStripping_63" (6, 3) Sente 5 (DropMove Pawn (6, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_295_GuardStripping_64" (6, 4) Sente 5 (DropMove Pawn (6, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_296_GuardStripping_65" (6, 5) Sente 5 (DropMove Pawn (6, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_297_GuardStripping_66" (6, 6) Sente 5 (DropMove Pawn (6, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_298_GuardStripping_67" (6, 7) Sente 5 (DropMove Pawn (6, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_299_GuardStripping_71" (7, 1) Sente 5 (DropMove Pawn (7, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_300_GuardStripping_72" (7, 2) Sente 5 (DropMove Pawn (7, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_301_GuardStripping_73" (7, 3) Sente 5 (DropMove Pawn (7, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_302_GuardStripping_74" (7, 4) Sente 5 (DropMove Pawn (7, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_303_GuardStripping_75" (7, 5) Sente 5 (DropMove Pawn (7, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_304_GuardStripping_76" (7, 6) Sente 5 (DropMove Pawn (7, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_305_GuardStripping_77" (7, 7) Sente 5 (DropMove Pawn (7, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_306_GuardStripping_81" (8, 1) Sente 5 (DropMove Pawn (8, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_307_GuardStripping_82" (8, 2) Sente 5 (DropMove Pawn (8, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_308_GuardStripping_83" (8, 3) Sente 5 (DropMove Pawn (8, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_309_GuardStripping_84" (8, 4) Sente 5 (DropMove Pawn (8, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_310_GuardStripping_85" (8, 5) Sente 5 (DropMove Pawn (8, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_311_GuardStripping_86" (8, 6) Sente 5 (DropMove Pawn (8, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_312_GuardStripping_87" (8, 7) Sente 5 (DropMove Pawn (8, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_313_GuardStripping_91" (9, 1) Sente 5 (DropMove Pawn (9, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_314_GuardStripping_92" (9, 2) Sente 5 (DropMove Pawn (9, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_315_GuardStripping_93" (9, 3) Sente 5 (DropMove Pawn (9, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_316_GuardStripping_94" (9, 4) Sente 5 (DropMove Pawn (9, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_317_GuardStripping_95" (9, 5) Sente 5 (DropMove Pawn (9, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_318_GuardStripping_96" (9, 6) Sente 5 (DropMove Pawn (9, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_319_GuardStripping_97" (9, 7) Sente 5 (DropMove Pawn (9, 2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "MetaIdeal_320_GuardStripping_Center" (5, 2) Sente 5 (DropMove Pawn (5, 2)) [(Pawn, 1)]
  ]

phase6_CastleDestruction :: [DynamicQEGTBEntry]
phase6_CastleDestruction =
  [ DynamicQEGTBEntry "MetaIdeal_321_YaguraDestruction_1" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_322_YaguraDestruction_2" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_323_YaguraDestruction_3" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_324_YaguraDestruction_4" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_325_YaguraDestruction_5" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_326_YaguraDestruction_6" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_327_YaguraDestruction_7" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_328_YaguraDestruction_8" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_329_MinoDestruction_1" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_330_MinoDestruction_2" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_331_MinoDestruction_3" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_332_MinoDestruction_4" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_333_MinoDestruction_5" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_334_MinoDestruction_6" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_335_MinoDestruction_7" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_336_MinoDestruction_8" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_337_AnagumaDestruction_1" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_338_AnagumaDestruction_2" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_339_AnagumaDestruction_3" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_340_AnagumaDestruction_4" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_341_AnagumaDestruction_5" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_342_AnagumaDestruction_6" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_343_AnagumaDestruction_7" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_344_AnagumaDestruction_8" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_345_FunagakoiDestruction_1" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_346_FunagakoiDestruction_2" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_347_FunagakoiDestruction_3" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_348_FunagakoiDestruction_4" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_349_FunagakoiDestruction_5" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_350_FunagakoiDestruction_6" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_351_FunagakoiDestruction_7" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_352_FunagakoiDestruction_8" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_353_GangiDestruction_1" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_354_GangiDestruction_2" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_355_GangiDestruction_3" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_356_GangiDestruction_4" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_357_GangiDestruction_5" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_358_GangiDestruction_6" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_359_GangiDestruction_7" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_360_GangiDestruction_8" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_361_ElmoDestruction_1" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_362_ElmoDestruction_2" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_363_ElmoDestruction_3" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_364_ElmoDestruction_4" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_365_ElmoDestruction_5" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_366_ElmoDestruction_6" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_367_ElmoDestruction_7" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_368_ElmoDestruction_8" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_369_SittingKingDestruction_1" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_370_SittingKingDestruction_2" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_371_SittingKingDestruction_3" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_372_SittingKingDestruction_4" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_373_SittingKingDestruction_5" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_374_SittingKingDestruction_6" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_375_SittingKingDestruction_7" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_376_SittingKingDestruction_8" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_377_RightKingDestruction_1" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_378_RightKingDestruction_2" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_379_RightKingDestruction_3" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_380_RightKingDestruction_4" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_381_RightKingDestruction_5" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_382_RightKingDestruction_6" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_383_RightKingDestruction_7" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  , DynamicQEGTBEntry "MetaIdeal_384_RightKingDestruction_8" (5, 1) Sente 5 (DropMove Silver (5, 2)) [(Silver, 1)]
  ]

phase7_MaterialSupremacy :: [DynamicQEGTBEntry]
phase7_MaterialSupremacy =
  [ DynamicQEGTBEntry "MetaIdeal_385_MaterialDomination_1" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_386_MaterialDomination_2" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_387_MaterialDomination_3" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_388_MaterialDomination_4" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_389_MaterialDomination_5" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_390_MaterialDomination_6" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_391_MaterialDomination_7" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_392_MaterialDomination_8" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_393_MaterialDomination_9" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_394_MaterialDomination_10" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_395_MaterialDomination_11" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_396_MaterialDomination_12" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_397_MaterialDomination_13" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_398_MaterialDomination_14" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_399_MaterialDomination_15" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_400_MaterialDomination_16" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_401_MaterialDomination_17" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_402_MaterialDomination_18" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_403_MaterialDomination_19" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_404_MaterialDomination_20" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_405_MaterialDomination_21" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_406_MaterialDomination_22" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_407_MaterialDomination_23" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_408_MaterialDomination_24" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_409_MaterialDomination_25" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_410_MaterialDomination_26" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_411_MaterialDomination_27" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_412_MaterialDomination_28" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_413_MaterialDomination_29" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_414_MaterialDomination_30" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_415_MaterialDomination_31" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_416_MaterialDomination_32" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_417_MaterialDomination_33" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_418_MaterialDomination_34" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_419_MaterialDomination_35" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_420_MaterialDomination_36" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_421_MaterialDomination_37" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_422_MaterialDomination_38" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_423_MaterialDomination_39" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_424_MaterialDomination_40" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_425_MaterialDomination_41" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_426_MaterialDomination_42" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_427_MaterialDomination_43" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_428_MaterialDomination_44" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_429_MaterialDomination_45" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_430_MaterialDomination_46" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_431_MaterialDomination_47" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_432_MaterialDomination_48" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_433_MaterialDomination_49" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_434_MaterialDomination_50" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_435_MaterialDomination_51" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_436_MaterialDomination_52" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_437_MaterialDomination_53" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_438_MaterialDomination_54" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_439_MaterialDomination_55" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_440_MaterialDomination_56" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_441_MaterialDomination_57" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_442_MaterialDomination_58" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_443_MaterialDomination_59" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_444_MaterialDomination_60" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_445_MaterialDomination_61" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_446_MaterialDomination_62" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_447_MaterialDomination_63" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_448_MaterialDomination_64" (5, 1) Sente 5 (DropMove Gold (5, 2)) [(Gold, 1)]
  ]

phase8_AsymptoticSinks :: [DynamicQEGTBEntry]
phase8_AsymptoticSinks =
  [ DynamicQEGTBEntry "MetaIdeal_449_MorseGradientSink_1" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_450_MorseGradientSink_2" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_451_MorseGradientSink_3" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_452_MorseGradientSink_4" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_453_MorseGradientSink_5" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_454_MorseGradientSink_6" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_455_MorseGradientSink_7" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_456_MorseGradientSink_8" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_457_MorseGradientSink_9" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_458_MorseGradientSink_10" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_459_MorseGradientSink_11" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_460_MorseGradientSink_12" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_461_MorseGradientSink_13" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_462_MorseGradientSink_14" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_463_MorseGradientSink_15" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_464_MorseGradientSink_16" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_465_MorseGradientSink_17" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_466_MorseGradientSink_18" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_467_MorseGradientSink_19" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_468_MorseGradientSink_20" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_469_MorseGradientSink_21" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_470_MorseGradientSink_22" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_471_MorseGradientSink_23" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_472_MorseGradientSink_24" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_473_MorseGradientSink_25" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_474_MorseGradientSink_26" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_475_MorseGradientSink_27" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_476_MorseGradientSink_28" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_477_MorseGradientSink_29" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_478_MorseGradientSink_30" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_479_MorseGradientSink_31" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_480_MorseGradientSink_32" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_481_MorseGradientSink_33" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_482_MorseGradientSink_34" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_483_MorseGradientSink_35" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_484_MorseGradientSink_36" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_485_MorseGradientSink_37" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_486_MorseGradientSink_38" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_487_MorseGradientSink_39" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_488_MorseGradientSink_40" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_489_MorseGradientSink_41" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_490_MorseGradientSink_42" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_491_MorseGradientSink_43" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_492_MorseGradientSink_44" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_493_MorseGradientSink_45" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_494_MorseGradientSink_46" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_495_MorseGradientSink_47" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_496_MorseGradientSink_48" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_497_MorseGradientSink_49" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_498_MorseGradientSink_50" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_499_MorseGradientSink_51" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_500_MorseGradientSink_52" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_501_MorseGradientSink_53" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_502_MorseGradientSink_54" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_503_MorseGradientSink_55" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_504_MorseGradientSink_56" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_505_MorseGradientSink_57" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_506_MorseGradientSink_58" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_507_MorseGradientSink_59" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_508_MorseGradientSink_60" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_509_MorseGradientSink_61" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_510_MorseGradientSink_62" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_511_MorseGradientSink_63" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  , DynamicQEGTBEntry "MetaIdeal_512_MorseGradientSink_64" (5, 1) Sente 7 (DropMove Gold (5, 2)) [(Gold, 1)]
  ]


initSeedQEGTB :: DynamicQEGTB
initSeedQEGTB = initMetaIdeals512

isCheckmatePosition :: State -> Bool
isCheckmatePosition st =
  let def = turn st
      inCheck = isKingUnderAttack def (board st)
  in inCheck && null (f5SyzygyFilter st (generateMoves st))

extractAndBackpropagateQEGTB :: State -> Move -> Int -> DynamicQEGTB -> DynamicQEGTB
extractAndBackpropagateQEGTB termSt lastMv dtm existingQEGTB =
  let att = opponent (turn termSt)
      def = turn termSt
      maybeDefK = findKing def (board termSt)
  in case maybeDefK of
       Nothing -> existingQEGTB
       Just kSq ->
         let entryId = printf "QEGTB_K%d%d_%s_DTM%d" (fst kSq) (snd kSq) (showMove lastMv) dtm
             alreadyExists = any (\e -> dqeId e == entryId) existingQEGTB
         in if alreadyExists
            then existingQEGTB
            else
              let newEntry = DynamicQEGTBEntry
                    { dqeId = entryId
                    , dqeKingSq = kSq
                    , dqeAttacker = att
                    , dqeDTM = dtm
                    , dqeBestMove = lastMv
                    , dqeRequiredHand = case lastMv of
                        DropMove pt _ -> [(pt, 1)]
                        _             -> []
                    }
              in newEntry : existingQEGTB

matchQEGTB :: State -> DynamicQEGTB -> Maybe (Int, Move)
matchQEGTB st qegtb =
  let att = turn st
      def = opponent att
      maybeDefK = findKing def (board st)
  in case maybeDefK of
       Nothing -> Nothing
       Just kSq ->
         let candidates = filter (\e -> dqeAttacker e == att && dqeKingSq e == kSq) qegtb
         in if null candidates
            then Nothing
            else
              let myHands = fromMaybe emptyHand (Map.lookup att (hands st))
                  hasHand reqs = all (\(pt, cnt) -> Map.findWithDefault 0 pt myHands >= cnt) reqs
                  matches = [ (dqeDTM e, dqeBestMove e)
                            | e <- candidates
                            , hasHand (dqeRequiredHand e)
                            , let mv = dqeBestMove e
                            , let nextSt = applyMove mv st
                            , isKingUnderAttack def (board nextSt)
                            ]
              in listToMaybe matches
