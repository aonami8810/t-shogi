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

-- | D. 統合中盤層化商空間 (Midgame Stratum)
data MidgameStratum = MidgameStratum
  { msTurn      :: !Player
  , msHand      :: !HandStratumKey
  , msPromotion :: !PromotionStratumKey
  , msDrop      :: !DropStratumKey
  , msMorseTier :: !Int -- -2: 後手優勢, -1: 後手微利, 0: 互角サドル点, +1: 先手微利, +2: 先手優勢
  } deriving (Eq, Ord, Show)

stratifyState :: State -> MidgameStratum
stratifyState st =
  let hKey = stratifyHand (hands st)
      pKey = stratifyPromotion (board st)
      dKey = stratifyDrop st
      eval = evaluateStratifiedState Sente st
      tier | eval >= 300.0  = 2
           | eval >= 100.0  = 1
           | eval <= -300.0 = -2
           | eval <= -100.0 = -1
           | otherwise      = 0
  in MidgameStratum (turn st) hKey pKey dKey tier

showStratumKey :: MidgameStratum -> String
showStratumKey MidgameStratum{..} =
  printf "Stratum[T:%s|MajDiff:%+d|GS:%+d|Pawn:(%d,%d)|Drag:(%d,%d)|Wedge:(%d,%d)|Fork:(%d,%d)|Tier:%+d]"
    (show msTurn)
    (hsMajorDiff msHand)
    (hsGoldSilverDiff msHand)
    (fst (hsPawnTier msHand)) (snd (hsPawnTier msHand))
    (if fst (psDragonStatus msPromotion) then (1::Int) else 0)
    (if snd (psDragonStatus msPromotion) then (1::Int) else 0)
    (fst (dsWedgeCount msDrop)) (snd (dsWedgeCount msDrop))
    (fst (dsKnightForkPot msDrop)) (snd (dsKnightForkPot msDrop))
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
      
      -- 5. モース玉危険度ポテンシャル
      morsePot (Just (kx, ky)) pTarget =
        let enemies = Map.toList $ Map.filter (\(Piece p _) -> p /= pTarget) bd
        in sum [ 10.0 / (fromIntegral (abs (kx - sx) + abs (ky - sy)) + 0.1) | ((sx, sy), _) <- enemies ]
      morsePot Nothing _ = 10000.0
      morseDiff = (morsePot oppKingPos (opponent plr) - morsePot myKingPos plr) * 6.0
  in bdTotal + myHandTotal - enemyHandTotal + totalPromoBonus + wedgeBonus oppKingPos + knightForkBonus + recentForkBonus + morseDiff

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

initSeedQEGTB :: DynamicQEGTB
initSeedQEGTB =
  [ DynamicQEGTBEntry "QEGTB_AtamaKin"          (5,1) Sente 1 (DropMove Gold (5,2)) [(Gold, 1)]
  , DynamicQEGTBEntry "QEGTB_RookHead"          (5,1) Sente 1 (NormalMove (5,3) (5,2) False) []
  , DynamicQEGTBEntry "QEGTB_TsurushiKei"       (5,1) Sente 1 (DropMove Knight (4,3)) [(Knight, 1)]
  , DynamicQEGTBEntry "QEGTB_KnightFork_DTM1"   (5,1) Sente 1 (DropMove Knight (4,3)) [(Knight, 1)]
  , DynamicQEGTBEntry "QEGTB_HaraKin"           (1,1) Sente 1 (DropMove Gold (2,1)) [(Gold, 1)]
  , DynamicQEGTBEntry "QEGTB_ShiriKin"          (5,1) Sente 1 (DropMove Gold (5,0)) [(Gold, 1)]
  , DynamicQEGTBEntry "QEGTB_Secchin_Corner"    (1,1) Sente 1 (DropMove Gold (1,2)) [(Gold, 1)]
  , DynamicQEGTBEntry "QEGTB_Lance_Skewer"      (5,1) Sente 1 (DropMove Lance (5,9)) [(Lance, 1)]
  , DynamicQEGTBEntry "QEGTB_Tatataki_Pawn"     (5,1) Sente 1 (DropMove Pawn (5,2)) [(Pawn, 1)]
  , DynamicQEGTBEntry "QEGTB_Silver_Diagonal"   (5,1) Sente 1 (DropMove Silver (4,2)) [(Silver, 1)]
  , DynamicQEGTBEntry "QEGTB_Horse_Dive"        (5,1) Sente 1 (NormalMove (7,3) (6,2) False) []
  , DynamicQEGTBEntry "QEGTB_Miyako_Center"     (5,5) Sente 1 (DropMove Gold (5,4)) [(Gold, 1)]
  , DynamicQEGTBEntry "QEGTB_Dragon_SideDive"   (5,1) Sente 1 (NormalMove (7,1) (6,1) False) []
  , DynamicQEGTBEntry "QEGTB_IkkenRyu"          (5,1) Sente 3 (DropMove Silver (5,2)) [(Silver, 1), (Gold, 1)]
  , DynamicQEGTBEntry "QEGTB_Okuri"             (5,2) Sente 3 (DropMove Silver (5,1)) [(Gold, 1)]
  , DynamicQEGTBEntry "QEGTB_HasamiUchi_Pincer" (5,1) Sente 3 (DropMove Silver (4,2)) [(Gold, 1), (Silver, 1)]
  , DynamicQEGTBEntry "QEGTB_Focal_Sacrifice"   (5,1) Sente 3 (DropMove Silver (5,3)) [(Gold, 1)]
  , DynamicQEGTBEntry "QEGTB_Staggered_Dragons" (5,1) Sente 3 (NormalMove (2,2) (5,2) False) []
  , DynamicQEGTBEntry "QEGTB_CrossRook_Encircle"(5,1) Sente 3 (DropMove Gold (5,2)) [(Gold, 1)]
  , DynamicQEGTBEntry "QEGTB_HeadSilver_BackGold"(5,1) Sente 3 (DropMove Silver (5,2)) [(Silver, 1), (Gold, 1)]
  , DynamicQEGTBEntry "QEGTB_KnightFork_PullIn" (5,1) Sente 3 (DropMove Knight (4,3)) [(Knight, 1), (Gold, 1)]
  , DynamicQEGTBEntry "QEGTB_EscapeRoute_Block" (5,1) Sente 3 (DropMove Silver (5,0)) [(Gold, 1)]
  , DynamicQEGTBEntry "QEGTB_Pincer_Brinkmate"  (5,1) Sente 5 (DropMove Gold (5,2)) [(Gold, 2)]
  , DynamicQEGTBEntry "QEGTB_Discovered_Mate"   (5,1) Sente 3 (NormalMove (5,2) (6,2) False) []
  , DynamicQEGTBEntry "QEGTB_Ryu_SokoFu"        (5,1) Sente 3 (DropMove Pawn (5,2)) [(Pawn, 1)]
  ]

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
         let myHands = fromMaybe emptyHand (Map.lookup att (hands st))
             hasHand reqs = all (\(pt, cnt) -> Map.findWithDefault 0 pt myHands >= cnt) reqs
             
             matches = [ (dqeDTM e, dqeBestMove e)
                       | e <- qegtb
                       , dqeAttacker e == att
                       , dqeKingSq e == kSq
                       , hasHand (dqeRequiredHand e)
                       , let mv = dqeBestMove e
                       , let nextSt = applyMove mv st
                       , isKingUnderAttack def (board nextSt)
                       ]
         in listToMaybe matches
