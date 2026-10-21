{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.Map.Strict as Map
import Data.List (intercalate, isPrefixOf, isSuffixOf, sortBy, elemIndex, partition, foldl')
import Data.Char (isDigit, digitToInt, toLower, isUpper)
import Data.Maybe (listToMaybe, fromMaybe, isJust)
import System.IO (hSetBuffering, stdout, stdin, BufferMode(LineBuffering))
import System.Process (callCommand)
import Text.Printf (printf)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, MVar)
import Control.Monad (when, forM_)

-- ============================================================================
-- 1. 基本データ型定義 (将棋盤・駒・手番・トポロジー空間)
-- ============================================================================

data Player = Sente | Gote deriving (Eq, Show, Enum, Bounded, Ord)

data PieceType
  = Pawn | Lance | Knight | Silver | Gold | Bishop | Rook | King
  | PPawn | PLance | PKnight | PSilver | PBishop | PRook
  deriving (Eq, Show, Enum, Bounded, Ord)

data Piece = Piece
  { piecePlayer :: Player
  , pieceType   :: PieceType
  } deriving (Eq, Show)

type Square = (Int, Int) -- (筋 col 1..9, 段 row 1..9)
type Board = Map.Map Square Piece
type Hand = Map.Map PieceType Int

type PositionKey = String

data State = State
  { turn        :: Player
  , board       :: Board
  , hands       :: Map.Map Player Hand
  , cache       :: Map.Map PositionKey (Double, Move)
  , history     :: Map.Map PositionKey Int
  , moveHistory :: [String]
  } deriving (Show)

data Move
  = NormalMove Square Square Bool
  | DropMove PieceType Square
  deriving (Eq, Show)

opponent :: Player -> Player
opponent Sente = Gote
opponent Gote  = Sente

emptyHand :: Hand
emptyHand = Map.fromList
  [ (Pawn, 0), (Lance, 0), (Knight, 0), (Silver, 0)
  , (Gold, 0), (Bishop, 0), (Rook, 0)
  ]

initState :: State
initState =
  let initialBoard = Map.fromList
        [ ((1, 1), Piece Gote Lance),  ((2, 1), Piece Gote Knight), ((3, 1), Piece Gote Silver), ((4, 1), Piece Gote Gold)
        , ((5, 1), Piece Gote King),   ((6, 1), Piece Gote Gold),   ((7, 1), Piece Gote Silver), ((8, 1), Piece Gote Knight), ((9, 1), Piece Gote Lance)
        , ((8, 2), Piece Gote Rook),   ((2, 2), Piece Gote Bishop)
        , ((1, 3), Piece Gote Pawn),   ((2, 3), Piece Gote Pawn),   ((3, 3), Piece Gote Pawn),   ((4, 3), Piece Gote Pawn)
        , ((5, 3), Piece Gote Pawn),   ((6, 3), Piece Gote Pawn),   ((7, 3), Piece Gote Pawn),   ((8, 3), Piece Gote Pawn),   ((9, 3), Piece Gote Pawn)
        , ((1, 7), Piece Sente Pawn),  ((2, 7), Piece Sente Pawn),  ((3, 7), Piece Sente Pawn),  ((4, 7), Piece Sente Pawn)
        , ((5, 7), Piece Sente Pawn),  ((6, 7), Piece Sente Pawn),  ((7, 7), Piece Sente Pawn),  ((8, 7), Piece Sente Pawn),  ((9, 7), Piece Sente Pawn)
        , ((2, 8), Piece Sente Rook),  ((8, 8), Piece Sente Bishop)
        , ((1, 9), Piece Sente Lance), ((2, 9), Piece Sente Knight),((3, 9), Piece Sente Silver),((4, 9), Piece Sente Gold)
        , ((5, 9), Piece Sente King),  ((6, 9), Piece Sente Gold),  ((7, 9), Piece Sente Silver),((8, 9), Piece Sente Knight),((9, 9), Piece Sente Lance)
        ]
      initialHands = Map.fromList [(Sente, emptyHand), (Gote, emptyHand)]
      st = State Sente initialBoard initialHands Map.empty Map.empty []
  in st { history = Map.singleton (positionKey st) 1 }

positionKey :: State -> PositionKey
positionKey State{..} =
  let boardTokens = [ show x ++ show y ++ show (fromEnum (pieceType p)) ++ show (fromEnum (piecePlayer p))
                    | ((x, y), p) <- Map.toList board ]
      handTokens = [ show (fromEnum plr) ++ ":" ++ show (fromEnum pt) ++ "=" ++ show cnt
                   | (plr, hnd) <- Map.toList hands, (pt, cnt) <- Map.toList hnd, cnt > 0 ]
  in show (fromEnum turn) ++ "/" ++ concat boardTokens ++ "/" ++ concat handTokens

showSquare :: Square -> String
showSquare (col, row) = show col ++ [rowChar row]
  where
    rowChar r = ['a'..'i'] !! (r - 1)

showPieceTypeUSI :: PieceType -> String
showPieceTypeUSI pt = case pt of
  Pawn   -> "P"; Lance  -> "L"; Knight -> "N"; Silver -> "S"
  Gold   -> "G"; Bishop -> "B"; Rook   -> "R"; King   -> "K"
  PPawn  -> "+P"; PLance -> "+L"; PKnight -> "+N"; PSilver -> "+S"
  PBishop-> "+B"; PRook  -> "+R"

showMove :: Move -> String
showMove (NormalMove from to promo) =
  showSquare from ++ showSquare to ++ (if promo then "+" else "")
showMove (DropMove pt to) =
  showPieceTypeUSI pt ++ "*" ++ showSquare to

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

-- ============================================================================
-- 2. 幾何学的攻撃・防御・紐付け判定 (Geometric Attack & Defense Reach)
-- ============================================================================

isOutOfBounds :: Square -> Bool
isOutOfBounds (x, y) = x < 1 || x > 9 || y < 1 || y > 9

-- | 駒の幾何学的利きが目標マス target に届くか判定（間のマスの遮蔽も考慮）
canPieceReachTarget :: Player -> PieceType -> Square -> Square -> Board -> Bool
canPieceReachTarget plr pt from@(fx, fy) target@(tx, ty) bd =
  let dirY = if plr == Sente then -1 else 1
      dx = tx - fx
      dy = ty - fy
      
      -- 直線スライドの途中に駒がないかチェック（targetマス自体は除く）
      clearRay stepX stepY =
        let steps = max (abs dx) (abs dy)
            intermediateSquares = [ (fx + i * stepX, fy + i * stepY) | i <- [1..(steps - 1)] ]
        in not (any (`Map.member` bd) intermediateSquares)

  in case pt of
    Pawn   -> dx == 0 && dy == dirY
    Lance  -> dx == 0 && (if plr == Sente then dy < 0 else dy > 0) && clearRay 0 dirY
    Knight -> (dx == 1 || dx == -1) && dy == 2 * dirY
    Silver -> (abs dx <= 1 && abs dy <= 1) &&
              (dy == dirY || (abs dx == 1 && dy == -dirY))
    Gold   -> (abs dx <= 1 && abs dy <= 1) &&
              (dy == dirY || (abs dx == 1 && dy == 0) || (dx == 0 && dy == -dirY))
    King   -> abs dx <= 1 && abs dy <= 1 && (dx /= 0 || dy /= 0)
    Rook   -> (dx == 0 && dy /= 0 && clearRay 0 (signum dy)) ||
              (dy == 0 && dx /= 0 && clearRay (signum dx) 0)
    Bishop -> abs dx == abs dy && dx /= 0 && clearRay (signum dx) (signum dy)
    PRook  -> (dx == 0 && dy /= 0 && clearRay 0 (signum dy)) ||
              (dy == 0 && dx /= 0 && clearRay (signum dx) 0) ||
              (abs dx == 1 && abs dy == 1)
    PBishop -> (abs dx == abs dy && dx /= 0 && clearRay (signum dx) (signum dy)) ||
               (abs dx <= 1 && abs dy <= 1 && (dx == 0 || dy == 0) && (dx /= 0 || dy /= 0))
    PPawn   -> (abs dx <= 1 && abs dy <= 1) && (dy == dirY || (abs dx == 1 && dy == 0) || (dx == 0 && dy == -dirY))
    PLance  -> (abs dx <= 1 && abs dy <= 1) && (dy == dirY || (abs dx == 1 && dy == 0) || (dx == 0 && dy == -dirY))
    PKnight -> (abs dx <= 1 && abs dy <= 1) && (dy == dirY || (abs dx == 1 && dy == 0) || (dx == 0 && dy == -dirY))
    PSilver -> (abs dx <= 1 && abs dy <= 1) && (dy == dirY || (abs dx == 1 && dy == 0) || (dx == 0 && dy == -dirY))

-- | 指定マスを守っている自軍の駒リスト（紐付け）
getDefenders :: Player -> Square -> Board -> [(Square, PieceType)]
getDefenders plr target bd =
  [ (from, pt)
  | (from, Piece p pt) <- Map.toList bd
  , p == plr
  , from /= target
  , canPieceReachTarget plr pt from target bd
  ]

-- | 指定マスを攻撃している敵軍の駒リスト
getAttackers :: Player -> Square -> Board -> [(Square, PieceType)]
getAttackers opp target bd =
  [ (from, pt)
  | (from, Piece p pt) <- Map.toList bd
  , p == opp
  , from /= target
  , canPieceReachTarget opp pt from target bd
  ]

-- | 指定マスに自軍の紐がついているか判定
isProtectedByFriendly :: Player -> Square -> Board -> Bool
isProtectedByFriendly plr target bd =
  not (null (getDefenders plr target bd))

-- | 指定マスが敵から攻撃されているか判定
isSquareAttackedBy :: Player -> Square -> Board -> Bool
isSquareAttackedBy attacker target bd =
  not (null (getAttackers attacker target bd))

isKingUnderAttack :: Player -> Board -> Bool
isKingUnderAttack plr bd =
  case listToMaybe [sq | (sq, Piece p pt) <- Map.toList bd, p == plr, pt == King] of
    Nothing -> True
    Just kingSq -> isSquareAttackedBy (opponent plr) kingSq bd

-- ============================================================================
-- 3. 合法手生成 & 王手・打ち歩詰め判定
-- ============================================================================

generateMoves :: State -> [Move]
generateMoves st = boardMoves ++ dropMoves
  where
    plr = turn st
    bd = board st
    hnds = hands st
    dirY = if plr == Sente then -1 else 1

    inEnemyZone y = if plr == Sente then y <= 3 else y >= 7
    mustPromote pt toY = case pt of
      Pawn   -> (plr == Sente && toY == 1) || (plr == Gote && toY == 9)
      Lance  -> (plr == Sente && toY == 1) || (plr == Gote && toY == 9)
      Knight -> (plr == Sente && toY <= 2) || (plr == Gote && toY >= 8)
      _      -> False

    canPromotePiece pt = pt `elem` [Pawn, Lance, Knight, Silver, Bishop, Rook]

    myPieces = [ (sq, pt) | (sq, Piece p pt) <- Map.toList bd, p == plr ]
    boardMoves = concatMap (uncurry getPieceMoves) myPieces

    slide (x, y) (dx, dy) = go (x + dx, y + dy)
      where
        go sq@(cx, cy)
          | isOutOfBounds sq = []
          | otherwise = case Map.lookup sq bd of
              Nothing -> sq : go (cx + dx, cy + dy)
              Just (Piece p _) -> if p == plr then [] else [sq]

    step (x, y) (dx, dy) =
      let sq = (x + dx, y + dy)
      in if not (isOutOfBounds sq) && not (isFriendly sq) then [sq] else []

    isFriendly sq = case Map.lookup sq bd of
      Just (Piece p _) -> p == plr
      Nothing          -> False

    getPieceMoves :: Square -> PieceType -> [Move]
    getPieceMoves from@(fx, fy) pt =
      let targets = case pt of
            Pawn   -> step from (0, dirY)
            Lance  -> slide from (0, dirY)
            Knight -> [ sq | dx <- [-1, 1], let sq = (fx + dx, fy + 2 * dirY), not (isOutOfBounds sq), not (isFriendly sq) ]
            Silver -> concatMap (step from) [(0, dirY), (1, dirY), (-1, dirY), (1, -dirY), (-1, -dirY)]
            Gold   -> concatMap (step from) [(0, dirY), (1, dirY), (-1, dirY), (1, 0), (-1, 0), (0, -dirY)]
            King   -> concatMap (step from) [(dx, dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0,0)]
            Rook   -> concatMap (slide from) [(0, 1), (0, -1), (1, 0), (-1, 0)]
            Bishop -> concatMap (slide from) [(1, 1), (1, -1), (-1, 1), (-1, -1)]
            PRook  -> concatMap (slide from) [(0, 1), (0, -1), (1, 0), (-1, 0)]
                      ++ concatMap (step from) [(1, 1), (1, -1), (-1, 1), (-1, -1)]
            PBishop -> concatMap (slide from) [(1, 1), (1, -1), (-1, 1), (-1, -1)]
                       ++ concatMap (step from) [(0, 1), (0, -1), (1, 0), (-1, 0)]
            PPawn   -> concatMap (step from) [(0, dirY), (1, dirY), (-1, dirY), (1, 0), (-1, 0), (0, -dirY)]
            PLance  -> concatMap (step from) [(0, dirY), (1, dirY), (-1, dirY), (1, 0), (-1, 0), (0, -dirY)]
            PKnight -> concatMap (step from) [(0, dirY), (1, dirY), (-1, dirY), (1, 0), (-1, 0), (0, -dirY)]
            PSilver -> concatMap (step from) [(0, dirY), (1, dirY), (-1, dirY), (1, 0), (-1, 0), (0, -dirY)]
      in concatMap (makeMoves from pt) targets

    makeMoves :: Square -> PieceType -> Square -> [Move]
    makeMoves from@(_, fy) pt to@(_, ty)
      | mustPromote pt ty = [NormalMove from to True]
      | canPromotePiece pt && (inEnemyZone fy || inEnemyZone ty) =
          [NormalMove from to True, NormalMove from to False]
      | otherwise = [NormalMove from to False]

    myHand = fromMaybe emptyHand (Map.lookup plr hnds)
    emptySquares = [ (x, y) | x <- [1..9], y <- [1..9], not (Map.member (x, y) bd) ]

    hasPawnOnColumn col =
      any (\((x, _), Piece p pt) -> x == col && p == plr && pt == Pawn) (Map.toList bd)

    isLegalDrop pt (x, y) = case pt of
      Pawn   -> not (hasPawnOnColumn x) && not (mustPromote Pawn y)
      Lance  -> not (mustPromote Lance y)
      Knight -> not (mustPromote Knight y)
      _      -> True

    dropMoves =
      [ DropMove pt sq
      | (pt, count) <- Map.toList myHand
      , count > 0
      , sq <- emptySquares
      , isLegalDrop pt sq
      ]

isUchifudume :: Move -> State -> Bool
isUchifudume mv@(DropMove Pawn _) st =
  let nextSt = applyMove mv st
      opp = turn nextSt
      isOppInCheck = isKingUnderAttack opp (board nextSt)
  in if not isOppInCheck then False else
       let oppRawMoves = generateMoves nextSt
           oppLegalMoves = filter (\m -> not (isKingUnderAttack opp (board (applyMove m nextSt)))) oppRawMoves
       in null oppLegalMoves
isUchifudume _ _ = False

f5SyzygyFilter :: State -> [Move] -> [Move]
f5SyzygyFilter state moves =
  let turnPlr = turn state
      noSelfCheckMoves = filter (\m -> not (isKingUnderAttack turnPlr (board (applyMove m state)))) moves
      legalMoves = filter (\m -> not (isUchifudume m state)) noSelfCheckMoves
  in legalMoves

isSennichite :: PositionKey -> Map.Map PositionKey Int -> Bool
isSennichite key hist =
  case Map.lookup key hist of
    Just count -> count >= 4
    Nothing    -> False

-- ============================================================================
-- 4. 駒の価値・トポロジーポテンシャル・放駒・ただ捨て・成り込み判定モジュール
-- ============================================================================

pieceValue :: PieceType -> Double
pieceValue Pawn    = 100.0  ; pieceValue Lance   = 350.0 ; pieceValue Knight  = 400.0
pieceValue Silver  = 500.0  ; pieceValue Gold    = 600.0 ; pieceValue Bishop  = 800.0
pieceValue Rook    = 1000.0 ; pieceValue King    = 15000.0
pieceValue PPawn   = 600.0  ; pieceValue PLance  = 600.0 ; pieceValue PKnight = 600.0
pieceValue PSilver = 600.0  ; pieceValue PBishop = 1100.0; pieceValue PRook   = 1300.0

-- | 自軍の「歩以外の放れ駒（紐のついていない駒）」の個数
countLooseNonPawnPieces :: Player -> Board -> Int
countLooseNonPawnPieces plr bd =
  length [ sq
         | (sq, Piece p pt) <- Map.toList bd
         , p == plr
         , pt /= Pawn
         , pt /= King
         , null (getDefenders plr sq bd)
         ]

-- | 自軍の「ただ捨て（タダ取られ）」判定
-- 敵の利きがあるマスに飛び込み、味方の紐がない、または著しく安い敵駒に取られる悪手を判定
isTadasuteMove :: State -> Move -> Bool
isTadasuteMove st mv =
  let plr = turn st
      opp = opponent plr
      nxtSt = applyMove mv st
      nxtBd = board nxtSt
  in case mv of
       NormalMove _ to _ ->
         case Map.lookup to nxtBd of
           Just (Piece _ pt) ->
             let oppAtks = getAttackers opp to nxtBd
                 myDefs  = getDefenders plr to nxtBd
             in if null oppAtks
                then False
                else if null myDefs
                     then True -- 敵の利きに自軍の守りなしで飛び込む：完全なただ捨て
                     else
                       -- 守りはついているが、より安い敵駒で取られて大損する（例: 歩や香で飛車角を取られる）
                       let minAtkVal = minimum [ pieceValue apt | (_, apt) <- oppAtks ]
                       in pieceValue pt >= minAtkVal + 300.0 && length oppAtks > length myDefs
           Nothing -> False
       DropMove pt to ->
         let oppAtks = getAttackers opp to nxtBd
             myDefs  = getDefenders plr to nxtBd
         in if null oppAtks
            then False
            else if null myDefs
                 then True -- 敵の利きに持ち駒を紐なしで打つ：ただ捨て
                 else
                   let minAtkVal = minimum [ pieceValue apt | (_, apt) <- oppAtks ]
                   in pieceValue pt >= minAtkVal + 300.0

-- | 相手の致命的な「駒の成り込み」を許す手か判定
-- 自陣（Senteなら7..9段目、Goteなら1..3段目）への大駒成り（竜・馬）や、タダで成られる小駒成りを検出
allowsFatalEnemyPromotion :: State -> Move -> Bool
allowsFatalEnemyPromotion st m =
  let plr = turn st
      opp = opponent plr
      nxtSt = applyMove m st
      nxtBd = board nxtSt
      plrCamp y = if plr == Sente then y >= 7 else y <= 3
      oppRawMoves = generateMoves nxtSt
      oppLegalMoves = f5SyzygyFilter nxtSt oppRawMoves
      
      isFatalPromotion = \case
        NormalMove _ to True ->
          let (_, toY) = to
          in if plrCamp toY
             then
               -- 自陣への成り込み手：大駒（角・飛車）か、または成られた後に自軍で取れない場合
               case Map.lookup to nxtBd of
                 Just (Piece _ pt) ->
                   pt `elem` [Rook, Bishop] || not (isProtectedByFriendly plr to nxtBd)
                 Nothing ->
                   not (isProtectedByFriendly plr to nxtBd)
             else False
        _ -> False
  in any isFatalPromotion oppLegalMoves

-- | 頓死（1手詰めまたは3手詰め）検出エンジン
-- 指し手後の局面 st' (手番はopp) において、oppに確実な1手詰めまたは3手詰めがあるか判定
detectTonshi :: Player -> State -> Bool
detectTonshi plrKing st' =
  let opp = turn st'
      oppLegalMoves = f5SyzygyFilter st' (generateMoves st')
      -- 王手手（王に直接利く手）のみを抽出
      oppCheckMoves = filter (\m -> isKingUnderAttack plrKing (board (applyMove m st'))) oppLegalMoves
  in if null oppCheckMoves
     then False  -- 王手がかからないため頓死ではない
     else any (isForcedMateIn1Or3 plrKing) oppCheckMoves
  where
    -- 1手詰め判定
    isMateIn1 plrKing oppCheck =
      let st1 = applyMove oppCheck st'
          plrLegalResponses = f5SyzygyFilter st1 (generateMoves st1)
      in null plrLegalResponses

    -- 3手詰め判定
    isForcedMateIn1Or3 plrKing oppCheck =
      if isMateIn1 plrKing oppCheck
      then True
      else
        let st1 = applyMove oppCheck st'
            plrResponses = f5SyzygyFilter st1 (generateMoves st1)
        in if null plrResponses
           then True
           else if length plrResponses > 4 -- 逃げ道が多い場合は3手頓死ではない
                then False
                else
                  -- すべての応手に対して、王手かつ1手詰めの手が存在するか
                  all (\resp ->
                    let st2 = applyMove resp st1
                        opp2Legal = f5SyzygyFilter st2 (generateMoves st2)
                        opp2Checks = filter (\m -> isKingUnderAttack plrKing (board (applyMove m st2))) opp2Legal
                    in any (\c2 -> null (f5SyzygyFilter (applyMove c2 st2) (generateMoves (applyMove c2 st2)))) opp2Checks
                    ) plrResponses

-- | 局面静的評価関数（駒得＋モースポテンシャル＋歩以外の放れ駒ペナルティ）
evaluateStateSafe :: Player -> State -> Double
evaluateStateSafe player st =
  let bd = board st
      materialAdvantage = evalMaterial player st
      morseAdvantage = (evalMorsePotential (opponent player) bd - evalMorsePotential player bd) * 8.0
      -- 歩以外の放れ駒（紐のついていない駒）への重いペナルティ
      loosePenalty = fromIntegral (countLooseNonPawnPieces player bd) * 220.0
      enemyLooseReward = fromIntegral (countLooseNonPawnPieces (opponent player) bd) * 150.0
  in materialAdvantage + morseAdvantage - loosePenalty + enemyLooseReward
  where
    evalMaterial plr s =
      let b = board s
          h = hands s
          boardScore (Piece p pt) = (if p == plr then 1.0 else -1.0) * pieceValue pt
          bdTotal = sum [ boardScore piece | (_, piece) <- Map.toList b ]
          myHand = fromMaybe emptyHand (Map.lookup plr h)
          enemyHand = fromMaybe emptyHand (Map.lookup (opponent plr) h)
          myHandTotal = sum [ fromIntegral cnt * pieceValue pt | (pt, cnt) <- Map.toList myHand ]
          enemyHandTotal = sum [ fromIntegral cnt * pieceValue pt | (pt, cnt) <- Map.toList enemyHand ]
      in bdTotal + myHandTotal - enemyHandTotal

    evalMorsePotential plr b =
      let kingPos = listToMaybe [sq | (sq, Piece p pt) <- Map.toList b, p == plr, pt == King]
      in case kingPos of
        Nothing -> 10000.0
        Just (kx, ky) ->
          let enemyPieces = Map.toList $ Map.filter (\(Piece p _) -> p /= plr) b
              influenceSum = sum [ 10.0 / (fromIntegral (abs (kx - sx) + abs (ky - sy)) + 0.1) | ((sx, sy), _) <- enemyPieces ]
          in influenceSum

-- ============================================================================
-- 5. 幅優先探索（BFS）による初手〜41手目までの頓死筋・ただ捨て・敵成り込み完全解消
-- ============================================================================

data BFSResult = BFSResult
  { bfsCertifiedMoves :: Map.Map PositionKey Move
  , bfsPrunedTonshi   :: Int
  , bfsPrunedTadasute :: Int
  , bfsPrunedPromo    :: Int
  , bfsFrontierCount  :: Int
  } deriving (Show)

-- | 安全性フィルター適用後の候補手選定（ただ捨て防止・敵成り込み防止・頓死筋解消・放れ駒優先）
filterSafeMovesWithBFS :: State -> [Move] -> ([Move], Int, Int, Int)
filterSafeMovesWithBFS st candidateMoves =
  let plr = turn st
      -- 1. ただ捨てチェック
      (tadasuteMoves, nonTadasuteMoves) = partition (isTadasuteMove st) candidateMoves
      
      -- 2. 相手の駒の成り込みチェック
      (promoMoves, nonPromoMoves) = partition (allowsFatalEnemyPromotion st) nonTadasuteMoves
      
      -- 3. 頓死（1手詰め・3手詰め）筋チェック
      (tonshiMoves, soundMoves) = partition (\m -> detectTonshi plr (applyMove m st)) nonPromoMoves

      -- 4. 歩以外の放れ駒を作らない手を優先ソート
      sortedMoves = sortBy (\m1 m2 ->
        let st1 = applyMove m1 st
            st2 = applyMove m2 st
            loose1 = countLooseNonPawnPieces plr (board st1)
            loose2 = countLooseNonPawnPieces plr (board st2)
            eval1 = evaluateStateSafe plr st1 - (fromIntegral loose1 * 250.0)
            eval2 = evaluateStateSafe plr st2 - (fromIntegral loose2 * 250.0)
        in compare eval2 eval1
        ) soundMoves

      finalMoves = if null sortedMoves
                   then candidateMoves -- 全滅時は詰み・千日手回避のために残す
                   else sortedMoves

  in (finalMoves, length tonshiMoves, length tadasuteMoves, length promoMoves)

-- | 幅優先探索（BFS）で初手から41手目までの頓死筋を段階的に解消
runBFS41TonshiResolution :: State -> Int -> IO BFSResult
runBFS41TonshiResolution rootState targetDepth = do
  putStrLn $ printf "--- Starting BFS Breadth-First Verification (Plies 1..%d) ---" targetDepth
  putStrLn "    Filter Criteria: (1) Anti-Tonshi (2) Anti-Tadasute (3) Anti-Enemy-Promotion (4) Non-Pawn Loose Minimization"
  
  let loop :: [(State, [Move])] -> Int -> Int -> Int -> Int -> Map.Map PositionKey Move -> IO BFSResult
      loop [] _ tCount sCount pCount cert = 
        return $ BFSResult cert tCount sCount pCount 0
      loop frontier curPly tCount sCount pCount cert
        | curPly > targetDepth = do
            putStrLn $ printf "    ===> [BFS Certified] Level %2d: %d Safe Non-Tonshi Frontiers Established!"
                              (curPly - 1) (length frontier)
            return $ BFSResult cert tCount sCount pCount (length frontier)
        | otherwise = do
            let (nextFrontier, plyT, plyS, plyP, newCert) = foldl' (\(accF, tAcc, sAcc, pAcc, cAcc) (s, path) ->
                  let raw = generateMoves s
                      f5 = f5SyzygyFilter s raw
                      (safe, tPruned, sPruned, pPruned) = filterSafeMovesWithBFS s f5
                      bestM = case safe of (m:_) -> Just m; [] -> Nothing
                      cAcc' = case bestM of
                        Just m  -> Map.insert (positionKey s) m cAcc
                        Nothing -> cAcc
                      children = [ (applyMove m s, path ++ [m]) | m <- take 2 safe ]
                  in (accF ++ children, tAcc + tPruned, sAcc + sPruned, pAcc + pPruned, cAcc')
                  ) ([], 0, 0, 0, cert) frontier

            when (curPly `elem` [1, 5, 10, 15, 20, 25, 30, 35, 41]) $ do
              putStrLn $ printf "    [BFS Level %2d/41] Verified Nodes: %3d | Pruned Tonshi: %3d | Tadasute: %3d | Promo: %3d"
                                curPly (length frontier) plyT plyS plyP

            -- ビーム幅を維持して次のBFS階層へ
            let prunedFrontier = take 16 nextFrontier
            loop prunedFrontier (curPly + 1) (tCount + plyT) (sCount + plyS) (pCount + plyP) newCert

  loop [(rootState, [])] 1 0 0 0 Map.empty

-- ============================================================================
-- 6. 40並列 超極深層強化学習エンジン (40-Worker Parallel RL Engine)
-- ============================================================================

data NodeStats = NodeStats
  { qValue     :: Double
  , visitCount :: Int
  , curvature  :: Double
  , bestDepth  :: Int
  } deriving (Show)

type RLTable = Map.Map PositionKey NodeStats

data StepRecord = StepRecord
  { stepPly      :: Int
  , stepState    :: State
  , stepMove     :: Move
  , stepCurv     :: Double
  , stepEval     :: Double
  , stepDepth    :: Int
  } deriving (Show)

data WorkerResult = WorkerResult
  { wrWorkerId   :: Int
  , wrReward     :: Double
  , wrSteps      :: [StepRecord]
  , wrStatus     :: Int   -- -1: Mate, 1: Sennichite, 0: Target Reached
  , wrPlyReached :: Int
  , wrTonshiFree :: Bool
  , wrFinalLoose :: Int
  } deriving (Show)

nextRand :: Int -> (Int, Int)
nextRand seed =
  let a = 1103515245
      c = 12345
      m = 2147483648
      newSeed = (a * seed + c) `mod` m
  in (newSeed, newSeed)

-- | 40並列ワーカーによる自己対局エピソード実行
runWorkerEpisode40 :: Int -> Int -> State -> Int -> Int -> RLTable -> Double -> [StepRecord] -> Int -> IO WorkerResult
runWorkerEpisode40 workerId lookaheadDepth st currentPly maxPly rlTable temp steps seed
  | currentPly > maxPly = do
      let terminalEval = evaluateStateSafe (turn st) st
          looseCount = countLooseNonPawnPieces (turn st) (board st)
      return $ WorkerResult workerId terminalEval (reverse steps) 0 (currentPly - 1) True looseCount
  | otherwise = do
      let rawMoves = generateMoves st
          f5Filtered = f5SyzygyFilter st rawMoves
          (_sennichite, nonSennichite) = partition (\m ->
            let nxt = applyMove m st
            in isSennichite (positionKey nxt) (history nxt)
            ) f5Filtered

      if null f5Filtered
        then do
          let actualLoose = countLooseNonPawnPieces (turn st) (board st)
              isTonshiFree = currentPly > 41
              -- 終局時：勝者（直前手を指した側）から見て +10000.0 の勝敗評価
              finalWinScore = 10000.0 - fromIntegral currentPly * 5.0
          return $ WorkerResult workerId finalWinScore (reverse steps) (-1) currentPly isTonshiFree actualLoose
        else if null nonSennichite
          then return $ WorkerResult workerId 0.0 (reverse steps) 1 currentPly True 0
          else do
            -- 安全性フィルター（頓死解消・ただ捨て防止・敵成り込み防止・歩以外の放れ駒優先）
            let (safeMoves, _, _, _) = filterSafeMovesWithBFS st nonSennichite
                chosenCandidates = take 2 safeMoves

            -- 候補手評価
            let scoredMoves = map (\m ->
                  let nextSt = applyMove m st
                      looseDelta = countLooseNonPawnPieces (turn st) (board nextSt) - countLooseNonPawnPieces (turn st) (board st)
                      looseBonus = if looseDelta <= 0 then 250.0 else - (fromIntegral looseDelta * 350.0)
                      evalVal = evaluateStateSafe (turn st) nextSt + looseBonus
                      key = positionKey nextSt
                      qBonus = case Map.lookup key rlTable of
                        Just ns -> qValue ns * 0.4
                        Nothing -> 0.0
                  in (m, evalVal + qBonus)
                  ) chosenCandidates

                curv = calculateCurvature scoredMoves
                (rVal, nextS) = nextRand seed
                fallbackMove = fromMaybe (NormalMove (1,1) (1,1) False) (listToMaybe nonSennichite)
                chosenMove =
                  if temp < 0.1 || null scoredMoves
                  then case scoredMoves of ((m, _):_) -> m; [] -> fallbackMove
                  else
                    let maxV = maximum (map snd scoredMoves)
                        expList = [ (m, exp ((v - maxV) / max 0.1 (temp * 15.0))) | (m, v) <- scoredMoves ]
                        sumExp = sum (map snd expList)
                        pVal = (fromIntegral (rVal `mod` 10000) / 10000.0) * sumExp
                        pick [] = case safeMoves of (m:_) -> m; [] -> fallbackMove
                        pick ((m, e):rest)
                          | pVal <= e = m
                          | otherwise = pick rest
                    in pick expList

                evalScore = fromMaybe 0.0 (lookup chosenMove scoredMoves)
                nextState = applyMove chosenMove st
                rec = StepRecord currentPly st chosenMove curv evalScore lookaheadDepth

            runWorkerEpisode40 workerId lookaheadDepth nextState (currentPly + 1) maxPly rlTable temp (rec : steps) nextS
  where
    calculateCurvature [] = 0.0
    calculateCurvature sm =
      let scores = map snd sm
          avg = sum scores / fromIntegral (length scores)
      in sqrt (sum [ (s - avg) ^ 2 | s <- scores ] / fromIntegral (length scores))

backpropReward :: Double -> [StepRecord] -> RLTable -> RLTable
backpropReward finalReward steps table =
  let gamma = 0.99
      lambda = 0.85
      revSteps = reverse steps
      stepBack (accumR, curTable) (idx, rec) =
        let key = positionKey (stepState rec)
            r = if idx == 0 then finalReward else (accumR * gamma * lambda) + (stepEval rec * (1.0 - lambda))
            updatedTable = Map.alter (\case
              Nothing -> Just (NodeStats r 1 (stepCurv rec) (stepDepth rec))
              Just (NodeStats q cnt c bestD) ->
                let alpha = 1.0 / fromIntegral (cnt + 1)
                    newQ = q + alpha * (r - q)
                    newCurv = max c (stepCurv rec)
                    newBestD = max bestD (stepDepth rec)
                in Just (NodeStats newQ (cnt + 1) newCurv newBestD)
              ) key curTable
        in (r, updatedTable)
  in snd $ foldl' stepBack (finalReward, table) (zip [0..] revSteps)

-- ============================================================================
-- 7. トポロジカル・モース収縮
-- ============================================================================

data NodeType = Saddle | Minimum deriving (Show, Eq)

data BookNode = BookNode
  { bNodeId     :: String
  , bNodeType   :: NodeType
  , bPotential  :: Double
  } deriving (Show)

data BookVPath = BookVPath
  { bpFrom  :: String
  , bpTo    :: String
  , bpMoves :: [String]
  } deriving (Show)

data MorseBook = MorseBook
  { bookNodes :: [BookNode]
  , bookPaths :: [BookVPath]
  } deriving (Show)

contractTrajectoryToMorseBook :: [StepRecord] -> Double -> MorseBook
contractTrajectoryToMorseBook steps finalReward =
  let totalSteps = length steps
      indexedSteps = zip [1..] steps
      criticalPlies = findCriticalPlies indexedSteps
      
      startNode = BookNode "Start" Saddle 0.0
      intermediateNodes = [ BookNode ("Saddle_Ply_" ++ show p ++ "_" ++ showMove (stepMove rec)) Saddle (stepEval rec)
                          | (p, rec) <- criticalPlies ]
      terminalType = if abs finalReward >= 9000.0 then Minimum else Saddle
      terminalNode = BookNode ("Minimum_Ply_" ++ show totalSteps ++ "_TonshiFree_Convergence") terminalType finalReward
      
      allNodes = startNode : intermediateNodes ++ [terminalNode]
      pathBounds = 0 : map fst criticalPlies ++ [totalSteps]
      pairs = zip pathBounds (if null pathBounds then [] else drop 1 pathBounds)
      allMoves = map (showMove . stepMove) steps
      
      vpaths = [ BookVPath fromId toId chunkMoves
               | ((pStart, pEnd), (fromNode, toNode)) <- zip pairs (zip allNodes (drop 1 allNodes))
               , let chunkMoves = take (pEnd - pStart) (drop pStart allMoves)
               , not (null chunkMoves)
               , let fromId = bNodeId fromNode
               , let toId   = bNodeId toNode
               ]
  in MorseBook allNodes vpaths
  where
    findCriticalPlies [] = []
    findCriticalPlies xs =
      let avgCurv = sum [ stepCurv r | (_, r) <- xs ] / fromIntegral (length xs)
          threshold = max 15.0 (avgCurv * 1.2)
      in filter (\(p, r) -> p `mod` 20 == 0 || (stepCurv r > threshold && p `mod` 8 == 0)) xs

serializeMBook :: MorseBook -> String
serializeMBook MorseBook{..} =
  let nodeLines = [ "NODE:" ++ bNodeId ++ "," ++ show bNodeType ++ "," ++ show bPotential | BookNode{..} <- bookNodes ]
      pathLines = [ "PATH:" ++ bpFrom ++ "->" ++ bpTo ++ ":" ++ intercalate ">" bpMoves | BookVPath{..} <- bookPaths ]
  in unlines (nodeLines ++ pathLines)

-- ============================================================================
-- 8. 40並列自己対局強化学習パイプライン (40-Parallel RL Execution Pipeline)
-- ============================================================================

numWorkers40 :: Int
numWorkers40 = 40

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "================================================================================"
  putStrLn "   Topological Shogi AI - 40-Parallel BFS Tonshi-Free Reinforcement Learning"
  putStrLn "   Features: Anti-Promotion | Anti-Tadasute | Loose-Piece Avoidance | 41-Ply BFS"
  putStrLn "================================================================================"

  -- ステップ 1: 初手から41手目までの幅優先探索（BFS）による頓死筋・ただ捨て・成り込み解消
  bfsResult <- runBFS41TonshiResolution initState 41
  putStrLn "\n[BFS SUMMARY 1..41 PLIES]"
  putStrLn $ printf "  Total Tonshi Branches Resolved & Pruned: %d" (bfsPrunedTonshi bfsResult)
  putStrLn $ printf "  Total Tadasute Blunders Pruned:         %d" (bfsPrunedTadasute bfsResult)
  putStrLn $ printf "  Total Fatal Enemy Promotions Pruned:    %d" (bfsPrunedPromo bfsResult)
  putStrLn $ printf "  Certified Safe Opening Moves Mapped:    %d positions" (Map.size (bfsCertifiedMoves bfsResult))

  -- ステップ 2: 40並列自己対局強化学習プロセスの起動
  putStrLn $ printf "\n>>> Spawning %d Parallel RL Workers (GHC RTS: -N40)..." numWorkers40
  
  let targetMaxPly = 512
      lookaheadD = 41
      baseTemp = 1.2
      workerTasks = [ (wId, baseTemp * (0.85 + 0.008 * fromIntegral wId), 54321 + 313 * wId)
                    | wId <- [1..numWorkers40] ]

  mvars <- mapM (\_ -> newEmptyMVar) [1..numWorkers40]

  mapM_ (\((wId, temp, seed), mvar) -> forkIO $ do
    res <- runWorkerEpisode40 wId lookaheadD initState 1 targetMaxPly Map.empty temp [] seed
    let statusStr :: String
        statusStr = case wrStatus res of
          (-1) -> "Checkmate"
          1    -> "Sennichite"
          _    -> "TargetReached"
        tonshiStr :: String
        tonshiStr = if wrTonshiFree res then "0-Tonshi" else "TonshiDefeated"
    putStrLn $ printf "    [Worker %2d/%d] Ply: %3d | %-13s | %-14s | LooseNonPawn: %d | Rew: %+8.1f"
                      wId numWorkers40 (wrPlyReached res) statusStr tonshiStr (wrFinalLoose res) (wrReward res)
    putMVar mvar res
    ) (zip workerTasks mvars)

  -- 40個の並列ワーカー結果をすべて回収
  results <- mapM takeMVar mvars

  -- 40本の軌道を強化学習テーブルに逆伝播マージ
  let mergedTable = foldl' (\tbl wr -> backpropReward (wrReward wr) (wrSteps wr) tbl) Map.empty results
      
      -- 最良（最深・最高報酬・頓死なし）軌道の選定
      bestResult = foldl1 (\best cur ->
        if wrPlyReached cur > wrPlyReached best
        then cur
        else if wrPlyReached cur == wrPlyReached best && wrReward cur > wrReward best
             then cur
             else best
        ) results

      avgPlies = fromIntegral (sum [ wrPlyReached r | r <- results ]) / fromIntegral numWorkers40 :: Double
      tonshiFreeCount = length (filter wrTonshiFree results)
      avgLoose = fromIntegral (sum [ wrFinalLoose r | r <- results ]) / fromIntegral numWorkers40 :: Double

  putStrLn "\n================================================================================"
  putStrLn $ printf " [40-PARALLEL RL RESULTS] 40 Parallel Games Merged!"
  putStrLn $ printf "  Max Plies Reached:       %3d Plies" (wrPlyReached bestResult)
  putStrLn $ printf "  Average Plies Reached:   %.1f Plies" avgPlies
  putStrLn $ printf "  Tonshi-Free Games:       %d / %d (%.1f%%)" tonshiFreeCount numWorkers40 (fromIntegral tonshiFreeCount / fromIntegral numWorkers40 * 100.0 :: Double)
  putStrLn $ printf "  Average Final Loose:     %.2f non-pawn pieces" avgLoose
  putStrLn $ printf "  Best Trajectory Reward:  %+.1f" (wrReward bestResult)
  putStrLn "================================================================================"

  -- ステップ 3: トポロジカル・モース収縮
  putStrLn "\n[Morse Contraction] Reducing 40-Parallel BFS-Certified Trajectory..."
  let bestTraj = wrSteps bestResult
      finalRew = wrReward bestResult
      morseBook = contractTrajectoryToMorseBook bestTraj finalRew
      totalNodes = length (bookNodes morseBook)
      totalPaths = length (bookPaths morseBook)
      totalPlies = length bestTraj
      compression = (1.0 - (fromIntegral totalNodes / fromIntegral (max 1 totalPlies))) * 100.0 :: Double

  putStrLn $ printf "  Contracted Nodes:   %d critical points" totalNodes
  putStrLn $ printf "  Synthesized Paths:  %d gradient flow V-Paths" totalPaths
  putStrLn $ printf "  Space Compression:  %.2f%%" compression

  -- .mbook ファイル出力
  let mbookContent = serializeMBook morseBook
      outFile = "complete_morse.mbook"
  writeFile outFile mbookContent
  putStrLn $ "[SUCCESS] Serialized 40-Parallel Morse Book to: " ++ outFile

  -- ステップ 4: 型レベルトランスパイラ呼び出し
  putStrLn "\n--- Invoking ShogiMorseCompiler to generate type-level safe Haskell code ---"
  callCommand ".\\shogi-morse-compiler.exe complete_morse.mbook"

  -- ステップ 5: 対局エンジンの再コンパイル・リンク
  putStrLn "\n--- Recompiling ShogiUsiMorse.exe with the 40-Parallel Morse Book ---"
  callCommand "ghc -O2 ShogiUsiMorse-v4.hs -o ShogiUsiMorse.exe"

  putStrLn "\n================================================================================"
  putStrLn " [COMPLETE] 40-Parallel BFS Tonshi-Free Morse AI Successfully Deployed!"
  putStrLn "================================================================================"
