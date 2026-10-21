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
-- 2. 合法手生成 & 王手・打ち歩詰め・千日手判定
-- ============================================================================

isOutOfBounds :: Square -> Bool
isOutOfBounds (x, y) = x < 1 || x > 9 || y < 1 || y > 9

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

isSquareAttackedBy :: Player -> Square -> Board -> Bool
isSquareAttackedBy attacker target bd =
  any attacksFrom (Map.toList bd)
  where
    attacksFrom (sq, Piece p pt)
      | p /= attacker = False
      | otherwise = target `elem` pieceAttackSquares p pt sq bd

pieceAttackSquares :: Player -> PieceType -> Square -> Board -> [Square]
pieceAttackSquares plr pt from@(fx, fy) bd =
  let dirY = if plr == Sente then -1 else 1
      slide (dx, dy) = go (fx + dx, fy + dy)
        where
          go sq@(cx, cy)
            | isOutOfBounds sq = []
            | otherwise = case Map.lookup sq bd of
                Nothing -> sq : go (cx + dx, cy + dy)
                Just (Piece p _) -> if p == plr then [] else [sq]
      step (dx, dy) =
        let sq = (fx + dx, fy + dy)
        in if not (isOutOfBounds sq) && not (isFriendly sq) then [sq] else []
      isFriendly sq = case Map.lookup sq bd of
        Just (Piece p _) -> p == plr
        Nothing          -> False
  in case pt of
    Pawn   -> step (0, dirY)
    Lance  -> slide (0, dirY)
    Knight -> [ sq | dx <- [-1, 1], let sq = (fx + dx, fy + 2 * dirY), not (isOutOfBounds sq), not (isFriendly sq) ]
    Silver -> concatMap step [(0, dirY), (1, dirY), (-1, dirY), (1, -dirY), (-1, -dirY)]
    Gold   -> concatMap step [(0, dirY), (1, dirY), (-1, dirY), (1, 0), (-1, 0), (0, -dirY)]
    King   -> concatMap step [(dx, dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0,0)]
    Rook   -> concatMap slide [(0, 1), (0, -1), (1, 0), (-1, 0)]
    Bishop -> concatMap slide [(1, 1), (1, -1), (-1, 1), (-1, -1)]
    PRook  -> concatMap slide [(0, 1), (0, -1), (1, 0), (-1, 0)] ++ concatMap step [(1, 1), (1, -1), (-1, 1), (-1, -1)]
    PBishop -> concatMap slide [(1, 1), (1, -1), (-1, 1), (-1, -1)] ++ concatMap step [(0, 1), (0, -1), (1, 0), (-1, 0)]
    PPawn   -> concatMap step [(0, dirY), (1, dirY), (-1, dirY), (1, 0), (-1, 0), (0, -dirY)]
    PLance  -> concatMap step [(0, dirY), (1, dirY), (-1, dirY), (1, 0), (-1, 0), (0, -dirY)]
    PKnight -> concatMap step [(0, dirY), (1, dirY), (-1, dirY), (1, 0), (-1, 0), (0, -dirY)]
    PSilver -> concatMap step [(0, dirY), (1, dirY), (-1, dirY), (1, 0), (-1, 0), (0, -dirY)]

isKingUnderAttack :: Player -> Board -> Bool
isKingUnderAttack plr bd =
  case listToMaybe [sq | (sq, Piece p pt) <- Map.toList bd, p == plr, pt == King] of
    Nothing -> True
    Just kingSq -> isSquareAttackedBy (opponent plr) kingSq bd

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
-- 3. モースポテンシャル & 駒得評価関数
-- ============================================================================

pieceValue :: PieceType -> Double
pieceValue Pawn    = 100.0 ; pieceValue Lance   = 300.0 ; pieceValue Knight  = 400.0
pieceValue Silver  = 500.0 ; pieceValue Gold    = 600.0 ; pieceValue Bishop  = 800.0
pieceValue Rook    = 1000.0 ; pieceValue King   = 15000.0
pieceValue PPawn   = 600.0 ; pieceValue PLance  = 600.0 ; pieceValue PKnight = 600.0
pieceValue PSilver = 600.0 ; pieceValue PBishop = 1100.0 ; pieceValue PRook   = 1300.0

evalMaterial :: Player -> State -> Double
evalMaterial plr st =
  let bd = board st
      hnds = hands st
      boardScore (Piece p pt) = (if p == plr then 1.0 else -1.0) * pieceValue pt
      bdTotal = sum [ boardScore piece | (_, piece) <- Map.toList bd ]
      myHand = fromMaybe emptyHand (Map.lookup plr hnds)
      enemyHand = fromMaybe emptyHand (Map.lookup (opponent plr) hnds)
      myHandTotal = sum [ fromIntegral cnt * pieceValue pt | (pt, cnt) <- Map.toList myHand ]
      enemyHandTotal = sum [ fromIntegral cnt * pieceValue pt | (pt, cnt) <- Map.toList enemyHand ]
  in bdTotal + myHandTotal - enemyHandTotal

evalMorsePotential :: Player -> Board -> Double
evalMorsePotential player bd =
  let kingPos = findKing player bd
  in case kingPos of
    Nothing -> 10000.0
    Just (kx, ky) ->
      let enemyPieces = Map.toList $ Map.filter (\(Piece p _) -> p /= player) bd
          influenceSum = sum [ 10.0 / (fromIntegral (dist (kx, ky) sq) + 0.1) | (sq, _) <- enemyPieces ]
          surroundings = [(kx + dx, ky + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]
          wallPenalties = sum [ if isOutOfBounds sq || Map.member sq bd then 0.0 else 1.5 | sq <- surroundings ]
      in influenceSum + wallPenalties
  where
    findKing plr b = listToMaybe [sq | (sq, Piece p pt) <- Map.toList b, p == plr, pt == King]
    dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

evaluateState :: Player -> State -> Double
evaluateState player st =
  let bd = board st
      myDanger = evalMorsePotential player bd
      enemyDanger = evalMorsePotential (opponent player) bd
      morseAdvantage = (enemyDanger - myDanger) * 10.0
      materialAdvantage = evalMaterial player st
  in morseAdvantage + materialAdvantage

-- ============================================================================
-- 4. 超極深層 41-Ply 適応的ビーム α-β 探索エンジン (Lookahead 33..41 Plies)
-- ============================================================================

-- | 超極深層 α-β 探索: 5手先以上は最善筋（PV手）への単一線追従を行うことで、
-- 深さ 41 でも探索空間が指数爆発せず、1手あたり数ミリ秒で完結する
searchAlphaBetaDeep :: Int -> Double -> Double -> State -> (Double, Maybe Move)
searchAlphaBetaDeep 0 _ _ st =
  (evaluateState (turn st) st, Nothing)
searchAlphaBetaDeep depth alpha beta st =
  let rawMoves = generateMoves st
      f5Moves = f5SyzygyFilter st rawMoves
      (_sennichite, safeMoves) = partition (\m -> 
        let nextSt = applyMove m st
        in isSennichite (positionKey nextSt) (history nextSt)
        ) f5Moves
  in if null f5Moves
     then (-10000.0, Nothing) -- 詰み
     else if null safeMoves
       then (0.0, Nothing)     -- 千日手
       else
         let orderedMoves = sortBy (\m1 m2 ->
               let s1 = evaluateState (turn st) (applyMove m1 st)
                   s2 = evaluateState (turn st) (applyMove m2 st)
               in compare s2 s1) safeMoves
             beamWidth
               | depth >= 4 = 1  -- 4手先以上の超極深層では最善筋（PV: Principal Variation）をシャープに1本追従
               | otherwise  = 2  -- 1〜3手先の中浅層では上位2手を評価
             topMoves = take beamWidth orderedMoves
         in loop topMoves alpha Nothing
  where
    loop [] curAlpha bestMv = (curAlpha, bestMv)
    loop (m:ms) curAlpha bestMv =
      let nextSt = applyMove m st
          (childScore, _) = searchAlphaBetaDeep (depth - 1) (-beta) (-curAlpha) nextSt
          score = -childScore
      in if score >= beta
         then (score, Just m)
         else if score > curAlpha
              then loop ms score (Just m)
              else loop ms curAlpha bestMv

-- | 深さ D (33..41) による候補手評価
evalMoveDeep :: Int -> State -> Move -> Double
evalMoveDeep depth st m =
  let st1 = applyMove m st
      (oppScore, _) = searchAlphaBetaDeep (depth - 1) (-20000.0) 20000.0 st1
  in -oppScore

calculateCurvatureDeep :: State -> [(Move, Double)] -> Double
calculateCurvatureDeep _ [] = 0.0
calculateCurvatureDeep _ scoredMoves =
  let scores = map snd scoredMoves
      avgScore = sum scores / fromIntegral (length scores)
      variance = sum [ (s - avgScore) ^ 2 | s <- scores ] / fromIntegral (length scores)
  in sqrt variance

-- ============================================================================
-- 5. 16並列 強化学習データ構造 ＆ ワーカーロジック
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
  } deriving (Show)

nextRand :: Int -> (Int, Int)
nextRand seed =
  let a = 1103515245
      c = 12345
      m = 2147483648
      newSeed = (a * seed + c) `mod` m
  in (newSeed, newSeed)

selectMoveRLDeep :: Int -> State -> [Move] -> RLTable -> Double -> Int -> (Move, Double, Double, Int)
selectMoveRLDeep depth st safeMoves rlTable temperature seed =
  let sortedBy1Ply = sortBy (\m1 m2 ->
        let s1 = evaluateState (turn st) (applyMove m1 st)
            s2 = evaluateState (turn st) (applyMove m2 st)
        in compare s2 s1) safeMoves
      topCandidates = take 2 sortedBy1Ply

      scoredMovesDeep = map (\m -> (m, evalMoveDeep depth st m)) topCandidates
      curv = calculateCurvatureDeep st scoredMovesDeep

      scoredMoves = map (\(m, scoreDeep) -> 
        let nextSt = applyMove m st
            key = positionKey nextSt
        in case Map.lookup key rlTable of
             Just ns -> 
               let totalVisits = fromIntegral (sum [ visitCount n | n <- Map.elems rlTable ] + 1)
                   ucb = 50.0 * sqrt (log totalVisits / fromIntegral (visitCount ns + 1))
                   combined = (qValue ns * 0.4) + (scoreDeep * 0.6) + ucb
               in (m, combined, scoreDeep)
             Nothing -> (m, scoreDeep, scoreDeep)
        ) scoredMovesDeep

      (rVal, nextS) = nextRand seed
      bestMove = 
        if temperature < 0.1 
        then let sorted = sortBy (\(_, s1, _) (_, s2, _) -> compare s2 s1) scoredMoves
             in case sorted of
                  ((m, _, _):_) -> m
                  []            -> case safeMoves of (m:_) -> m; [] -> NormalMove (1,1) (1,1) False
        else 
          let maxVal = maximum [ c | (_, c, _) <- scoredMoves ]
              expScores = [ (m, exp ((c - maxVal) / max 0.1 (temperature * 20.0))) | (m, c, _) <- scoredMoves ]
              totalExp = sum [ e | (_, e) <- expScores ]
              pickVal = (fromIntegral (rVal `mod` 10000) / 10000.0) * totalExp
              pick [] = case safeMoves of (m:_) -> m; [] -> NormalMove (1,1) (1,1) False
              pick ((m, e):rest)
                | pickVal <= e = m
                | otherwise    = pick rest
          in pick expScores

      chosenScore = fromMaybe 0.0 (lookup bestMove [ (m, s) | (m, _, s) <- scoredMoves ])
  in (bestMove, curv, chosenScore, nextS)

runWorkerEpisode :: Int -> Int -> State -> Int -> Int -> RLTable -> Double -> [StepRecord] -> Int -> IO WorkerResult
runWorkerEpisode workerId depth st currentPly maxPly rlTable temp steps seed
  | currentPly > maxPly = do
      let terminalEval = evaluateState (turn st) st
      return $ WorkerResult workerId terminalEval (reverse steps) 0 (currentPly - 1)
  | otherwise = do
      let rawMoves = generateMoves st
          f5Filtered = f5SyzygyFilter st rawMoves
          (_sennichite, safeMoves) = partition (\m ->
            let nxt = applyMove m st
            in isSennichite (positionKey nxt) (history nxt)
            ) f5Filtered

      if null f5Filtered
        then return $ WorkerResult workerId (-10000.0) (reverse steps) (-1) currentPly
        else if null safeMoves
          then return $ WorkerResult workerId 0.0 (reverse steps) 1 currentPly
          else do
            let (chosenMove, curv, evalScore, nextS) = selectMoveRLDeep depth st safeMoves rlTable temp seed
                nextState = applyMove chosenMove st
                rec = StepRecord currentPly st chosenMove curv evalScore depth
            runWorkerEpisode workerId depth nextState (currentPly + 1) maxPly rlTable temp (rec : steps) nextS

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
-- 6. トポロジカル・モース収縮
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
      terminalNode = BookNode ("Minimum_Ply_" ++ show totalSteps ++ "_Convergence") terminalType finalReward
      
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
    findCriticalPlies :: [(Int, StepRecord)] -> [(Int, StepRecord)]
    findCriticalPlies [] = []
    findCriticalPlies xs =
      let avgCurv = sum [ stepCurv r | (_, r) <- xs ] / fromIntegral (length xs)
          threshold = max 20.0 (avgCurv * 1.25)
      in filter (\(p, r) -> p `mod` 25 == 0 || (stepCurv r > threshold && p `mod` 10 == 0)) xs

serializeMBook :: MorseBook -> String
serializeMBook MorseBook{..} =
  let nodeLines = [ "NODE:" ++ bNodeId ++ "," ++ show bNodeType ++ "," ++ show bPotential | BookNode{..} <- bookNodes ]
      pathLines = [ "PATH:" ++ bpFrom ++ "->" ++ bpTo ++ ":" ++ intercalate ">" bpMoves | BookVPath{..} <- bookPaths ]
  in unlines (nodeLines ++ pathLines)

-- ============================================================================
-- 7. 16並列 カリキュラム強化学習ドライバ (33から41まで2手ずつ深化)
-- ============================================================================

curriculumDepths :: [Int]
curriculumDepths = [33, 35, 37, 39, 41]

numWorkers :: Int
numWorkers = 16

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "================================================================================"
  putStrLn "   Topological Shogi AI - 16-Parallel Ultra-Deep Curriculum RL Engine"
  putStrLn "   Depths: 33 -> 35 -> 37 -> 39 -> 41 Plies (16-Workers Simultaneous Self-Play)"
  putStrLn "================================================================================"

  let targetMaxPly = 512
      initialRLTable = Map.empty

  -- 16並列カリキュラム深層自己対局ループの実行
  (finalRLTable, bestTrajectory, finalReward, deepestDepth) <- 
    runCurriculumParallelLoop curriculumDepths 1 (length curriculumDepths) initialRLTable [] 0.0 33 targetMaxPly

  putStrLn "\n================================================================================"
  putStrLn $ printf " [Morse Contraction] Reducing Ultra-Deep %d-Ply 16-Parallel Trajectory..." deepestDepth
  putStrLn "================================================================================"
  
  let morseBook = contractTrajectoryToMorseBook bestTrajectory finalReward
      totalNodes = length (bookNodes morseBook)
      totalPaths = length (bookPaths morseBook)
      totalPlies = length bestTrajectory

  putStrLn $ printf "Total Plies Extended: %d moves (Deepest Lookahead: %d-Ply)" totalPlies deepestDepth
  putStrLn $ printf "Contracted Critical Nodes (Saddles/Minima): %d nodes" totalNodes
  putStrLn $ printf "Synthesized Morse V-Paths: %d paths" totalPaths
  let compression = (1.0 - (fromIntegral totalNodes / fromIntegral (max 1 totalPlies))) * 100.0 :: Double
  putStrLn $ printf "Homological Search Space Compression: %.2f%%" compression

  -- .mbook ファイルへの保存
  let mbookContent = serializeMBook morseBook
      outFile = "complete_morse.mbook"
  writeFile outFile mbookContent
  putStrLn $ "\n[SUCCESS] Serialized 16-Parallel Ultra-Deep Morse Book to: " ++ outFile

  -- 型レベルトランスパイラ呼び出し
  putStrLn "\n--- Invoking ShogiMorseCompiler to generate type-level safe Haskell code ---"
  callCommand ".\\shogi-morse-compiler.exe complete_morse.mbook"

  -- 対局エンジンの再コンパイル・リンク
  putStrLn "\n--- Recompiling ShogiUsiMorse.exe with the 16-Parallel Deep Morse Book ---"
  callCommand "ghc -O2 ShogiUsiMorse-v4.hs -o ShogiUsiMorse.exe"

  putStrLn "\n================================================================================"
  putStrLn " [16-PARALLEL RL COMPLETE] Depths 33..41 Lookahead Complete Morse Book Built!"
  putStrLn "================================================================================"

runCurriculumParallelLoop :: [Int] -> Int -> Int -> RLTable -> [StepRecord] -> Double -> Int -> Int -> IO (RLTable, [StepRecord], Double, Int)
runCurriculumParallelLoop [] _ _ table bestTraj bestRew bestD _ = return (table, bestTraj, bestRew, bestD)
runCurriculumParallelLoop (d:ds) stage totalStages table bestTraj bestRew bestD maxPly = do
  let baseTemp = 3.0 * (0.85 ^ (stage - 1))
  putStrLn $ printf "\n>>> [Curriculum Stage %d/%d] Lookahead Depth: %2d Plies | Spawning %d Parallel Workers..."
                    stage totalStages d numWorkers

  -- 16並列ワーカーの起動
  mvars <- mapM (\_ -> newEmptyMVar) [1..numWorkers]
  let workerTasks = [ (wId, baseTemp * (0.92 + 0.01 * fromIntegral wId), 20011 * stage + 101 * wId)
                    | wId <- [1..numWorkers] ]

  mapM_ (\((wId, temp, seed), mvar) -> forkIO $ do
    res <- runWorkerEpisode wId d initState 1 maxPly table temp [] seed
    let statusStr :: String
        statusStr = case wrStatus res of
          (-1) -> "Checkmate"
          1    -> "Sennichite"
          _    -> "TargetReached"
    putStrLn $ printf "    [Worker %2d/%d] Finished at Ply %3d | Outcome: %-13s | Reward: %+8.1f"
                      wId numWorkers (wrPlyReached res) statusStr (wrReward res)
    putMVar mvar res
    ) (zip workerTasks mvars)

  -- 16個のワーカーの完了結果を回収
  results <- mapM takeMVar mvars

  -- 16本の軌道をすべて強化学習テーブルに逆伝播マージ
  let updatedTable = foldl' (\tbl wr -> backpropReward (wrReward wr) (wrSteps wr) tbl) table results
      
      -- 最も深く伸びた最良軌道の選択
      bestInStage = foldl1 (\best current ->
        if wrPlyReached current > wrPlyReached best 
        then current 
        else if wrPlyReached current == wrPlyReached best && wrReward current > wrReward best
             then current
             else best
        ) results

      (newBestTraj, newBestRew, newBestD) =
        if wrPlyReached bestInStage >= length bestTraj
        then (wrSteps bestInStage, wrReward bestInStage, d)
        else (bestTraj, bestRew, bestD)

      avgPlies = fromIntegral (sum [ wrPlyReached r | r <- results ]) / fromIntegral numWorkers :: Double

  putStrLn $ printf "    ===> Stage %d (Depth %2d) Summary: %d Parallel Games Merged | Max: %3d Plies | Avg: %.1f Plies"
                    stage d numWorkers (wrPlyReached bestInStage) avgPlies

  runCurriculumParallelLoop ds (stage + 1) totalStages updatedTable newBestTraj newBestRew newBestD maxPly
