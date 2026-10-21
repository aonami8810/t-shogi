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
      isFriendly sq = case Map.lookup sq bd of
        Just (Piece p _) -> p == plr
        Nothing          -> False
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
-- 4. 深層適応的ビーム反復深化 α-β 探索エンジン (Deep Lookahead: 5, 7, 9, 11, 13, 15)
-- ============================================================================

-- | 適応的ビーム α-β 探索 (自手番の視点でのスコアと最善手)
-- 深さに応じて展開候補数を動的に調整し、深さ15でも爆発せず高速に収束させる
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
         -- Move Ordering: 1手先静的評価順にソート
         let orderedMoves = sortBy (\m1 m2 ->
               let s1 = evaluateState (turn st) (applyMove m1 st)
                   s2 = evaluateState (turn st) (applyMove m2 st)
               in compare s2 s1) safeMoves
             -- 適応的ビーム幅: 深い階層ほど最有力手群（PV手・対抗手）に絞り込んで探索爆発を防止
             beamWidth
               | depth >= 5 = 1  -- 5手先以上の深層では最善筋（PV: Principal Variation）をシャープに追従
               | depth >= 2 = 2  -- 2〜4手先の中層では上位2手の分岐を精査
               | otherwise  = 3  -- 1手先の浅層では上位3手を評価
             topMoves = take beamWidth orderedMoves
         in loop topMoves alpha Nothing
  where
    loop [] curAlpha bestMv = (curAlpha, bestMv)
    loop (m:ms) curAlpha bestMv =
      let nextSt = applyMove m st
          (childScore, _) = searchAlphaBetaDeep (depth - 1) (-beta) (-curAlpha) nextSt
          score = -childScore
      in if score >= beta
         then (score, Just m) -- βカット
         else if score > curAlpha
              then loop ms score (Just m)
              else loop ms curAlpha bestMv

-- | 深さ D (奇数深さ: 5, 7, 9, 11, 13, 15) による候補手評価
-- 手 m を指した後、相手の視点から (D-1) 手のα-β探索を実行
evalMoveDeep :: Int -> State -> Move -> Double
evalMoveDeep depth st m =
  let st1 = applyMove m st
      (oppScore, _) = searchAlphaBetaDeep (depth - 1) (-20000.0) 20000.0 st1
  in -oppScore

-- | 深層モース曲率 Hessian ノルム κ_D(s)
calculateCurvatureDeep :: State -> [(Move, Double)] -> Double
calculateCurvatureDeep _ [] = 0.0
calculateCurvatureDeep _ scoredMoves =
  let scores = map snd scoredMoves
      avgScore = sum scores / fromIntegral (length scores)
      variance = sum [ (s - avgScore) ^ 2 | s <- scores ] / fromIntegral (length scores)
  in sqrt variance

-- ============================================================================
-- 5. カリキュラム深層自己対局強化学習プロセス (Curriculum RL)
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

nextRand :: Int -> (Int, Int)
nextRand seed =
  let a = 1103515245
      c = 12345
      m = 2147483648
      newSeed = (a * seed + c) `mod` m
  in (newSeed, newSeed)

-- | 深層先読み評価と Q(s) を統合したボルツマン行動選択
selectMoveRLDeep :: Int -> State -> [Move] -> RLTable -> Double -> Int -> (Move, Double, Double, Int)
selectMoveRLDeep depth st safeMoves rlTable temperature seed =
  let -- 1. 1手静的評価で有望手トップ候補を抽出
      sortedBy1Ply = sortBy (\m1 m2 ->
        let s1 = evaluateState (turn st) (applyMove m1 st)
            s2 = evaluateState (turn st) (applyMove m2 st)
        in compare s2 s1) safeMoves
      topCandidates = take 5 sortedBy1Ply

      -- 2. 上位手に対して深さ D の先読み評価を実行
      scoredMovesDeep = map (\m -> (m, evalMoveDeep depth st m)) topCandidates
      curv = calculateCurvatureDeep st scoredMovesDeep

      -- 3. Q値と探索評価の合成
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
                  []            -> head safeMoves
        else 
          -- ボルツマン分布
          let maxVal = maximum [ c | (_, c, _) <- scoredMoves ]
              expScores = [ (m, exp ((c - maxVal) / max 0.1 (temperature * 20.0))) | (m, c, _) <- scoredMoves ]
              totalExp = sum [ e | (_, e) <- expScores ]
              pickVal = (fromIntegral (rVal `mod` 10000) / 10000.0) * totalExp
              pick [] = head safeMoves
              pick ((m, e):rest)
                | pickVal <= e = m
                | otherwise    = pick rest
          in pick expScores

      chosenScore = fromMaybe 0.0 (lookup bestMove [ (m, s) | (m, _, s) <- scoredMoves ])
  in (bestMove, curv, chosenScore, nextS)

-- | 1エピソードの深層フロンティア延伸 (最大512手)
runEpisodeDeep :: Int -> State -> Int -> Int -> RLTable -> Double -> [StepRecord] -> Int -> IO (Double, [StepRecord], Int)
runEpisodeDeep depth st currentPly maxPly rlTable temp steps seed
  | currentPly > maxPly = do
      let terminalEval = evaluateState (turn st) st
      putStrLn $ printf "    [Ply %d] Reached %d-Ply Frontier Target! (Terminal Eval: %+.2f)" (currentPly - 1) maxPly terminalEval
      return (terminalEval, reverse steps, 0)
  | otherwise = do
      let rawMoves = generateMoves st
          f5Filtered = f5SyzygyFilter st rawMoves
          (_sennichite, safeMoves) = partition (\m ->
            let nxt = applyMove m st
            in isSennichite (positionKey nxt) (history nxt)
            ) f5Filtered

      if null f5Filtered
        then do
          putStrLn $ printf "    [Ply %3d] Checkmate / Mate Resolution!" currentPly
          return (-10000.0, reverse steps, -1)
        else if null safeMoves
          then do
            putStrLn $ printf "    [Ply %3d] Repetition Loop (Sennichite) Avoided!" currentPly
            return (0.0, reverse steps, 1)
          else do
            let (chosenMove, curv, evalScore, nextS) = selectMoveRLDeep depth st safeMoves rlTable temp seed
                nextState = applyMove chosenMove st
                rec = StepRecord currentPly st chosenMove curv evalScore depth

            -- 進行状況のインターバル表示
            if currentPly == 1 || currentPly `mod` 20 == 0 || currentPly == maxPly
              then putStrLn $ printf "    [Ply %3d/%d] Move: %-7s | %2d-Ply Eval: %+8.2f | Curvature κ(s): %7.2f"
                                     currentPly maxPly (showMove chosenMove) depth evalScore curv
              else return ()

            runEpisodeDeep depth nextState (currentPly + 1) maxPly rlTable temp (rec : steps) nextS

-- | TD(λ) による終局報酬の多段階割引逆伝播
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
-- 6. トポロジカル・モース収縮 & 定跡シリアライズ
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
-- 7. カリキュラム深層先読み自己対局強化学習ドライバ (5 -> 7 -> 9 -> 11 -> 13 -> 15)
-- ============================================================================

curriculumDepths :: [Int]
curriculumDepths = [5, 7, 9, 11, 13, 15]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "================================================================================"
  putStrLn "   Topological Shogi AI - Progressive Lookahead Curriculum RL Engine"
  putStrLn "   Target Depths: 5 -> 7 -> 9 -> 11 -> 13 -> 15 Plies | Max Frontier: 512 Plies"
  putStrLn "================================================================================"

  let targetMaxPly = 512
      initialRLTable = Map.empty

  -- カリキュラム深層自己対局ループの実行
  (finalRLTable, bestTrajectory, finalReward, bestDepthReached) <- 
    runCurriculumLoop curriculumDepths 1 (length curriculumDepths) initialRLTable [] 0.0 5 targetMaxPly

  putStrLn "\n================================================================================"
  putStrLn $ printf " [Morse Contraction] Reducing Deep %d-Ply Lookahead Trajectory..." bestDepthReached
  putStrLn "================================================================================"
  
  let morseBook = contractTrajectoryToMorseBook bestTrajectory finalReward
      totalNodes = length (bookNodes morseBook)
      totalPaths = length (bookPaths morseBook)
      totalPlies = length bestTrajectory

  putStrLn $ printf "Total Plies Extended: %d moves (Deepest Lookahead: %d-Ply)" totalPlies bestDepthReached
  putStrLn $ printf "Contracted Critical Nodes (Saddles/Minima): %d nodes" totalNodes
  putStrLn $ printf "Synthesized Morse V-Paths: %d paths" totalPaths
  let compression = (1.0 - (fromIntegral totalNodes / fromIntegral (max 1 totalPlies))) * 100.0 :: Double
  putStrLn $ printf "Homological Search Space Compression: %.2f%%" compression

  -- .mbook ファイルへの保存
  let mbookContent = serializeMBook morseBook
      outFile = "complete_morse.mbook"
  writeFile outFile mbookContent
  putStrLn $ "\n[SUCCESS] Serialized Deep Progressive Morse Book to: " ++ outFile

  -- 型レベルトランスパイラ呼び出し
  putStrLn "\n--- Invoking ShogiMorseCompiler to generate type-level safe Haskell code ---"
  callCommand ".\\shogi-morse-compiler.exe complete_morse.mbook"

  -- 対局エンジンの再コンパイル・リンク
  putStrLn "\n--- Recompiling ShogiUsiMorse.exe with the Deep Lookahead Morse Book ---"
  callCommand "ghc -O2 ShogiUsiMorse-v4.hs -o ShogiUsiMorse.exe"

  putStrLn "\n================================================================================"
  putStrLn " [CURRICULUM RL COMPLETE] Depths 5->7->9->11->13->15 Self-Play RL Finished!"
  putStrLn "================================================================================"

runCurriculumLoop :: [Int] -> Int -> Int -> RLTable -> [StepRecord] -> Double -> Int -> Int -> IO (RLTable, [StepRecord], Double, Int)
runCurriculumLoop [] _ _ table bestTraj bestRew bestD _ = return (table, bestTraj, bestRew, bestD)
runCurriculumLoop (d:ds) stage totalStages table bestTraj bestRew bestD maxPly = do
  let temp = 5.0 * (0.8 ^ (stage - 1))
      seed = 42 + stage * 1007
  putStrLn $ printf "\n>>> [Curriculum Stage %d/%d] Lookahead Depth: %2d Plies (Temperature: %.2f)..." stage totalStages d temp
  
  (reward, steps, status) <- runEpisodeDeep d initState 1 maxPly table temp [] seed
  let plyReached = length steps
      statusStr :: String
      statusStr = case status of
        (-1) -> "Checkmate / Mate Resolution"
        1    -> "Sennichite (Repetition Draw Avoided)"
        _    -> "Max Depth (512 Plies) Frontier Reached"

  putStrLn $ printf "    ===> Stage %d (Depth %d) Outcome: %s at Ply %d (Reward: %+.2f)" stage d statusStr plyReached reward
  
  let newTable = backpropReward reward steps table
      (newBestTraj, newBestRew, newBestD) = 
        if plyReached >= length bestTraj 
        then (steps, reward, d) 
        else (bestTraj, bestRew, bestD)

  runCurriculumLoop ds (stage + 1) totalStages newTable newBestTraj newBestRew newBestD maxPly
