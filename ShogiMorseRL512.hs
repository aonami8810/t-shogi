{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import System.Exit
import System.Process (callCommand)
import Control.Monad (when)
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Text.Printf (printf)

-- ============================================================================
-- 1. 基本将棋データ構造
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
  deriving (Eq, Ord)

instance Show Piece where
  show (Piece Sente p) = case p of
    Pawn    -> "P" ; Lance   -> "L" ; Knight  -> "N" ; Silver  -> "S"
    Gold    -> "G" ; Bishop  -> "B" ; Rook    -> "R" ; King    -> "K"
    PPawn   -> "+P"; PLance  -> "+L"; PKnight -> "+N"; PSilver -> "+S"
    PBishop -> "+B"; PRook   -> "+R"
  show (Piece Gote p) = case p of
    Pawn    -> "p" ; Lance   -> "l" ; Knight  -> "n" ; Silver  -> "s"
    Gold    -> "g" ; Bishop  -> "b" ; Rook    -> "r" ; King    -> "k"
    PPawn   -> "+p"; PLance  -> "+l"; PKnight -> "+n"; PSilver -> "+s"
    PBishop -> "+b"; PRook   -> "+r"

type Square = (Int, Int)
type Board = Map.Map Square Piece
type Hand = Map.Map PieceType Int
type PositionKey = (Player, Board, Map.Map Player Hand)

data State = State
  { turn    :: Player
  , board   :: Board
  , hands   :: Map.Map Player Hand
  , history :: Map.Map PositionKey Int
  } deriving (Show)

positionKey :: State -> PositionKey
positionKey State{..} = (turn, board, hands)

data Move 
  = NormalMove Square Square Bool
  | DropMove PieceType Square
  deriving (Eq, Show, Ord)

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

emptyHand :: Hand
emptyHand = Map.fromList [(p, 0) | p <- [Pawn, Lance, Knight, Silver, Gold, Bishop, Rook]]

initBoard :: Board
initBoard = Map.fromList $
  [ ((9,1), Piece Gote Lance),  ((8,1), Piece Gote Knight), ((7,1), Piece Gote Silver), ((6,1), Piece Gote Gold)
  , ((5,1), Piece Gote King),   ((4,1), Piece Gote Gold),   ((3,1), Piece Gote Silver), ((2,1), Piece Gote Knight)
  , ((1,1), Piece Gote Lance)
  , ((8,2), Piece Gote Rook),   ((2,2), Piece Gote Bishop)
  ] ++ [((i,3), Piece Gote Pawn) | i <- [1..9]] ++
  [((i,7), Piece Sente Pawn) | i <- [1..9]] ++
  [ ((8,8), Piece Sente Bishop), ((2,8), Piece Sente Rook)
  , ((9,9), Piece Sente Lance),  ((8,9), Piece Sente Knight), ((7,9), Piece Sente Silver), ((6,9), Piece Sente Gold)
  , ((5,9), Piece Sente King),   ((4,9), Piece Sente Gold),   ((3,9), Piece Sente Silver), ((2,9), Piece Sente Knight)
  , ((1,9), Piece Sente Lance)
  ]

initState :: State
initState =
  let st = State Sente initBoard (Map.fromList [(Sente, emptyHand), (Gote, emptyHand)]) Map.empty
  in st { history = Map.singleton (positionKey st) 1 }

applyMove :: Move -> State -> State
applyMove (NormalMove from to promo) State{..} =
  let piece = Map.lookup from board
      captured = Map.lookup to board
      boardWithSourceRemoved = Map.delete from board
      newPiece = case piece of
        Just (Piece plr pt) -> if promo then Piece plr (promote pt) else Piece plr pt
        Nothing -> Piece turn Pawn
      newBoard = Map.insert to newPiece boardWithSourceRemoved

      newHands = case captured of
        Just (Piece _ pt) ->
          let rawPt = demote pt
              handMap = fromMaybe emptyHand (Map.lookup turn hands)
              updatedHand = Map.insertWith (+) rawPt 1 handMap
          in Map.insert turn updatedHand hands
        Nothing -> hands

      nextSt = State (opponent turn) newBoard newHands history
      newKey = positionKey nextSt
  in nextSt { history = Map.insertWith (+) newKey 1 history }
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

      nextSt = State (opponent turn) newBoard newHands history
      newKey = positionKey nextSt
  in nextSt { history = Map.insertWith (+) newKey 1 history }

-- ============================================================================
-- 2. 合法手生成 & F5シジジーフィルター
-- ============================================================================

isOutOfBounds :: Square -> Bool
isOutOfBounds (x, y) = x < 1 || x > 9 || y < 1 || y > 9

isPathClear :: Square -> Square -> Board -> Bool
isPathClear (x1, y1) (x2, y2) bd =
  let dx = signum (x2 - x1)
      dy = signum (y2 - y1)
      path = takeWhile (/= (x2, y2)) $ drop 1 $ iterate (\(cx, cy) -> (cx + dx, cy + dy)) (x1, y1)
  in all (\sq -> not (Map.member sq bd)) path

isSquareUnderAttack :: Player -> Square -> Board -> Bool
isSquareUnderAttack attackerPlayer targetSq bd =
  any (\(sq, Piece _ pt) -> canPieceReach sq pt) attackerPieces
  where
    attackerPieces = [ (sq, p) | (sq, p@(Piece plr _)) <- Map.toList bd, plr == attackerPlayer ]
    canPieceReach (x, y) pt =
      let dx = fst targetSq - x
          dy = snd targetSq - y
          absDx = abs dx
          absDy = abs dy
          dirY = if attackerPlayer == Sente then -1 else 1
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

isKingUnderAttack :: Player -> Board -> Bool
isKingUnderAttack plr bd =
  let kingPos = listToMaybe [sq | (sq, Piece p pt) <- Map.toList bd, p == plr, pt == King]
  in case kingPos of
    Just kSq -> isSquareUnderAttack (opponent plr) kSq bd
    Nothing  -> True

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
          wallPenalties = sum [ if outOfBounds sq || Map.member sq bd then 0.0 else 1.5 | sq <- surroundings ]
      in influenceSum + wallPenalties
  where
    findKing plr b = listToMaybe [sq | (sq, Piece p pt) <- Map.toList b, p == plr, pt == King]
    dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
    outOfBounds (x, y) = x < 1 || x > 9 || y < 1 || y > 9

evaluateState :: Player -> State -> Double
evaluateState player st =
  let bd = board st
      myDanger = evalMorsePotential player bd
      enemyDanger = evalMorsePotential (opponent player) bd
      morseAdvantage = (enemyDanger - myDanger) * 10.0
      materialAdvantage = evalMaterial player st
  in morseAdvantage + materialAdvantage

-- ============================================================================
-- 4. 3手先読み ネガマックス α-β 探索エンジン (3-Ply Lookahead)
-- ============================================================================

-- | ネガマックス α-β 探索 (自手番の視点でのスコアを返す)
searchAlphaBeta :: Int -> Double -> Double -> State -> (Double, Maybe Move)
searchAlphaBeta 0 _ _ st =
  (evaluateState (turn st) st, Nothing)
searchAlphaBeta depth alpha beta st =
  let rawMoves = generateMoves st
      f5Moves = f5SyzygyFilter st rawMoves
      (_sennichite, safeMoves) = partition (\m -> 
        let nextSt = applyMove m st
        in isSennichite (positionKey nextSt) (history nextSt)
        ) f5Moves
  in if null f5Moves
     then (-10000.0, Nothing) -- 詰み（自玉が詰まされている）
     else if null safeMoves
       then (0.0, Nothing)     -- 千日手引き分け
       else
         -- 枝刈り効率化のため、1手先静的評価で降順ソート (Move Ordering)
         let orderedMoves = sortBy (\m1 m2 ->
               let s1 = evaluateState (turn st) (applyMove m1 st)
                   s2 = evaluateState (turn st) (applyMove m2 st)
               in compare s2 s1) safeMoves
             -- 上位候補手を重点探索（高速α-β枝刈り）
             topMoves = take (if depth >= 2 then 5 else 4) orderedMoves
         in loop topMoves alpha Nothing
  where
    loop [] curAlpha bestMv = (curAlpha, bestMv)
    loop (m:ms) curAlpha bestMv =
      let nextSt = applyMove m st
          (childScore, _) = searchAlphaBeta (depth - 1) (-beta) (-curAlpha) nextSt
          score = -childScore
      in if score >= beta
         then (score, Just m) -- βカット（枝刈り）
         else if score > curAlpha
           then loop ms score (Just m)
           else loop ms curAlpha bestMv

-- | 3手先読みによる各手の評価 (自手 m -> 相手最善応手 -> 自手最善反撃 -> 評価)
evalMove3Ply :: State -> Move -> Double
evalMove3Ply st m =
  let st1 = applyMove m st
      -- 相手番の視点から2手先（深さ2）を探索
      (oppScore, _) = searchAlphaBeta 2 (-20000.0) 20000.0 st1
  in -oppScore -- 自手番の視点に戻す

-- | 3手先読み評価に基づいた深層モース曲率（Hessianノルム：読み筋のばらつき）
calculateCurvature3Ply :: State -> [(Move, Double)] -> Double
calculateCurvature3Ply _ [] = 0.0
calculateCurvature3Ply _ scoredMoves =
  let scores = map snd scoredMoves
      avgScore = sum scores / fromIntegral (length scores)
      variance = sum [ (s - avgScore) ^ 2 | s <- scores ] / fromIntegral (length scores)
  in sqrt variance

-- ============================================================================
-- 5. 3手先読み 自己対局強化学習プロセス (RL with 3-Ply Lookahead)
-- ============================================================================

data NodeStats = NodeStats
  { qValue     :: Double  -- 行動価値/評価 Q(s)
  , visitCount :: Int     -- 訪問回数 N(s)
  , curvature  :: Double  -- 局所曲率 κ(s)
  } deriving (Show)

type RLTable = Map.Map PositionKey NodeStats

data StepRecord = StepRecord
  { stepPly      :: Int
  , stepState    :: State
  , stepMove     :: Move
  , stepCurv     :: Double
  , stepPot      :: Double
  , step3PlyEval :: Double
  } deriving (Show)

nextRand :: Int -> (Int, Int)
nextRand seed =
  let a = 1103515245
      c = 12345
      m = 2147483648
      newSeed = (a * seed + c) `mod` m
  in (newSeed, newSeed)

-- | 3手先読みと強化学習Q値を融合した適応指し手選択
selectMoveRL3Ply :: State -> [Move] -> RLTable -> Double -> Int -> (Move, Double, Double, Int)
selectMoveRL3Ply st safeMoves rlTable temperature seed =
  let -- 1. まず静的評価で有望手トップ8に事前スクリーニング
      sortedBy1Ply = sortBy (\m1 m2 ->
        let s1 = evaluateState (turn st) (applyMove m1 st)
            s2 = evaluateState (turn st) (applyMove m2 st)
        in compare s2 s1) safeMoves
      topCandidates = take 6 sortedBy1Ply

      -- 2. 上位候補手に対して3手先読み（α-β）を実行
      scoredMoves3Ply = map (\m -> (m, evalMove3Ply st m)) topCandidates
      curv = calculateCurvature3Ply st scoredMoves3Ply
      
      -- 3. Q値・訪問数・UCBボーナスの合算
      scoredMoves = map (\(m, score3Ply) -> 
        let nextSt = applyMove m st
            key = positionKey nextSt
            stats = Map.findWithDefault (NodeStats score3Ply 0 curv) key rlTable
            q = qValue stats
            visits = fromIntegral (visitCount stats)
            ucbBonus = 30.0 * sqrt (log (visits + 2.0) / (visits + 1.0))
            combinedScore = 0.8 * score3Ply + 0.2 * q + ucbBonus + 0.05 * curv
        in (m, combinedScore, score3Ply)
        ) scoredMoves3Ply
      
      -- 4. ソフトマックスによる確率的重点サンプリング
      maxScore = maximum (map (\(_, s, _) -> s) scoredMoves)
      expWeights = map (\(m, s, s3) -> (m, exp ((s - maxScore) / max 1.0 temperature), s3)) scoredMoves
      totalWeight = sum (map (\(_, w, _) -> w) expWeights)
      (seed1, rInt) = nextRand seed
      randVal = (fromIntegral (rInt `mod` 1000000) / 1000000.0) * totalWeight
      (pickedMove, picked3Ply) = pickWeighted randVal 0.0 expWeights
  in (pickedMove, picked3Ply, curv, seed1)
  where
    pickWeighted _ _ [(m, _, s3)] = (m, s3)
    pickWeighted _ _ [] = let m = head safeMoves in (m, evaluateState (turn st) (applyMove m st))
    pickWeighted rv acc ((m, w, s3):rest)
      | rv <= acc + w = (m, s3)
      | otherwise     = pickWeighted rv (acc + w) rest

-- | 3手先読みで1手ずつ512手まで自己対局を伸ばしていく (リアルタイムストリーミング出力)
runEpisode3Ply512 :: State -> Int -> Int -> RLTable -> Double -> [StepRecord] -> IO (Double, [StepRecord], Int)
runEpisode3Ply512 st ply maxPly rlTable temp accSteps
  | ply > maxPly = do
      let finalPot = evaluateState (turn st) st
      putStrLn $ printf "    [Ply %3d] Reached 512-Ply Target! (Terminal Eval: %+8.2f)" maxPly finalPot
      return (finalPot, reverse accSteps, 0)
  | otherwise = do
      let rawMoves = generateMoves st
          f5Moves = f5SyzygyFilter st rawMoves
          (_sennichite, safeMoves) = partition (\m -> 
            let nextSt = applyMove m st
            in isSennichite (positionKey nextSt) (history nextSt)
            ) f5Moves
      if null f5Moves
        then do
          putStrLn $ printf "    [Ply %3d] Checkmate / Mate Resolution!" ply
          return (-10000.0, reverse accSteps, -1)
        else if null safeMoves
          then do
            putStrLn $ printf "    [Ply %3d] Sennichite Repetition Detected (Draw)" ply
            return (0.0, reverse accSteps, 1)
          else do
            let curPot = evaluateState (turn st) st
                (bestMove, eval3Ply, curCurv, _) = selectMoveRL3Ply st safeMoves rlTable temp (ply * 7919 + 104729)
                nextSt = applyMove bestMove st
                rec = StepRecord ply st bestMove curCurv curPot eval3Ply

            -- 1手ずつリアルタイムに進捗を表示（節目と定跡伸長ポイント）
            when (ply == 1 || ply `mod` 20 == 0 || ply == maxPly) $
              putStrLn $ printf "    [Ply %3d/%3d] Move: %-6s | 3-Ply Eval: %+8.2f | Curvature κ(s): %7.2f"
                         ply maxPly (showMove bestMove) eval3Ply curCurv

            runEpisode3Ply512 nextSt (ply + 1) maxPly rlTable temp (rec : accSteps)

-- | 報酬の逆伝播
backpropReward :: Double -> [StepRecord] -> RLTable -> RLTable
backpropReward terminalReward steps rlTable =
  foldl' updateStep rlTable (zip [0..] (reverse steps))
  where
    gamma = 0.995
    updateStep table (idx, StepRecord{..}) =
      let key = positionKey stepState
          reward = terminalReward * (gamma ^ idx)
          curStats = Map.findWithDefault (NodeStats step3PlyEval 0 stepCurv) key table
          newVisits = visitCount curStats + 1
          alpha = 1.0 / fromIntegral (newVisits + 1)
          newQ = (1.0 - alpha) * qValue curStats + alpha * reward
          updatedStats = curStats { qValue = newQ, visitCount = newVisits, curvature = stepCurv }
      in Map.insert key updatedStats table

-- ============================================================================
-- 6. モース収縮アルゴリズム (Morse Contraction)
-- ============================================================================

data CriticalNodeType = Saddle | Maxima | Minimum
  deriving (Eq, Show)

data BookNode = BookNode
  { bNodeId    :: String
  , bNodeType  :: CriticalNodeType
  , bPotential :: Double
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
      intermediateNodes = [ BookNode ("Saddle_Ply_" ++ show p ++ "_" ++ showMove (stepMove rec)) Saddle (step3PlyEval rec)
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
          threshold = max 20.0 (avgCurv * 1.3)
      in filter (\(p, r) -> p `mod` 25 == 0 || (stepCurv r > threshold && p `mod` 10 == 0)) xs

serializeMBook :: MorseBook -> String
serializeMBook MorseBook{..} =
  let nodeLines = [ "NODE:" ++ bNodeId ++ "," ++ show bNodeType ++ "," ++ show bPotential | BookNode{..} <- bookNodes ]
      pathLines = [ "PATH:" ++ bpFrom ++ "->" ++ bpTo ++ ":" ++ intercalate ">" bpMoves | BookVPath{..} <- bookPaths ]
  in unlines (nodeLines ++ pathLines)

-- ============================================================================
-- 7. メイン実行ルーチン (3手先読み 自己対局強化学習ドライバ)
-- ============================================================================

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "================================================================================"
  putStrLn "   Topological Shogi AI - 3-Ply Lookahead Self-Play Morse Contraction RL Engine"
  putStrLn "================================================================================"
  putStrLn "Initializing 3-Ply Minimax (Negamax Alpha-Beta) and Reinforcement Learning table..."

  let targetMaxPly = 512
      numEpisodes = 3
      initialRLTable = Map.empty

  -- 3手先読み自己対局エピソードループ
  (finalRLTable, bestTrajectory, finalReward) <- runLearningLoop 1 numEpisodes initialRLTable [] 0.0 targetMaxPly

  putStrLn "\n================================================================================"
  putStrLn " [Morse Contraction] Performing Homological Reduction on 3-Ply Trajectory..."
  putStrLn "================================================================================"
  
  let morseBook = contractTrajectoryToMorseBook bestTrajectory finalReward
      totalNodes = length (bookNodes morseBook)
      totalPaths = length (bookPaths morseBook)
      totalPlies = length bestTrajectory

  putStrLn $ printf "Total Plies Extended with 3-Ply Lookahead: %d moves (Target: %d)" totalPlies targetMaxPly
  putStrLn $ printf "Contracted Critical Nodes (Saddles/Minima): %d nodes" totalNodes
  putStrLn $ printf "Synthesized Morse V-Paths: %d paths" totalPaths
  let compression = (1.0 - (fromIntegral totalNodes / fromIntegral (max 1 totalPlies))) * 100.0 :: Double
  putStrLn $ printf "Homological Search Space Compression: %.2f%%" compression

  -- .mbook ファイルへのシリアライズ保存
  let mbookContent = serializeMBook morseBook
      outFile = "complete_morse.mbook"
  writeFile outFile mbookContent
  putStrLn $ "\n[SUCCESS] Serialized 3-Ply Lookahead Complete Morse Book to: " ++ outFile

  -- 型レベルコンパイラによる CompiledMorseBook.hs の生成
  putStrLn "\n--- Invoking ShogiMorseCompiler to generate type-level safe Haskell code ---"
  callCommand ".\\shogi-morse-compiler.exe complete_morse.mbook"

  -- 対局エンジンの再コンパイル（3手先読み完全定跡を静的リンク）
  putStrLn "\n--- Recompiling ShogiUsiMorse.exe with the 3-Ply Complete Morse Book ---"
  callCommand "ghc -O2 ShogiUsiMorse-v4.hs -o ShogiUsiMorse.exe"

  putStrLn "\n================================================================================"
  putStrLn " [3-PLY RL COMPLETE] 3-Ply Lookahead Complete Morse Book Successfully Built & Linked!"
  putStrLn "================================================================================"

runLearningLoop :: Int -> Int -> RLTable -> [StepRecord] -> Double -> Int -> IO (RLTable, [StepRecord], Double)
runLearningLoop ep maxEp table bestTraj bestRew maxPly
  | ep > maxEp = return (table, bestTraj, bestRew)
  | otherwise = do
      let temp = 6.0 * (0.7 ^ (ep - 1))
      putStrLn $ printf "\n>>> [Episode %d/%d] 3-Ply Lookahead Frontier Extension (Temperature: %.2f)..." ep maxEp temp
      
      (reward, steps, status) <- runEpisode3Ply512 initState 1 maxPly table temp []
      let plyReached = length steps
          statusStr = case status of
            (-1) -> "Checkmate / Mate Resolution"
            1    -> "Sennichite (Repetition Draw Avoided)"
            _    -> "Max Depth (512 Plies) Frontier Reached"

      putStrLn $ printf "    ===> Episode %d Outcome: %s at Ply %d (Reward: %+.2f)" ep statusStr plyReached reward
      
      let newTable = backpropReward reward steps table
          (newBestTraj, newBestRew) = 
            if plyReached >= length bestTraj 
            then (steps, reward) 
            else (bestTraj, bestRew)

      runLearningLoop (ep + 1) maxEp newTable newBestTraj newBestRew maxPly
