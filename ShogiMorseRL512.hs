{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import System.Exit
import System.Process (callCommand)
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
-- 3. モースポテンシャル & ヘッセ行列曲率（Curvature）
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

-- | 局所的モース曲率（Hessianノルム近似：後続局面へのポテンシャル勾配の分散）
calculateCurvature :: State -> [Move] -> Double
calculateCurvature st legalMoves
  | null legalMoves = 0.0
  | otherwise =
      let currentScore = evaluateState (turn st) st
          nextScores = [ evaluateState (turn st) (applyMove m st) | m <- legalMoves ]
          gradients = [ s - currentScore | s <- nextScores ]
          avgGrad = sum gradients / fromIntegral (length gradients)
          variance = sum [ (g - avgGrad) ^ 2 | g <- gradients ] / fromIntegral (length gradients)
      in sqrt variance

-- ============================================================================
-- 4. 512手 自己対局モース強化学習プロセス (Reinforcement Learning)
-- ============================================================================

-- | 局面統計量 (RL Value & Frequency & Curvature)
data NodeStats = NodeStats
  { qValue     :: Double  -- 行動価値/評価 Q(s)
  , visitCount :: Int     -- 訪問回数 N(s)
  , curvature  :: Double  -- 局所曲率 κ(s)
  } deriving (Show)

type RLTable = Map.Map PositionKey NodeStats

-- | 局面ステップ記録（エピソード履歴）
data StepRecord = StepRecord
  { stepPly      :: Int
  , stepState    :: State
  , stepMove     :: Move
  , stepCurv     :: Double
  , stepPot      :: Double
  } deriving (Show)

-- 擬似乱数 (線形合同法)
nextRand :: Int -> (Int, Int)
nextRand seed =
  let a = 1103515245
      c = 12345
      m = 2147483648
      newSeed = (a * seed + c) `mod` m
  in (newSeed, newSeed)

-- | モース適応型サンプリングによる指し手選択 (Exploration vs Exploitation)
selectMoveRL :: State -> [Move] -> RLTable -> Double -> Int -> (Move, Int)
selectMoveRL st legalMoves rlTable temperature seed =
  let curPlr = turn st
      scoredMoves = map (\m -> 
        let nextSt = applyMove m st
            key = positionKey nextSt
            stats = Map.findWithDefault (NodeStats (evaluateState curPlr nextSt) 0 1.0) key rlTable
            -- Q値 + 曲率ボーナス / (訪問数+1)
            q = qValue stats
            curv = curvature stats
            visits = fromIntegral (visitCount stats)
            ucbBonus = 100.0 * sqrt (log (visits + 2.0) / (visits + 1.0))
            score = q + ucbBonus + 0.1 * curv
        in (m, score)
        ) legalMoves
      -- ソフトマックス重み計算 (アニーリング温度適用)
      maxScore = maximum (map snd scoredMoves)
      expWeights = map (\(m, s) -> (m, exp ((s - maxScore) / max 1.0 temperature))) scoredMoves
      totalWeight = sum (map snd expWeights)
      (seed1, rInt) = nextRand seed
      randVal = (fromIntegral (rInt `mod` 1000000) / 1000000.0) * totalWeight
      pickedMove = pickWeighted randVal 0.0 expWeights
  in (pickedMove, seed1)
  where
    pickWeighted _ _ [(m, _)] = m
    pickWeighted _ _ [] = head legalMoves
    pickWeighted rv acc ((m, w):rest)
      | rv <= acc + w = m
      | otherwise     = pickWeighted rv (acc + w) rest

-- | 1手ずつ512手まで局面を伸ばしていく自己対局ステップ実行
runEpisode512 :: State -> Int -> Int -> RLTable -> Double -> [StepRecord] -> (Double, [StepRecord], Int)
runEpisode512 st ply maxPly rlTable temp accSteps
  | ply > maxPly =
      -- 512手に到達：深層均衡（Draw / Equilibrium）
      let finalPot = evaluateState (turn st) st
      in (finalPot, reverse accSteps, 0)
  | otherwise =
      let rawMoves = generateMoves st
          f5Moves = f5SyzygyFilter st rawMoves
          -- 千日手回避
          (_sennichite, safeMoves) = partition (\m -> 
            let nextSt = applyMove m st
            in isSennichite (positionKey nextSt) (history nextSt)
            ) f5Moves
      in if null f5Moves
         then
           -- 詰み（相手の勝利）
           let mateScore = -10000.0
           in (mateScore, reverse accSteps, -1)
         else if null safeMoves
           then
             -- 回避不能な千日手引き分け
             (0.0, reverse accSteps, 1)
           else
             let curCurv = calculateCurvature st safeMoves
                 curPot  = evaluateState (turn st) st
                 (bestMove, nextSeed) = selectMoveRL st safeMoves rlTable temp (ply * 7919 + 104729)
                 nextSt = applyMove bestMove st
                 rec = StepRecord ply st bestMove curCurv curPot
             in runEpisode512 nextSt (ply + 1) maxPly rlTable temp (rec : accSteps)

-- | 報酬の逆伝播（TD / バックプロパゲーション）
backpropReward :: Double -> [StepRecord] -> RLTable -> RLTable
backpropReward terminalReward steps rlTable =
  foldl' updateStep rlTable (zip [0..] (reverse steps))
  where
    gamma = 0.995 -- 512手に対応する長期割引率
    updateStep table (idx, StepRecord{..}) =
      let key = positionKey stepState
          reward = terminalReward * (gamma ^ idx)
          curStats = Map.findWithDefault (NodeStats stepPot 0 stepCurv) key table
          newVisits = visitCount curStats + 1
          -- 移動平均更新 (TD学習)
          alpha = 1.0 / fromIntegral (newVisits + 1)
          newQ = (1.0 - alpha) * qValue curStats + alpha * reward
          updatedStats = curStats { qValue = newQ, visitCount = newVisits, curvature = stepCurv }
      in Map.insert key updatedStats table

-- ============================================================================
-- 5. モース収縮アルゴリズム (Morse Contraction / Collapsing)
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

-- | 512手の軌道から曲率ピーク（サドル点）を検出し、平坦流を V-Path として収縮統合する
contractTrajectoryToMorseBook :: [StepRecord] -> Double -> MorseBook
contractTrajectoryToMorseBook steps finalReward =
  let totalSteps = length steps
      -- 1. サドル点（臨界ノード）の検出：局所曲率のスパイク、または一定手数ごとのトポロジー節目
      indexedSteps = zip [1..] steps
      criticalPlies = findCriticalPlies indexedSteps
      
      -- 2. ノードの構築
      startNode = BookNode "Start" Saddle 0.0
      intermediateNodes = [ BookNode ("Saddle_Ply_" ++ show p ++ "_" ++ showMove (stepMove rec)) Saddle (stepPot rec)
                          | (p, rec) <- criticalPlies ]
      terminalType = if abs finalReward >= 9000.0 then Minimum else Saddle
      terminalNode = BookNode ("Minimum_Ply_" ++ show totalSteps ++ "_Convergence") terminalType finalReward
      
      allNodes = startNode : intermediateNodes ++ [terminalNode]
      
      -- 3. ノード間の指し手を集約し、V-Path（必然手順）として収縮
      pathBounds = 0 : map fst criticalPlies ++ [totalSteps]
      pairs = zip pathBounds (tail pathBounds)
      allMoves = map (showMove . stepMove) steps
      
      vpaths = [ BookVPath fromId toId chunkMoves
               | ((pStart, pEnd), (fromNode, toNode)) <- zip pairs (zip allNodes (tail allNodes))
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
      -- 32手ごとのマクロ節目、または曲率が平均より有意に高いサドル点を臨界ノードとして抽出
      let avgCurv = sum [ stepCurv r | (_, r) <- xs ] / fromIntegral (length xs)
          threshold = max 15.0 (avgCurv * 1.5)
      in filter (\(p, r) -> p `mod` 32 == 0 || (stepCurv r > threshold && p `mod` 8 == 0)) xs

serializeMBook :: MorseBook -> String
serializeMBook MorseBook{..} =
  let nodeLines = [ "NODE:" ++ bNodeId ++ "," ++ show bNodeType ++ "," ++ show bPotential | BookNode{..} <- bookNodes ]
      pathLines = [ "PATH:" ++ bpFrom ++ "->" ++ bpTo ++ ":" ++ intercalate ">" bpMoves | BookVPath{..} <- bookPaths ]
  in unlines (nodeLines ++ pathLines)

-- ============================================================================
-- 6. メイン実行ルーチン (512手 強化学習・モース収縮ドライバ)
-- ============================================================================

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "================================================================================"
  putStrLn "   Topological Shogi AI - 512-Ply Self-Play Morse Contraction RL Engine"
  putStrLn "================================================================================"
  putStrLn "Initializing Reinforcement Learning table and starting 512-ply frontier expansion..."

  let targetMaxPly = 512
      numEpisodes = 5
      initialRLTable = Map.empty

  -- 強化学習エピソードループ
  (finalRLTable, bestTrajectory, finalReward) <- runLearningLoop 1 numEpisodes initialRLTable [] 0.0 targetMaxPly

  putStrLn "\n================================================================================"
  putStrLn " [Morse Contraction] Performing Homological Reduction on 512-Ply Trajectory..."
  putStrLn "================================================================================"
  
  -- モース収縮を実行して完全定跡（Morse Complex）を生成
  let morseBook = contractTrajectoryToMorseBook bestTrajectory finalReward
      totalNodes = length (bookNodes morseBook)
      totalPaths = length (bookPaths morseBook)
      totalPlies = length bestTrajectory

  putStrLn $ printf "Total Plies Extended: %d moves (Target: %d)" totalPlies targetMaxPly
  putStrLn $ printf "Contracted Critical Nodes: %d nodes" totalNodes
  putStrLn $ printf "Synthesized Morse V-Paths: %d paths" totalPaths
  let compression = (1.0 - (fromIntegral totalNodes / fromIntegral (max 1 totalPlies))) * 100.0 :: Double
  putStrLn $ printf "Homological Search Space Compression: %.2f%%" compression

  -- .mbook ファイルへのシリアライズ保存
  let mbookContent = serializeMBook morseBook
      outFile = "complete_morse.mbook"
  writeFile outFile mbookContent
  putStrLn $ "\n[SUCCESS] Serialized 512-Ply Complete Morse Book to: " ++ outFile

  -- 型レベルコンパイラによる CompiledMorseBook.hs の生成
  putStrLn "\n--- Invoking ShogiMorseCompiler to generate type-level safe Haskell code ---"
  callCommand ".\\shogi-morse-compiler.exe complete_morse.mbook"

  -- 対局エンジンの再コンパイル（512手完全定跡を静的リンク）
  putStrLn "\n--- Recompiling ShogiUsiMorse.exe with the 512-Ply Complete Morse Book ---"
  callCommand "ghc -O2 ShogiUsiMorse-v4.hs -o ShogiUsiMorse.exe"

  putStrLn "\n================================================================================"
  putStrLn " [REINFORCEMENT LEARNING COMPLETE] 512-Ply Morse Book Successfully Built & Linked!"
  putStrLn "================================================================================"

runLearningLoop :: Int -> Int -> RLTable -> [StepRecord] -> Double -> Int -> IO (RLTable, [StepRecord], Double)
runLearningLoop ep maxEp table bestTraj bestRew maxPly
  | ep > maxEp = return (table, bestTraj, bestRew)
  | otherwise = do
      let temp = 10.0 * (0.8 ^ (ep - 1)) -- 探索温度のアニーリング
      putStrLn $ printf "\n>>> [Episode %d/%d] Expanding 512-ply frontier (Temperature: %.2f)..." ep maxEp temp
      
      let (reward, steps, status) = runEpisode512 initState 1 maxPly table temp []
          plyReached = length steps
          statusStr = case status of
            (-1) -> "Checkmate / Mate Resolution"
            1    -> "Sennichite (Repetition Draw Avoided)"
            _    -> "Max Depth (512 Plies) Frontier Reached"
      
      -- 1手ごとのサマリー進捗表示 (16手ごと、および節目)
      putStrLn "    --- Real-time Ply-by-Ply Extension Log ---"
      mapM_ (\rec -> 
        let p = stepPly rec
        in if p == 1 || p == 10 || p == 50 || p == 100 || p == 200 || p == 300 || p == 400 || p == 512 || p == plyReached
           then putStrLn $ printf "    [Ply %3d] Move: %-6s | Potential V(s): %+8.2f | Curvature κ(s): %7.2f"
                          p (showMove (stepMove rec)) (stepPot rec) (stepCurv rec)
           else return ()
        ) steps

      putStrLn $ printf "    ===> Episode %d Outcome: %s at Ply %d (Reward: %+.2f)" ep statusStr plyReached reward
      
      -- 報酬バックプロパゲーション
      let newTable = backpropReward reward steps table
          (newBestTraj, newBestRew) = 
            if plyReached >= length bestTraj 
            then (steps, reward) 
            else (bestTraj, bestRew)

      runLearningLoop (ep + 1) maxEp newTable newBestTraj newBestRew maxPly
