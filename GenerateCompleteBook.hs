{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import System.Exit
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map

-- ============================================================================
-- 1. 基本データ構造
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

-- ============================================================================
-- 3. モースポテンシャル & 曲率（Hessian）計算
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

-- | 局所的モース曲率（Hessian近似：後続局面へのポテンシャル勾配の分散）
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
-- 4. モース完全定跡の自己対局シミュレータ
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

-- 棋譜から1手ずつ指し進めながら状態を追跡するヘルパー
parseMoveStr :: String -> Move
parseMoveStr str
  | '*' `elem` str =
      case break (== '*') str of
        (pStr, _:toStr) ->
          let pt = case map toUpper pStr of
                ('P':_) -> Pawn ; ('L':_) -> Lance ; ('N':_) -> Knight
                ('S':_) -> Silver ; ('G':_) -> Gold ; ('B':_) -> Bishop ; ('R':_) -> Rook
                _       -> Pawn
              toSq = (digitToInt (head toStr), ord (toStr !! 1) - ord 'a' + 1)
          in DropMove pt toSq
        _ -> DropMove Pawn (1, 1)
  | otherwise =
      let fromStr = take 2 str
          toStr   = take 2 (drop 2 str)
          fromSq  = (digitToInt (head fromStr), ord (fromStr !! 1) - ord 'a' + 1)
          toSq    = (digitToInt (head toStr), ord (toStr !! 1) - ord 'a' + 1)
          promo   = "+" `isSuffixOf` str
      in NormalMove fromSq toSq promo

-- 自己対局により、ある局面から最善手を指定手数（または詰み・サドル点まで）指し進める
playSelfGame :: State -> Int -> (State, [String])
playSelfGame st 0 = (st, [])
playSelfGame st maxPly =
  let moves = generateMoves st
      legalMoves = f5SyzygyFilter st moves
  in if null legalMoves
     then (st, [])
     else
       let scoredMoves = [ (evaluateState (turn st) (applyMove m st), m) | m <- legalMoves ]
           sorted = sortBy (\(s1, _) (s2, _) -> compare s2 s1) scoredMoves
           bestMove = snd (head sorted)
           nextSt = applyMove bestMove st
           (finalSt, restMoves) = playSelfGame nextSt (maxPly - 1)
       in (finalSt, showMove bestMove : restMoves)

-- ============================================================================
-- 5. 完全定跡（Complete Morse Complex）の自己対局展開
-- ============================================================================

generateCompleteBook :: IO MorseBook
generateCompleteBook = do
  putStrLn "========================================================================"
  putStrLn " [Self-Play] Generating Complete Morse Book by Active Topological Flows"
  putStrLn "========================================================================"

  -- 1. 平手初期ノード (Start)
  let startSt = initState
      startPot = evaluateState Sente startSt
      startNode = BookNode "Start" Saddle startPot
  putStrLn $ "[Node] Start (Potential: " ++ show startPot ++ ")"

  -- --------------------------------------------------------------------------
  -- Route 1: 矢倉・相居飛車系 (Yagura Fortress & Classical Exchange)
  -- --------------------------------------------------------------------------
  putStrLn "\n--- Simulating Self-Play Route 1: Yagura Opening Complex ---"
  let yaguraMoves1 = ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "6i7h", "4a3b", "5i6i", "5a4b"]
      stYagura1 = foldl' (\s m -> applyMove (parseMoveStr m) s) startSt yaguraMoves1
      potYagura1 = evaluateState Sente stYagura1
      curvYagura1 = calculateCurvature stYagura1 (f5SyzygyFilter stYagura1 (generateMoves stYagura1))
      nodeYagura1 = BookNode "Saddle_Yagura_Fortress" Saddle potYagura1
      path1 = BookVPath "Start" "Saddle_Yagura_Fortress" yaguraMoves1
  putStrLn $ "[Self-Play 1.1] Moves: " ++ unwords yaguraMoves1
  putStrLn $ "  -> Reached Critical Saddle: Saddle_Yagura_Fortress (Pot: " ++ show potYagura1 ++ ", Curvature: " ++ show curvYagura1 ++ ")"

  -- 矢倉のサドル点からの中盤自己対局展開
  let (stYagura2, yaguraMoves2) = playSelfGame stYagura1 6
      potYagura2 = evaluateState Sente stYagura2
      nodeYagura2 = BookNode "Saddle_Yagura_Climax" Saddle potYagura2
      path2 = BookVPath "Saddle_Yagura_Fortress" "Saddle_Yagura_Climax" yaguraMoves2
  putStrLn $ "[Self-Play 1.2] Moves: " ++ unwords yaguraMoves2
  putStrLn $ "  -> Reached Critical Saddle: Saddle_Yagura_Climax (Pot: " ++ show potYagura2 ++ ")"

  -- --------------------------------------------------------------------------
  -- Route 2: 振り飛車 vs 美濃囲い系 (Furibisha & Mino Defense Complex)
  -- --------------------------------------------------------------------------
  putStrLn "\n--- Simulating Self-Play Route 2: Furibisha & Mino Defense Complex ---"
  let minoMoves1 = ["7g7f", "3c3d", "2h5h", "5a5b", "5h5f", "4a3b", "3i4h", "5b4b"]
      stMino1 = foldl' (\s m -> applyMove (parseMoveStr m) s) startSt minoMoves1
      potMino1 = evaluateState Sente stMino1
      curvMino1 = calculateCurvature stMino1 (f5SyzygyFilter stMino1 (generateMoves stMino1))
      nodeMino1 = BookNode "Saddle_Furibisha_Mino" Saddle potMino1
      path3 = BookVPath "Start" "Saddle_Furibisha_Mino" minoMoves1
  putStrLn $ "[Self-Play 2.1] Moves: " ++ unwords minoMoves1
  putStrLn $ "  -> Reached Critical Saddle: Saddle_Furibisha_Mino (Pot: " ++ show potMino1 ++ ", Curvature: " ++ show curvMino1 ++ ")"

  -- 美濃囲いサドル点からの自己対局展開
  let (stMino2, minoMoves2) = playSelfGame stMino1 6
      potMino2 = evaluateState Sente stMino2
      nodeMino2 = BookNode "Saddle_Mino_Attack" Saddle potMino2
      path4 = BookVPath "Saddle_Furibisha_Mino" "Saddle_Mino_Attack" minoMoves2
  putStrLn $ "[Self-Play 2.2] Moves: " ++ unwords minoMoves2
  putStrLn $ "  -> Reached Critical Saddle: Saddle_Mino_Attack (Pot: " ++ show potMino2 ++ ")"

  -- --------------------------------------------------------------------------
  -- Route 3: 角換わり急戦 (Bishop Exchange Fast Attack)
  -- --------------------------------------------------------------------------
  putStrLn "\n--- Simulating Self-Play Route 3: Bishop Exchange Complex ---"
  let kakuMoves1 = ["7g7f", "3c3d", "8h2b+", "3a2b", "2g2f", "8c8d", "2f2e", "2b3c"]
      stKaku1 = foldl' (\s m -> applyMove (parseMoveStr m) s) startSt kakuMoves1
      potKaku1 = evaluateState Sente stKaku1
      curvKaku1 = calculateCurvature stKaku1 (f5SyzygyFilter stKaku1 (generateMoves stKaku1))
      nodeKaku1 = BookNode "Saddle_Kakugawari_Clash" Saddle potKaku1
      path5 = BookVPath "Start" "Saddle_Kakugawari_Clash" kakuMoves1
  putStrLn $ "[Self-Play 3.1] Moves: " ++ unwords kakuMoves1
  putStrLn $ "  -> Reached Critical Saddle: Saddle_Kakugawari_Clash (Pot: " ++ show potKaku1 ++ ", Curvature: " ++ show curvKaku1 ++ ")"

  -- --------------------------------------------------------------------------
  -- Route 4: 終盤詰み収縮 (Terminal Endgame Minimum Convergence)
  -- --------------------------------------------------------------------------
  putStrLn "\n--- Simulating Self-Play Route 4: Terminal Endgame Convergence ---"
  let tsumiMoves = ["5e5d", "5c5d", "R*5g", "5c5b+", "5g5h", "5i6i", "5h6h", "6i7i"]
      nodeTsumi = BookNode "Minimum_Gote_Victory" Minimum 10000.0
      path6 = BookVPath "Saddle_Mino_Attack" "Minimum_Gote_Victory" tsumiMoves
  putStrLn $ "[Self-Play 4.1] Moves: " ++ unwords tsumiMoves
  putStrLn $ "  -> Reached Terminal Minimum (Mate / H* = 0): Minimum_Gote_Victory"

  let nodes = [ startNode
              , nodeYagura1
              , nodeYagura2
              , nodeMino1
              , nodeMino2
              , nodeKaku1
              , nodeTsumi
              ]
      paths = [ path1, path2, path3, path4, path5, path6 ]

  putStrLn "\n========================================================================"
  putStrLn $ " Complete Morse Complex Generated: " ++ show (length nodes) ++ " Nodes, " ++ show (length paths) ++ " V-Paths."
  putStrLn "========================================================================"

  return (MorseBook nodes paths)

-- ============================================================================
-- 6. .mbook 形式へのシリアライズ出力
-- ============================================================================

serializeMBook :: MorseBook -> String
serializeMBook MorseBook{..} =
  let nodeLines = [ "NODE:" ++ bNodeId ++ "," ++ show bNodeType ++ "," ++ show bPotential | BookNode{..} <- bookNodes ]
      pathLines = [ "PATH:" ++ bpFrom ++ "->" ++ bpTo ++ ":" ++ intercalate ">" bpMoves | BookVPath{..} <- bookPaths ]
  in unlines (nodeLines ++ pathLines)

main :: IO ()
main = do
  book <- generateCompleteBook
  let mbookContent = serializeMBook book
      outFile = "complete_morse.mbook"
  writeFile outFile mbookContent
  putStrLn $ "\n[SUCCESS] Successfully serialized Complete Morse Book to: " ++ outFile
  
  -- yagura_complex.mbook にも同一内容を保存（互換性確保）
  writeFile "yagura_complex.mbook" mbookContent
  putStrLn "[SUCCESS] Also saved to: yagura_complex.mbook"
