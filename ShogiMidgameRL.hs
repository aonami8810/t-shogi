{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO
import Data.Char
import Data.List
import Data.Maybe
import Text.Printf
import qualified Data.Map.Strict as Map
import Control.Exception (catch, SomeException)
import Text.Read (readMaybe)
import qualified CompiledMorseBook as Book

-- | 外部依存不要の高速LCG疑似乱数生成器
nextRand :: Int -> (Int, Int)
nextRand seed =
  let nextSeed = (seed * 1103515245 + 12345) `mod` 2147483647
  in (nextSeed, nextSeed)

randomRange :: (Int, Int) -> Int -> (Int, Int)
randomRange (low, high) seed =
  let (r, nextSeed) = nextRand seed
      val = low + (abs r `mod` (high - low + 1))
  in (val, nextSeed)

-- ============================================================================
-- 1. 基本データ型定義
-- ============================================================================

data Player = Sente | Gote deriving (Eq, Show, Enum, Bounded, Ord)

opponent :: Player -> Player
opponent Sente = Gote
opponent Gote  = Sente

data PieceType 
  = Pawn | Lance | Knight | Silver | Gold | Bishop | Rook | King
  | PPawn | PLance | PKnight | PSilver | PBishop | PRook
  deriving (Eq, Show, Enum, Bounded, Ord)

data Piece = Piece Player PieceType deriving (Eq, Ord, Show)

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

positionKey :: State -> PositionKey
positionKey State{..} = (turn, board, hands)

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
  let st = State Sente initBoard (Map.fromList [(Sente, emptyHand), (Gote, emptyHand)]) Map.empty Map.empty []
  in st { history = Map.singleton (positionKey st) 1 }

showSquare :: Square -> String
showSquare (f, r) = show f ++ [chr (ord 'a' + r - 1)]

parseSquare :: String -> Square
parseSquare [f, r] = (digitToInt f, ord r - ord 'a' + 1)
parseSquare _      = (1, 1)

showMove :: Move -> String
showMove (NormalMove from to promo) = showSquare from ++ showSquare to ++ (if promo then "+" else "")
showMove (DropMove p to) =
  let pieceChar = case p of
        Pawn   -> 'P' ; Lance  -> 'L' ; Knight -> 'N'
        Silver -> 'S' ; Gold   -> 'G' ; Bishop -> 'B' ; Rook   -> 'R'
        _      -> ' '
  in pieceChar : '*' : showSquare to

parseMove :: String -> Move
parseMove str
  | '*' `elem` str =
      case break (== '*') str of
        (pStr, _:toStr) ->
          let pt = case map toUpper pStr of
                ('P':_) -> Pawn ; ('L':_) -> Lance ; ('N':_) -> Knight
                ('S':_) -> Silver ; ('G':_) -> Gold ; ('B':_) -> Bishop ; ('R':_) -> Rook
                _       -> Pawn
          in DropMove pt (parseSquare toStr)
        _ -> DropMove Pawn (1, 1)
  | otherwise =
      let fromStr = take 2 str
          toStr   = take 2 (drop 2 str)
          promo   = "+" `isSuffixOf` str
      in NormalMove (parseSquare fromStr) (parseSquare toStr) promo

applyMove :: Move -> State -> State
applyMove (NormalMove from to promo) State{..} =
  let piece = Map.lookup from board
      captured = Map.lookup to board
      boardWithSourceRemoved = Map.delete from board
      newPiece = case piece of
        Just (Piece plr pt) -> Piece plr (if promo then promote pt else pt)
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
-- 2. トポロジー・利き・移動判定
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

isPieceProtected :: Square -> Piece -> Board -> Bool
isPieceProtected sq (Piece plr _) bd =
  let friendlyDefenders = [ fromSq | (fromSq, p@(Piece pPlr _)) <- Map.toList bd
                                   , pPlr == plr
                                   , fromSq /= sq
                                   , canPieceReachTarget fromSq p sq bd ]
  in not (null friendlyDefenders)

findKing :: Player -> Board -> Maybe Square
findKing plr b = listToMaybe [sq | (sq, Piece p pt) <- Map.toList b, p == plr, pt == King]

isKingUnderAttack :: Player -> Board -> Bool
isKingUnderAttack plr bd =
  case findKing plr bd of
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
  in if not isOppInCheck
     then False
     else
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
-- 3. 駒価値・中盤攻防特徴量・強化学習パラメータ
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

data MidgameWeights = MidgameWeights
  { wMaterial       :: !Double
  , wMorseDanger    :: !Double
  , wAttack         :: !Double
  , wDefense        :: !Double
  , wKakuto         :: !Double
  , wLoosePenalty   :: !Double
  } deriving (Show, Read, Eq)

defaultWeights :: MidgameWeights
defaultWeights = MidgameWeights
  { wMaterial       = 1.0
  , wMorseDanger    = 6.0
  , wAttack         = 1.3
  , wDefense        = 1.3
  , wKakuto         = 1.0
  , wLoosePenalty   = 1.0
  }

-- 特徴量ベクトル（各要素の優劣スコア）
data FeatureVector = FeatureVector
  { fMaterial    :: !Double
  , fMorseDanger :: !Double
  , fAttack      :: !Double
  , fDefense     :: !Double
  , fKakuto      :: !Double
  , fLoose       :: !Double
  } deriving (Show)

extractFeatures :: Player -> State -> FeatureVector
extractFeatures player st =
  let opp = opponent player
      bd = board st
      matAdv = evalMaterial player st
      myDanger = evalMorsePotential player bd
      enemyDanger = evalMorsePotential opp bd
      morseAdv = enemyDanger - myDanger
      atkAdv = evalMidgameAttack player st - evalMidgameAttack opp st
      defAdv = evalMidgameDefense player st - evalMidgameDefense opp st
      kakutoAdv = evalKakutoKeitoDefense player st
      looseAdv = evalLoosePieces player st
  in FeatureVector matAdv morseAdv atkAdv defAdv kakutoAdv looseAdv

dotProduct :: MidgameWeights -> FeatureVector -> Double
dotProduct MidgameWeights{..} FeatureVector{..} =
  fMaterial * wMaterial +
  fMorseDanger * wMorseDanger +
  fAttack * wAttack +
  fDefense * wDefense +
  fKakuto * wKakuto +
  fLoose * wLoosePenalty

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
  case findKing player bd of
    Nothing -> 10000.0
    Just (kx, ky) ->
      let enemyPieces = Map.toList $ Map.filter (\(Piece p _) -> p /= player) bd
          influenceSum = sum [ 10.0 / (fromIntegral (dist (kx, ky) sq) + 0.1) | (sq, _) <- enemyPieces ]
          surroundings = [(kx + dx, ky + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]
          wallPenalties = sum [ if isOutOfBounds sq || Map.member sq bd then 0.0 else 1.5 | sq <- surroundings ]
      in influenceSum + wallPenalties
  where
    dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

isPushingKakutoPawn :: State -> Move -> Bool
isPushingKakutoPawn st m =
  let plr = turn st
      bd = board st
  in case m of
    NormalMove (8, 7) (8, 6) _ | plr == Sente ->
      case Map.lookup (8, 8) bd of
        Just (Piece Sente Rook)  -> False
        Just (Piece Sente PRook) -> False
        _                        -> True
    NormalMove (2, 3) (2, 4) _ | plr == Gote ->
      case Map.lookup (2, 2) bd of
        Just (Piece Gote Rook)  -> False
        Just (Piece Gote PRook) -> False
        _                       -> True
    _ -> False

evalKakutoKeitoDefense :: Player -> State -> Double
evalKakutoKeitoDefense plr st =
  let bd = board st
      opp = opponent plr
      senteDefenseScore =
        let enemyRookPawnAdvanced =
              case Map.lookup (8, 4) bd of
                Just (Piece Gote Pawn) -> True
                _ -> case Map.lookup (8, 5) bd of
                       Just (Piece Gote Pawn) -> True
                       _ -> False
            kakutoCovered = isCoveredBy Sente (8, 7) bd
        in if enemyRookPawnAdvanced
           then if kakutoCovered then 150.0 else -1200.0
           else if kakutoCovered then 50.0 else 0.0

      goteDefenseScore =
        let enemyRookPawnAdvanced =
              case Map.lookup (2, 6) bd of
                Just (Piece Sente Pawn) -> True
                _ -> case Map.lookup (2, 5) bd of
                       Just (Piece Sente Pawn) -> True
                       _ -> False
            kakutoCovered = isCoveredBy Gote (2, 3) bd
        in if enemyRookPawnAdvanced
           then if kakutoCovered then 150.0 else -1200.0
           else if kakutoCovered then 50.0 else 0.0

      myScore = if plr == Sente then senteDefenseScore else goteDefenseScore
      oppScore = if opp == Sente then senteDefenseScore else goteDefenseScore
  in myScore - oppScore

evalLoosePieces :: Player -> State -> Double
evalLoosePieces plr st =
  let bd = board st
      opp = opponent plr
      countLoose player =
        length [ sq | (sq, p@(Piece pPlr pt)) <- Map.toList bd
                    , pPlr == player
                    , pt `notElem` [Pawn, King]
                    , not (isPieceProtected sq p bd)
               ]
      myLoose = countLoose plr
      oppLoose = countLoose opp
  in (fromIntegral oppLoose - fromIntegral myLoose) * 40.0

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
      oppAttackers = getPiecesCovering opp toSq bd
      nextBd = board (applyMove m st)
      friendlyDefenders = [ fromSq | (fromSq, Piece p _) <- Map.toList nextBd
                                   , p == plr, fromSq /= toSq
                                   , canPieceReachTarget fromSq (Piece plr ptMoved) toSq nextBd ]
  in if null oppAttackers
     then False
     else
       let minOppVal = minimum [ pieceValue oppPt | (_, Piece _ oppPt) <- oppAttackers ]
           movedVal  = pieceValue ptMoved
       in if null friendlyDefenders
          then ptMoved /= Pawn
          else movedVal > minOppVal + 250.0

evalMidgameAttack :: Player -> State -> Double
evalMidgameAttack plr st =
  let bd = board st
      opp = opponent plr
      oppKingSq = findKing opp bd
      promotedBonus = sum
        [ case pt of
            PRook   -> if inEnemyCamp plr (snd sq) then 320.0 else 200.0
            PBishop -> 240.0
            PPawn   -> if inEnemyCamp plr (snd sq) then 160.0 else 90.0
            PSilver -> 110.0
            PKnight -> 90.0
            PLance  -> 90.0
            _       -> 0.0
        | (sq, Piece p pt) <- Map.toList bd, p == plr
        ]
      kingPressureBonus = case oppKingSq of
        Nothing -> 0.0
        Just (kx, ky) ->
          let kingNeighbors = [ (kx + dx, ky + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0), not (isOutOfBounds (kx + dx, ky + dy)) ]
              headSq = (kx, ky + (if opp == Sente then -1 else 1))
              neighborPressures = sum [ if isCoveredBy plr sq bd then 40.0 else 0.0 | sq <- kingNeighbors ]
              headPressure = if isCoveredBy plr headSq bd then 80.0 else 0.0
          in neighborPressures + headPressure
      footholdBonus = sum
        [ 50.0 | (sq, Piece p pt) <- Map.toList bd, p == plr, inEnemyCamp plr (snd sq), pt `elem` [Pawn, PPawn] ]
  in promotedBonus + kingPressureBonus + footholdBonus
  where
    inEnemyCamp Sente y = y <= 3
    inEnemyCamp Gote  y = y >= 7

evalMidgameDefense :: Player -> State -> Double
evalMidgameDefense plr st =
  let bd = board st
      opp = opponent plr
      myKingSq = findKing plr bd
      (castleBonus, dangerAroundKing) = case myKingSq of
        Nothing -> (0.0, 0.0)
        Just (kx, ky) ->
          let myGuards = [ (sq, pt) | (sq, Piece p pt) <- Map.toList bd
                                    , p == plr
                                    , pt `elem` [Gold, Silver, PPawn, PSilver]
                                    , abs (fst sq - kx) <= 2 && abs (snd sq - ky) <= 2 ]
              guardCount = length myGuards
              linkPairs = [ 1 | ((sq1, _), (sq2, _)) <- [(g1, g2) | g1 <- myGuards, g2 <- myGuards, fst g1 < fst g2]
                              , canPieceReachTarget sq1 (Piece plr Gold) sq2 bd ]
              connectedBonus = fromIntegral (sum linkPairs) * 50.0
              guardBonus = fromIntegral guardCount * 45.0
              kingNeighbors = [ (kx + dx, ky + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0), not (isOutOfBounds (kx + dx, ky + dy)) ]
              enemyAttacks = sum [ if isCoveredBy opp sq bd then 50.0 else 0.0 | sq <- kingNeighbors ]
          in (guardBonus + connectedBonus, enemyAttacks)
      enemyPromotedInCamp = sum
        [ case pt of
            PRook   -> 260.0
            PBishop -> 210.0
            PPawn   -> 110.0
            _       -> 0.0
        | (sq, Piece p pt) <- Map.toList bd, p == opp, inMyCamp plr (snd sq)
        ]
  in castleBonus - dangerAroundKing - enemyPromotedInCamp
  where
    inMyCamp Sente y = y >= 7
    inMyCamp Gote  y = y <= 3

evaluateState :: MidgameWeights -> Player -> State -> Double
evaluateState weights player st = dotProduct weights (extractFeatures player st)

-- ============================================================================
-- 4. 静止探索 ＆ α-β探索
-- ============================================================================

searchQuiescence :: MidgameWeights -> Int -> Double -> Double -> State -> Double
searchQuiescence weights 0 _ _ st = evaluateState weights (turn st) st
searchQuiescence weights qdepth alpha beta st =
  let standPat = evaluateState weights (turn st) st
  in if standPat >= beta
     then beta
     else
       let curAlpha = max alpha standPat
           rawMoves = generateMoves st
           captures = filter isCaptureMove rawMoves
           f5Captures = f5SyzygyFilter st captures
       in if null f5Captures
          then curAlpha
          else qLoop f5Captures curAlpha beta
  where
    isCaptureMove (NormalMove _ to _) = Map.member to (board st)
    isCaptureMove (DropMove _ _)       = False

    qLoop [] curA _ = curA
    qLoop (m:ms) curA b =
      let nextSt = applyMove m st
          score = - searchQuiescence weights (qdepth - 1) (-b) (-curA) nextSt
      in if score >= b
         then b
         else qLoop ms (max curA score) b

searchAlphaBeta :: MidgameWeights -> Int -> Double -> Double -> State -> (Double, Maybe Move)
searchAlphaBeta weights 0 alpha beta st =
  (searchQuiescence weights 2 alpha beta st, Nothing)
searchAlphaBeta weights depth alpha beta st =
  let rawMoves = generateMoves st
      f5Moves = f5SyzygyFilter st rawMoves
      (_sennichite, safeMoves) = partition (\m -> 
        let nextSt = applyMove m st
        in isSennichite (positionKey nextSt) (history nextSt)
        ) f5Moves
      nonKakutoMoves = filter (not . isPushingKakutoPawn st) safeMoves
      effectiveMoves = if null nonKakutoMoves then safeMoves else nonKakutoMoves
  in if null f5Moves
     then (-10000.0, Nothing)
     else if null effectiveMoves
       then (0.0, Nothing)
       else
         let scoreMove m =
               let isBlunder = isBlunderMove st m
                   blunderPen = if isBlunder then -2000.0 else 0.0
                   nextSt = applyMove m st
                   s = evaluateState weights (turn st) nextSt
               in s + blunderPen
             orderedMoves = sortBy (\m1 m2 -> compare (scoreMove m2) (scoreMove m1)) effectiveMoves
             topMoves = take (if depth >= 2 then 8 else 6) orderedMoves
         in loop topMoves alpha Nothing
  where
    loop [] curAlpha bestMv = (curAlpha, bestMv)
    loop (m:ms) curAlpha bestMv =
      let nextSt = applyMove m st
          (childScore, _) = searchAlphaBeta weights (depth - 1) (-beta) (-curAlpha) nextSt
          score = -childScore
      in if score >= beta
         then (score, Just m)
         else if score > curAlpha
              then loop ms score (Just m)
              else loop ms curAlpha (if isJust bestMv then bestMv else Just m)

-- ============================================================================
-- 5. 自己対局エピソード記録と強化学習 (TD-Learning / Policy Gradient)
-- ============================================================================

data StepRecord = StepRecord
  { stepPlayer   :: !Player
  , stepFeatures :: !FeatureVector
  , stepMove     :: !Move
  , stepEval     :: !Double
  } deriving (Show)

-- 定跡の終点（中盤突入地点）を生成する
sampleMidgameOpeningState :: Int -> IO State
sampleMidgameOpeningState seed = do
  let paths = Map.elems Book.runtimePaths
      (idx, nextSeed) = randomRange (0, length paths - 1) seed
      selectedPath = paths !! idx
      -- 定跡の14〜22手目までを適用して中盤局面にする
      plyCount = min (length selectedPath) (16 + (nextSeed `mod` 7))
      openingMoves = take plyCount selectedPath
      midState = foldl' (\st mStr -> applyMove (parseMove mStr) st) initState openingMoves
  return midState

-- 中盤自己対局エピソードの実行
runMidgameEpisode :: MidgameWeights -> State -> Int -> Int -> Double -> Int -> [StepRecord] -> IO (Double, [StepRecord], State)
runMidgameEpisode weights st currentPly maxPly temp seed acc
  | currentPly >= maxPly = do
      let finalEval = evaluateState weights Sente st
      return (finalEval, reverse acc, st)
  | otherwise = do
      let (bestScore, maybeMove) = searchAlphaBeta weights 3 (-99999.0) 99999.0 st
      case maybeMove of
        Nothing -> do
          -- 詰み（直前の手番の勝利）
          let winnerRew = if turn st == Sente then -10000.0 else 10000.0
          return (winnerRew, reverse acc, st)
        Just mv -> do
          let fv = extractFeatures (turn st) st
              rec = StepRecord (turn st) fv mv bestScore
              nextSt = applyMove mv st
              (_, nextSeed) = nextRand seed
          runMidgameEpisode weights nextSt (currentPly + 1) maxPly temp nextSeed (rec : acc)

-- TD強化学習による重み更新
updateWeightsTD :: MidgameWeights -> Double -> [StepRecord] -> Double -> MidgameWeights
updateWeightsTD weights finalReward records lr =
  let totalSteps = length records
      gamma = 0.95
      -- 終局報酬と各ステップでの寄与度をバックプロパゲーション
      stepGrads = zipWith (\idx rec ->
        let discount = gamma ^ (totalSteps - 1 - idx)
            plrSign = if stepPlayer rec == Sente then 1.0 else -1.0
            targetRew = finalReward * plrSign
            tdError = targetRew - stepEval rec
            clippedError = max (-500.0) (min 500.0 tdError)
            feat = stepFeatures rec
        in ( discount * clippedError / 10000.0
           , feat
           )
        ) [0..] records

      -- 勾配の積算
      gradMaterial = sum [ w * fMaterial f | (w, f) <- stepGrads ] / fromIntegral (max 1 totalSteps)
      gradMorse    = sum [ w * fMorseDanger f | (w, f) <- stepGrads ] / fromIntegral (max 1 totalSteps)
      gradAttack   = sum [ w * fAttack f | (w, f) <- stepGrads ] / fromIntegral (max 1 totalSteps)
      gradDefense  = sum [ w * fDefense f | (w, f) <- stepGrads ] / fromIntegral (max 1 totalSteps)
      gradKakuto   = sum [ w * fKakuto f | (w, f) <- stepGrads ] / fromIntegral (max 1 totalSteps)
      gradLoose    = sum [ w * fLoose f | (w, f) <- stepGrads ] / fromIntegral (max 1 totalSteps)

      -- 重みの更新（クリッピングで極端な発散を防止）
      clamp minV maxV v = max minV (min maxV v)
      newMat   = clamp 0.5 2.0  (wMaterial weights + lr * gradMaterial)
      newMorse = clamp 2.0 12.0 (wMorseDanger weights + lr * gradMorse)
      newAtk   = clamp 0.8 3.0  (wAttack weights + lr * gradAttack)
      newDef   = clamp 0.8 3.0  (wDefense weights + lr * gradDefense)
      newKak   = clamp 0.5 2.0  (wKakuto weights + lr * gradKakuto)
      newLoose = clamp 0.5 2.5  (wLoosePenalty weights + lr * gradLoose)
  in MidgameWeights newMat newMorse newAtk newDef newKak newLoose

-- ============================================================================
-- 6. 強化学習メインループ
-- ============================================================================

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "================================================================================"
  putStrLn "   Shogi Midgame Reinforcement Learning Engine (Self-Play TD-Learning)"
  putStrLn "   Target: Establish Deep Midgame Attack & Defense Tactics from Opening Frontiers"
  putStrLn "================================================================================"

  let totalEpisodes = 12
      maxPlyPerGame = 45 -- 定跡終了後から45手（計60手超）の中盤激突
      initialWeights = defaultWeights
      initialLearningRate = 0.08

  putStrLn "\n[RL Initial Weights]"
  print initialWeights

  (trainedWeights, bestGameMoves) <- runRLLoop 1 totalEpisodes initialWeights initialLearningRate []

  putStrLn "\n================================================================================"
  putStrLn " [REINFORCEMENT LEARNING COMPLETE] Final Optimized Midgame Weights:"
  putStrLn "================================================================================"
  print trainedWeights

  -- 設定ファイルへの保存
  let configStr = show trainedWeights
      configFile = "midgame_weights.config"
  writeFile configFile configStr
  putStrLn $ "\n[SUCCESS] Saved Optimized Midgame Weights to: " ++ configFile

  putStrLn "\n--- Exemplary Midgame Tactical Self-Play Game Record (Sampled) ---"
  mapM_ (\(i, m) -> putStrLn $ printf "  Move %2d: %s" (i :: Int) m) (zip [1..] (take 36 bestGameMoves))

runRLLoop :: Int -> Int -> MidgameWeights -> Double -> [String] -> IO (MidgameWeights, [String])
runRLLoop ep totalEp curWeights lr bestMoves
  | ep > totalEp = return (curWeights, bestMoves)
  | otherwise = do
      let seed = 1000 + ep * 73
      midState <- sampleMidgameOpeningState seed
      let startPly = length (moveHistory midState)
      putStrLn $ printf "\n>>> [Episode %2d/%2d] Starting from Opening Frontier (Ply %d)..." ep totalEp startPly
      
      (finalReward, records, finalSt) <- runMidgameEpisode curWeights midState startPly (startPly + 20) 1.0 seed []
      let gameMoves = moveHistory finalSt
          updatedWeights = updateWeightsTD curWeights finalReward records lr
          nextLr = lr * 0.96

      putStrLn $ printf "    ===> Final Reward: %+.1f | Plies: %d -> %d | Attack: %.2f, Defense: %.2f, Morse: %.2f"
        finalReward startPly (length gameMoves) (wAttack updatedWeights) (wDefense updatedWeights) (wMorseDanger updatedWeights)

      runRLLoop (ep + 1) totalEp updatedWeights nextLr gameMoves
