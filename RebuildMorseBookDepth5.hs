{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO
import System.Process
import Data.Char
import Data.List
import Data.Maybe
import Text.Printf
import qualified Data.Map.Strict as Map
import Control.Exception (catch, SomeException)
import Text.Read (readMaybe)
import Control.Monad (forM)

-- ============================================================================
-- 1. 将棋盤と駒の基本データ構造
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
parseSquare [f, r]
  | isDigit f && r >= 'a' && r <= 'i' = (digitToInt f, ord r - ord 'a' + 1)
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
parseMove origStr
  | '*' `elem` cleanStr =
      case break (== '*') cleanStr of
        (pStr, _:toStr) ->
          let pt = case map toUpper pStr of
                ('P':_) -> Pawn ; ('L':_) -> Lance ; ('N':_) -> Knight
                ('S':_) -> Silver ; ('G':_) -> Gold ; ('B':_) -> Bishop ; ('R':_) -> Rook
                _       -> Pawn
          in DropMove pt (parseSquare toStr)
        _ -> DropMove Pawn (1, 1)
  | otherwise =
      let fromStr = take 2 cleanStr
          toStr   = take 2 (drop 2 cleanStr)
          promo   = "+" `isSuffixOf` cleanStr
      in NormalMove (parseSquare fromStr) (parseSquare toStr) promo
  where
    cleanStr = case origStr of
      ('+':rest) -> rest
      s          -> s

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
        Just (Piece _ King) -> hands
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
-- 2. トポロジー・利き・合法手生成
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

    isLegalDrop pt (x, y) =
      pt `elem` [Pawn, Lance, Knight, Silver, Gold, Bishop, Rook] &&
      case pt of
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
-- 3. 駒価値・評価関数・タダ捨て除外
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
defaultWeights = MidgameWeights 2.0 6.01 1.43 1.59 1.17 1.31

loadMidgameWeights :: IO MidgameWeights
loadMidgameWeights = do
  let configFile = "midgame_weights.config"
  catch (do
    content <- readFile configFile
    case readMaybe content of
      Just w  -> return w
      Nothing -> return defaultWeights
    ) (\(_ :: SomeException) -> return defaultWeights)

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
evaluateState weights player st =
  let opp = opponent player
      bd = board st
      myDanger = evalMorsePotential player bd
      enemyDanger = evalMorsePotential opp bd
      morseAdvantage = (enemyDanger - myDanger) * wMorseDanger weights
      materialAdvantage = evalMaterial player st * wMaterial weights
      attackAdvantage = (evalMidgameAttack player st - evalMidgameAttack opp st) * wAttack weights
      defenseAdvantage = (evalMidgameDefense player st - evalMidgameDefense opp st) * wDefense weights
      kakutoKeitoAdvantage = evalKakutoKeitoDefense player st * wKakuto weights
      looseAdvantage = evalLoosePieces player st * wLoosePenalty weights
  in materialAdvantage + morseAdvantage + attackAdvantage + defenseAdvantage + kakutoKeitoAdvantage + looseAdvantage

-- ============================================================================
-- 4. 深さ5手先読み探索 (静止探索2手付き、実質7手先読み)
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

searchDepth5 :: MidgameWeights -> Int -> Double -> Double -> State -> (Double, Maybe Move)
searchDepth5 weights 0 alpha beta st =
  (searchQuiescence weights 2 alpha beta st, Nothing)
searchDepth5 weights depth alpha beta st =
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
             beamLimit = if depth >= 4 then 6 else 8
             topMoves = take beamLimit orderedMoves
         in loop topMoves alpha Nothing
  where
    loop [] curAlpha bestMv = (curAlpha, bestMv)
    loop (m:ms) curAlpha bestMv =
      let nextSt = applyMove m st
          (childScore, _) = searchDepth5 weights (depth - 1) (-beta) (-curAlpha) nextSt
          score = -childScore
      in if score >= beta
         then (score, Just m)
         else if score > curAlpha
              then loop ms score (Just m)
              else loop ms curAlpha (if isJust bestMv then bestMv else Just m)

-- ============================================================================
-- 5. 16大完全定跡のベース手順
-- ============================================================================

baseMasterSequences :: [(String, String, Double, [String])]
baseMasterSequences =
  [ ( "Saddle_Aigakari_Standard", "Saddle", 25.0
    , ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3i3h", "7a7b", "5i6h", "5a4b", "4i5h", "4b3c", "6h7h", "7b6c", "3h2g", "8b8d", "2h2f", "8d7d", "2f2e", "7d8d", "2e2f", "8d7d"]
    )
  , ( "Saddle_Aigakari_Hikibisha", "Saddle", 30.0
    , ["2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d", "2h2d", "P*2c", "2d2f", "7a7b", "3i3h", "5a4b", "5i6h", "4b3c", "4i5h", "7b6c", "6g6f", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3h2g", "8b8d", "2f2h", "8d7d"]
    )
  , ( "Saddle_Kakugawari_Hayaguri", "Saddle", 40.0
    , ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "8h2b+", "3a2b", "7i8h", "7a6b", "3i4h", "6b7b", "4h3g", "4a3b", "6i7h", "5a4b", "5i6h", "6c6d", "3g4f", "7c7d", "4f3e", "3c3d", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h"]
    )
  , ( "Saddle_Kakugawari_Koshikake", "Saddle", 35.0
    , ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "8h2b+", "3a2b", "7i8h", "7a6b", "3i3h", "6b7b", "3h4h", "4a3b", "4h4g", "5a4b", "5i6h", "6c6d", "4g5f", "7b6c", "6i7h", "5a4b"]
    )
  , ( "Saddle_Yagura_Fortress", "Saddle", 50.0
    , ["7g7f", "8c8d", "6g6f", "3c3d", "7i6h", "6c6d", "6h7g", "7a6b", "5g5f", "5a4b", "4i5h", "3a3b", "5i6h", "4b3c", "6h7h", "5c5d", "7h8h", "4c4d", "8h7h", "8d8e", "7h8h", "6b5c", "2g2f", "7c7d", "2f2e", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "4g4f", "3b4c", "3i4h", "4c3b", "4h4g"]
    )
  , ( "Saddle_Yagura_Kato", "Saddle", 45.0
    , ["7g7f", "8c8d", "7i6h", "3c3d", "6h7g", "7a6b", "2g2f", "8d8e", "5g5f", "4a3b", "5i6h", "5a4b", "4i5h", "6b5c", "6h7h", "7c7d", "2f2e", "3a2b", "7h8h", "5c6d"]
    )
  , ( "Saddle_Shikenbisha_Mino", "Saddle", 60.0
    , ["7g7f", "3c3d", "6g6f", "8c8d", "2h6h", "8d8e", "8h7g", "6b6c", "4i4h", "7a6b", "3i3h", "4a3b", "5i4h", "5a4b", "4h3h", "3a2b", "3h2h", "7b7a", "2h1h", "7a8b", "6h7h", "8b7b", "1h2h", "1c1d", "1g1f", "5c5d", "2g2f", "4b3a", "2f2e", "2b3c", "3g3f"]
    )
  , ( "Saddle_Shikenbisha_Exchange", "Saddle", 55.0
    , ["7g7f", "3c3d", "2h6h", "8c8d", "8h2b+", "3a2b", "7i8h", "7a6b", "8h7g", "8d8e", "6h6f", "6b7b", "4i4h", "4a3b", "5i4h", "5a4b", "4h3h", "3a2b", "3h2h", "7b8b", "6i5h", "4b3a", "1g1f", "1c1d"]
    )
  , ( "Saddle_Sankenbisha_Ishida", "Saddle", 50.0
    , ["7g7f", "3c3d", "7i6h", "8c8d", "2h7h", "8d8e", "8h7g", "6b6c", "3i4h", "4a3b", "5i4h", "5a4b", "4h3h", "3a2b", "3h2h", "7a6b", "1g1f", "1c1d", "3g3f", "2b3c", "7h7f"]
    )
  , ( "Saddle_Nakabisha_Gokigen", "Saddle", 65.0
    , ["5g5f", "3c3d", "5h5e", "8c8d", "5e5f", "8d8e", "7g7f", "6b6c", "7i6h", "4a3b", "4i5h", "5a4b", "3i4h", "3a2b", "5i4h", "4b3a", "4h3h", "7a6b", "3h2h", "6b7c", "2g2f", "7c8d", "2f2e", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3g3f", "8d7c"]
    )
  , ( "Saddle_Nakabisha_LeftKing", "Saddle", 55.0
    , ["5g5f", "3c3d", "5h5e", "5c5d", "5e5f", "8c8d", "7g7f", "8d8e", "7i6h", "6b6c", "4i5h", "4a3b", "3i4h", "5a4b", "5i4h", "4b3a", "4h3h", "3a2b", "3h2h", "7a6b", "2h1h", "6b7c"]
    )
  , ( "Saddle_Yokofudori_34Rook", "Saddle", 70.0
    , ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d", "2h2d", "8e8f", "8g8f", "8b8f", "2d3d", "8f8h+", "7i8h", "B*4e", "3d3f", "4e2g+", "3f3d", "2g1g", "3d3f", "1g1f", "3f3e", "1f2f", "B*3c", "2f2e"]
    )
  , ( "Saddle_Gote_84Pawn_Ibisha", "Saddle", 20.0
    , ["2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3i3h", "7a7b", "5i6h", "5a4b", "4i5h", "4b3c", "6h7h", "7b6c"]
    )
  , ( "Saddle_Gote_34Pawn_Yagura", "Saddle", 25.0
    , ["7g7f", "3c3d", "2g2f", "4c4d", "4i4h", "3a3b", "5g5f", "4b4a", "5i6h", "5a4b", "6h7h", "4a4b", "7h8h", "4b4c", "3i3h", "3c3d", "2f2e", "2b3c", "4h4g", "8b4b"]
    )
  , ( "Saddle_EdgePawn_Solid", "Saddle", 15.0
    , ["1g1f", "3c3d", "7g7f", "8c8d", "6g6f", "8d8e", "7i6h", "6b6c", "2g2f", "4a3b", "6h7g", "7a6b", "5g5f", "5a4b", "4i5h", "4b3a", "5i6h", "3a2b", "6h7h", "6b7b"]
    )
  , ( "Saddle_DirectGold_KakutoDefense", "Saddle", 35.0
    , ["7g7f", "8c8d", "7i7h", "8d8e", "8h7g", "3c3d", "2g2f", "6b6c", "2f2e", "4a3b", "3i4h", "5a4b", "5i6h", "4b3c", "6h7h", "7a6b", "7h8h", "6b7c", "4h3g", "7c8d", "3g4f", "8d7c", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h"]
    )
  ]

-- 深さ5手先読みにより、定跡終点からさらに最善手を生成して伸長する
extendSequenceDepth5 :: MidgameWeights -> [String] -> Int -> State -> IO [String]
extendSequenceDepth5 _ acc 0 _ = return acc
extendSequenceDepth5 weights acc stepsLeft st = do
  let (score, maybeMv) = searchDepth5 weights 5 (-99999.0) 99999.0 st
  case maybeMv of
    Nothing -> return acc
    Just mv -> do
      let mvStr = showMove mv
          nextSt = applyMove mv st
      putStrLn $ printf "       + [Depth 5] Ply %2d: %-6s (Eval: %+6.1f)" (length acc + 1) mvStr score
      extendSequenceDepth5 weights (acc ++ [mvStr]) (stepsLeft - 1) nextSt

-- ============================================================================
-- 6. メインドライバ：深さ5手による完全定跡再構築
-- ============================================================================

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "================================================================================"
  putStrLn "   Topological Shogi AI - Complete Opening Book Rebuilding at Depth 5"
  putStrLn "   Expanding 16 Master Systems with 5-Ply Lookahead + Quiescence Midgame Engine"
  putStrLn "================================================================================"

  weights <- loadMidgameWeights
  putStrLn "\n[Using Midgame Weights]:"
  print weights

  putStrLn "\n--- Rebuilding and Extending Opening Sequences via 5-Ply Lookahead ---"

  rebuiltSequences <- forM baseMasterSequences $ \(nid, ntype, pot, mvs) -> do
    putStrLn $ printf "\n>>> Processing: %-32s (Base: %2d plies)" nid (length mvs)
    let baseState = foldl' (\s mStr -> applyMove (parseMove mStr) s) initState mvs
    -- 各定跡を深さ5手探索でさらに +6手（3往復）伸ばす
    extendedMoves <- extendSequenceDepth5 weights mvs 6 baseState
    let finalState = foldl' (\s mStr -> applyMove (parseMove mStr) s) initState extendedMoves
        totalPlies = length extendedMoves
    putStrLn $ printf "    ===> Rebuilt Complete Path: %2d plies | Final Turn: %s" totalPlies (show (turn finalState))
    return (nid, ntype, pot, extendedMoves)

  -- .mbook ファイルの生成
  let startNode = "NODE:Start,Saddle,0.0"
      nodeLines = [ "NODE:" ++ nid ++ "," ++ ntype ++ "," ++ show pot | (nid, ntype, pot, _) <- rebuiltSequences ]
      pathLines = [ "PATH:Start->" ++ nid ++ ":" ++ intercalate ">" mvs | (nid, _, _, mvs) <- rebuiltSequences ]
      mbookContent = unlines ([startNode] ++ nodeLines ++ pathLines)
      outFile = "complete_morse.mbook"

  writeFile outFile mbookContent
  putStrLn $ "\n[SUCCESS] Serialized Depth-5 Master Morse Opening Book to: " ++ outFile

  -- Morse Compiler 呼び出し
  putStrLn "\n--- Invoking shogi-morse-compiler.exe to generate type-level CompiledMorseBook.hs ---"
  callCommand ".\\shogi-morse-compiler.exe complete_morse.mbook"

  putStrLn "\n================================================================================"
  putStrLn " [COMPLETE] Successfully Rebuilt Complete Opening Book at Depth 5!"
  putStrLn "================================================================================"
