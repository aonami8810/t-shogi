{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import System.IO
import System.Exit
import Data.Bits (xor, shiftR)
import Data.Char (chr, ord, digitToInt, isDigit, isUpper, toUpper)
import Data.List (sortBy, partition, isSuffixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Word (Word64)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- ============================================================================
-- 1. 将棋盤と駒の基本データ構造 ＋ 64bit Zobrist 商空間ハッシュ
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

data Piece = Piece !Player !PieceType
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
type Board  = Map.Map Square Piece
type Hand   = Map.Map PieceType Int

data Move
  = NormalMove !Square !Square !Bool
  | DropMove !PieceType !Square
  deriving (Eq, Ord, Show)

-- | Df-pn 転置表エントリ (Zobrist DAG ノード)
data TTEntry = TTEntry
  { ttPn       :: !Int
  , ttDn       :: !Int
  , ttBestMove :: !(Maybe Move)
  } deriving (Show)

type DfpnTable = Map.Map Word64 TTEntry

-- | 状態（手番、盤面、持ち駒、Zobristハッシュ、Garside置換表、Df-pn DAG転置表、千日手履歴）
data State = State
  { turn    :: !Player
  , board   :: !Board
  , hands   :: !(Map.Map Player Hand)
  , zobrist :: !Word64
  , cache   :: !(Map.Map Word64 (Double, Move))
  , dfpnTT  :: !DfpnTable
  , history :: !(Map.Map Word64 Int)
  } deriving (Show)

-- | SplitMix64 による決定論的 64bit Zobrist ハッシュ基底
splitMix64 :: Word64 -> Word64
splitMix64 x0 =
  let x1 = x0 + 0x9e3779b97f4a7c15
      x2 = (x1 `xor` (x1 `shiftR` 30)) * 0xbf58476d1ce4e5b9
      x3 = (x2 `xor` (x2 `shiftR` 27)) * 0x94d049bb133111eb
  in x3 `xor` (x3 `shiftR` 31)

zobristPiece :: Square -> Piece -> Word64
zobristPiece (x, y) (Piece plr pt) =
  let sqIdx = (x - 1) * 9 + (y - 1)
      seed  = fromIntegral (1000 + sqIdx * 32 + fromEnum plr * 16 + fromEnum pt)
  in splitMix64 seed

zobristHand :: Player -> PieceType -> Int -> Word64
zobristHand plr pt cnt =
  let seed = fromIntegral (50000 + fromEnum plr * 1000 + fromEnum pt * 32 + cnt)
  in splitMix64 seed

zobristTurn :: Player -> Word64
zobristTurn Sente = splitMix64 99991
zobristTurn Gote  = splitMix64 99992

computeZobrist :: Player -> Board -> Map.Map Player Hand -> Word64
computeZobrist t bd hs =
  let bHash = Map.foldlWithKey' (\acc sq pc -> acc `xor` zobristPiece sq pc) 0 bd
      hHash = Map.foldlWithKey' (\acc plr hMap ->
                Map.foldlWithKey' (\a pt cnt ->
                  if cnt > 0 then a `xor` zobristHand plr pt cnt else a
                ) acc hMap
              ) 0 hs
  in zobristTurn t `xor` bHash `xor` hHash

-- ============================================================================
-- 2. 8方向幾何単体（Direction8 Simplicial Complex）代数モデル
-- ============================================================================

data Direction8
  = DirN | DirS | DirW | DirE
  | DirNW | DirNE | DirSW | DirSE
  deriving (Eq, Show, Enum, Bounded, Ord)

data RayType = UnitStep | InfiniteRay
  deriving (Eq, Show, Ord)

dirVector :: Player -> Direction8 -> (Int, Int)
dirVector Sente dir = case dir of
  DirN  -> ( 0, -1) ; DirS  -> ( 0,  1) ; DirW  -> ( 1,  0) ; DirE  -> (-1,  0)
  DirNW -> ( 1, -1) ; DirNE -> (-1, -1) ; DirSW -> ( 1,  1) ; DirSE -> (-1,  1)
dirVector Gote dir = case dir of
  DirN  -> ( 0,  1) ; DirS  -> ( 0, -1) ; DirW  -> (-1,  0) ; DirE  -> ( 1,  0)
  DirNW -> (-1,  1) ; DirNE -> ( 1,  1) ; DirSW -> (-1, -1) ; DirSE -> ( 1, -1)

pieceSimplicialChain :: PieceType -> [(Direction8, RayType)]
pieceSimplicialChain pt = case pt of
  Pawn    -> [(DirN, UnitStep)]
  Lance   -> [(DirN, InfiniteRay)]
  Silver  -> [ (DirN, UnitStep), (DirNW, UnitStep), (DirNE, UnitStep)
             , (DirSW, UnitStep), (DirSE, UnitStep) ]
  Gold    -> [ (DirN, UnitStep), (DirS, UnitStep), (DirW, UnitStep)
             , (DirE, UnitStep), (DirNW, UnitStep), (DirNE, UnitStep) ]
  Bishop  -> [ (DirNW, InfiniteRay), (DirNE, InfiniteRay)
             , (DirSW, InfiniteRay), (DirSE, InfiniteRay) ]
  Rook    -> [ (DirN, InfiniteRay), (DirS, InfiniteRay)
             , (DirW, InfiniteRay), (DirE, InfiniteRay) ]
  King    -> [ (d, UnitStep) | d <- [minBound .. maxBound] ]
  PPawn   -> pieceSimplicialChain Gold
  PLance  -> pieceSimplicialChain Gold
  PKnight -> pieceSimplicialChain Gold
  PSilver -> pieceSimplicialChain Gold
  PBishop -> pieceSimplicialChain Bishop ++ [(DirN, UnitStep), (DirS, UnitStep), (DirW, UnitStep), (DirE, UnitStep)]
  PRook   -> pieceSimplicialChain Rook   ++ [(DirNW, UnitStep), (DirNE, UnitStep), (DirSW, UnitStep), (DirSE, UnitStep)]
  Knight  -> []

isOutOfBounds :: Square -> Bool
isOutOfBounds (x, y) = x < 1 || x > 9 || y < 1 || y > 9

raySimplexTargets :: Square -> (Int, Int) -> RayType -> Board -> [Square]
raySimplexTargets (x, y) (dx, dy) UnitStep _ =
  let target = (x + dx, y + dy)
  in if isOutOfBounds target then [] else [target]
raySimplexTargets (x, y) (dx, dy) InfiniteRay bd =
  let step (cx, cy) =
        let nextSq = (cx + dx, cy + dy)
        in if isOutOfBounds nextSq
           then []
           else case Map.lookup nextSq bd of
                  Nothing -> nextSq : step nextSq
                  Just _  -> [nextSq]
  in step (x, y)

pieceAttacks :: Square -> Piece -> Board -> [Square]
pieceAttacks sq (Piece plr pt) bd =
  let regularTargets = concatMap (\(dir, ray) ->
                         raySimplexTargets sq (dirVector plr dir) ray bd
                       ) (pieceSimplicialChain pt)
      knightTargets = if pt == Knight
                      then let dy = if plr == Sente then -2 else 2
                               cands = [(fst sq - 1, snd sq + dy), (fst sq + 1, snd sq + dy)]
                           in filter (not . isOutOfBounds) cands
                      else []
  in regularTargets ++ knightTargets

isSquareUnderAttack :: Player -> Square -> Board -> Bool
isSquareUnderAttack attackerPlayer targetSq bd =
  any (\(sq, pc@(Piece p _)) -> p == attackerPlayer && targetSq `elem` pieceAttacks sq pc bd) (Map.toList bd)

findKing :: Player -> Board -> Maybe Square
findKing plr bd = listToMaybe [sq | (sq, Piece p King) <- Map.toList bd, p == plr]

isKingUnderAttack :: Player -> Board -> Bool
isKingUnderAttack plr bd =
  case findKing plr bd of
    Just kSq -> isSquareUnderAttack (opponent plr) kSq bd
    Nothing  -> False

-- ============================================================================
-- 3. USIプロトコルパーサ ＆ 状態遷移
-- ============================================================================

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

emptyHand :: Hand
emptyHand = Map.fromList [(p, 0) | p <- [Pawn, Lance, Knight, Silver, Gold, Bishop, Rook]]

initState :: State
initState =
  let hs = Map.fromList [(Sente, emptyHand), (Gote, emptyHand)]
      z0 = computeZobrist Sente initBoard hs
  in State
       { turn    = Sente
       , board   = initBoard
       , hands   = hs
       , zobrist = z0
       , cache   = Map.empty
       , dfpnTT  = Map.empty
       , history = Map.singleton z0 1
       }

showSquare :: Square -> String
showSquare (f, r) = show f ++ [chr (ord 'a' + r - 1)]

parseSquare :: String -> Square
parseSquare [f, r] = (digitToInt f, ord r - ord 'a' + 1)
parseSquare _      = (1, 1)

showMove :: Move -> String
showMove (NormalMove from to promo) =
  showSquare from ++ showSquare to ++ (if promo then "+" else "")
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
        (pStr:_, _:toStr) ->
          let pt = case pStr of
                'P' -> Pawn ; 'L' -> Lance ; 'N' -> Knight
                'S' -> Silver ; 'G' -> Gold ; 'B' -> Bishop ; 'R' -> Rook
                _   -> Pawn
          in DropMove pt (parseSquare toStr)
        _ -> DropMove Pawn (1, 1)
  | otherwise =
      let fromStr = take 2 str
          toStr   = take 2 (drop 2 str)
          promo   = "+" `isSuffixOf` str
      in NormalMove (parseSquare fromStr) (parseSquare toStr) promo

-- | SFEN文字列から盤面・手番・持ち駒を復元（任意の詰将棋・終盤局面の直接ロードに対応）
parseSfen :: String -> String -> String -> State
parseSfen boardStr turnStr handStr =
  let rows = splitBy '/' boardStr
      bd   = Map.fromList $ concat [ parseRow r rowStr | (r, rowStr) <- zip [1..9] rows ]
      t    = if turnStr == "w" then Gote else Sente
      hs   = parseHands handStr
      z0   = computeZobrist t bd hs
  in State
       { turn    = t
       , board   = bd
       , hands   = hs
       , zobrist = z0
       , cache   = Map.empty
       , dfpnTT  = Map.empty
       , history = Map.singleton z0 1
       }
  where
    splitBy :: Char -> String -> [String]
    splitBy _ "" = []
    splitBy delim s =
      let (w, rest) = break (== delim) s
      in w : case rest of
               []     -> []
               (_:rs) -> splitBy delim rs

    parseRow :: Int -> String -> [(Square, Piece)]
    parseRow r = go 9 False
      where
        go _ _ [] = []
        go f _ ('+':cs) = go f True cs
        go f isPromo (c:cs)
          | isDigit c = go (f - digitToInt c) False cs
          | otherwise =
              let plr = if isUpper c then Sente else Gote
                  basePt = charToPt (toUpper c)
                  pt = if isPromo then promote basePt else basePt
              in ((f, r), Piece plr pt) : go (f - 1) False cs

    charToPt :: Char -> PieceType
    charToPt c = case c of
      'P' -> Pawn ; 'L' -> Lance ; 'N' -> Knight ; 'S' -> Silver
      'G' -> Gold ; 'B' -> Bishop ; 'R' -> Rook ; 'K' -> King
      _   -> Pawn

    parseHands :: String -> Map.Map Player Hand
    parseHands "-" = Map.fromList [(Sente, emptyHand), (Gote, emptyHand)]
    parseHands s   = goH 1 s (Map.fromList [(Sente, emptyHand), (Gote, emptyHand)])
      where
        goH :: Int -> String -> Map.Map Player Hand -> Map.Map Player Hand
        goH _ [] acc = acc
        goH n (c:cs) acc
          | isDigit c =
              let (numChars, rest) = span isDigit (c:cs)
                  cnt = read numChars :: Int
              in goH cnt rest acc
          | otherwise =
              let plr = if isUpper c then Sente else Gote
                  pt  = charToPt (toUpper c)
                  h   = fromMaybe emptyHand (Map.lookup plr acc)
                  h'  = Map.insertWith (+) pt n h
              in goH 1 cs (Map.insert plr h' acc)

promote :: PieceType -> PieceType
promote Pawn   = PPawn   ; promote Lance  = PLance
promote Knight = PKnight ; promote Silver = PSilver
promote Bishop = PBishop ; promote Rook   = PRook
promote pt     = pt

demote :: PieceType -> PieceType
demote PPawn   = Pawn   ; demote PLance  = Lance
demote PKnight = Knight ; demote PSilver = Silver
demote PBishop = Bishop ; demote PRook   = Rook
demote pt      = pt

applyMove :: Move -> State -> State
applyMove (NormalMove from to promo) State{..} =
  let piece    = Map.lookup from board
      captured = Map.lookup to board
      bd1      = Map.delete from board
      newPiece = case piece of
        Just (Piece plr pt) -> Piece plr (if promo then promote pt else pt)
        Nothing             -> Piece turn Pawn
      newBoard = Map.insert to newPiece bd1
      newHands = case captured of
        Just (Piece _ pt) | pt /= King ->
          let rawPt   = demote pt
              handMap = fromMaybe emptyHand (Map.lookup turn hands)
              updHand = Map.insertWith (+) rawPt 1 handMap
          in Map.insert turn updHand hands
        _ -> hands
      nextTurn = opponent turn
      newZ     = computeZobrist nextTurn newBoard newHands
      newHist  = Map.insertWith (+) newZ 1 history
  in State nextTurn newBoard newHands newZ cache dfpnTT newHist

applyMove (DropMove pt to) State{..} =
  let newBoard = Map.insert to (Piece turn pt) board
      handMap  = fromMaybe emptyHand (Map.lookup turn hands)
      updHand  = Map.insertWith (\_ old -> max 0 (old - 1)) pt 0 handMap
      newHands = Map.insert turn updHand hands
      nextTurn = opponent turn
      newZ     = computeZobrist nextTurn newBoard newHands
      newHist  = Map.insertWith (+) newZ 1 history
  in State nextTurn newBoard newHands newZ cache dfpnTT newHist

-- ============================================================================
-- 4. F5フィルター ＆ 8方向単体テンソル積によるトポロジカル評価関数
-- ============================================================================

f5SyzygyFilter :: State -> [Move] -> [Move]
f5SyzygyFilter st moves =
  let turnPlr = turn st
  in filter (\m -> let State { board = nextBd } = applyMove m st in not (isKingUnderAttack turnPlr nextBd)) moves

countEscapeChambers :: Player -> Square -> Board -> Double
countEscapeChambers plr (kx, ky) bd =
  let enemy = opponent plr
      neighborSquares = [ (dir, (kx + dx, ky + dy))
                        | dir <- [minBound .. maxBound]
                        , let (dx, dy) = dirVector plr dir ]
      isOpenChamber (_, sq) =
        not (isOutOfBounds sq) &&
        not (isFriendly sq) &&
        not (isSquareUnderAttack enemy sq bd)
  in fromIntegral (length (filter isOpenChamber neighborSquares))
  where
    isFriendly sq = case Map.lookup sq bd of
      Just (Piece p _) -> p == plr
      Nothing          -> False

calcHandPressure :: Hand -> Double
calcHandPressure hand =
  let weights = [ (Rook, 5.0), (Bishop, 4.0), (Gold, 3.0)
                , (Silver, 2.5), (Knight, 1.5), (Lance, 1.2), (Pawn, 0.8) ]
  in sum [ w * fromIntegral (fromMaybe 0 (Map.lookup pt hand)) | (pt, w) <- weights ]

evalSyzygyTensorShield :: Player -> Square -> Board -> Double
evalSyzygyTensorShield plr kSq@(kx, ky) bd =
  let unitStepAttacks sq (Piece p pt) =
        let regular = [ tgt
                      | (dir, UnitStep) <- pieceSimplicialChain pt
                      , let (dx, dy) = dirVector p dir
                      , let tgt = (fst sq + dx, snd sq + dy)
                      , not (isOutOfBounds tgt) ]
            knight  = if pt == Knight
                      then let dy = if p == Sente then -2 else 2
                           in filter (not . isOutOfBounds) [(fst sq - 1, snd sq + dy), (fst sq + 1, snd sq + dy)]
                      else []
        in regular ++ knight

      castlePieces = [ (sq, pt, unitStepAttacks sq pc)
                     | (sq, pc@(Piece p pt)) <- Map.toList bd
                     , p == plr, sq /= kSq
                     , pt `notElem` [Rook, Bishop, King]
                     , abs (fst sq - kx) <= 3 && abs (snd sq - ky) <= 2 ]

      defenseShell = [ (x, y)
                     | x <- [max 1 (kx - 3) .. min 9 (kx + 3)]
                     , y <- [max 1 (ky - 2) .. min 9 (ky + 2)]
                     , (x, y) /= kSq ]

      tensorOverlapPairs = sum
        [ let m = length [ () | (_, _, targets) <- castlePieces, sq `elem` targets ]
          in fromIntegral (m * (m - 1)) * 0.5
        | sq <- defenseShell ]

      nearGenerals = [ (sq, pt)
                     | (sq, pt, _) <- castlePieces
                     , pt `elem` [Gold, Silver, PPawn, PLance, PKnight, PSilver] ]
      generalSyzygyLinks = sum
        [ fromIntegral (length [ () | (sq2, _, targets2) <- castlePieces, sq2 /= sq1, sq1 `elem` targets2 ])
        | (sq1, _) <- nearGenerals ]

      flankFactor = if kx == 5 then 1.0 else 1.45
      rawSyzygy   = flankFactor * (0.30 * tensorOverlapPairs + 0.45 * generalSyzygyLinks)
      initBaseline = 2.70
  in max 0.0 (rawSyzygy - initBaseline)

evalKingDistance :: Player -> State -> Double
evalKingDistance plr State{..} =
  case findKing plr board of
    Nothing -> 0.0
    Just kSq ->
      let enemy = opponent plr
          chambers = countEscapeChambers plr kSq board
          chamberScore = 3.0 * log (1.0 + chambers)
          defenders = [ sq | (sq, Piece p _) <- Map.toList board, p == plr, sq /= kSq ]
          defScore = sum [ 1.5 / (fromIntegral (manhattanDist kSq sq) + 0.5) | sq <- defenders ]
          syzygyShield = evalSyzygyTensorShield plr kSq board
          attackers = [ sq | (sq, Piece p _) <- Map.toList board, p == enemy ]
          attScore = sum [ 2.5 / (fromIntegral (manhattanDist kSq sq) + 0.2) | sq <- attackers ]
          enemyHand = fromMaybe emptyHand (Map.lookup enemy hands)
          handPressure = calcHandPressure enemyHand
          promotionZoneBonus = if (plr == Sente && snd kSq <= 3) || (plr == Gote && snd kSq >= 7)
                               then 25.0 else 0.0
          effectiveDistance = chamberScore + defScore + syzygyShield + promotionZoneBonus - (attScore + handPressure)
      in max 0.0 effectiveDistance
  where
    manhattanDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

evaluateState :: Player -> State -> Double
evaluateState viewPlayer st =
  let distSente = evalKingDistance Sente st
      distGote  = evalKingDistance Gote st
      senteAdvantage = (distSente - distGote) * 150.0
  in if viewPlayer == Sente then senteAdvantage else -senteAdvantage

-- ============================================================================
-- 5. 双対コバウンダリ王手生成 ＆ モース誘導型 Df-pn (最大512手完全詰み探索)
-- ============================================================================

canPromote :: Player -> PieceType -> Int -> Int -> Bool
canPromote plr pt srcY tgtY =
  let inZone y = if plr == Sente then y <= 3 else y >= 7
  in (inZone srcY || inZone tgtY) && pt `elem` [Pawn, Lance, Knight, Silver, Bishop, Rook]

mustPromote :: Player -> PieceType -> Int -> Bool
mustPromote Sente Pawn   y = y <= 1
mustPromote Sente Lance  y = y <= 1
mustPromote Sente Knight y = y <= 2
mustPromote Gote  Pawn   y = y >= 9
mustPromote Gote  Lance  y = y >= 9
mustPromote Gote  Knight y = y >= 8
mustPromote _     _      _ = False

generateCheckMoves :: State -> [Move]
generateCheckMoves st@State{..} =
  let enemy = opponent turn
  in case findKing enemy board of
       Nothing  -> []
       Just kSq@(kx, ky) ->
         let myPieces = [(sq, pc) | (sq, pc@(Piece p _)) <- Map.toList board, p == turn]
             boardCandidates = concatMap (\(sq, pc@(Piece _ pt)) ->
               let targets = filter (not . isFriendly) (pieceAttacks sq pc board)
               in concatMap (\tgt ->
                    let canP  = canPromote turn pt (snd sq) (snd tgt)
                        mustP = mustPromote turn pt (snd tgt)
                    in if canP
                       then if mustP then [NormalMove sq tgt True] else [NormalMove sq tgt True, NormalMove sq tgt False]
                       else [NormalMove sq tgt False]
                  ) targets
               ) myPieces

             myHand = fromMaybe emptyHand (Map.lookup turn hands)
             handPts = [pt | (pt, cnt) <- Map.toList myHand, cnt > 0]
             dropTargetSquares pt =
               [ (x, y)
               | x <- [1..9], y <- [1..9]
               , not (Map.member (x, y) board)
               , not (mustPromote turn pt y)
               , let dx = abs (x - kx)
                     dy = abs (y - ky)
               , case pt of
                   Rook   -> (x == kx || y == ky) && dx + dy <= 4
                   Bishop -> (dx == dy) && dx <= 4
                   Lance  -> x == kx && ((turn == Sente && y > ky && y - ky <= 4) || (turn == Gote && y < ky && ky - y <= 4))
                   Knight -> dx == 1 && ((turn == Sente && y == ky + 2) || (turn == Gote && y + 2 == ky))
                   _      -> dx <= 1 && dy <= 1
               ]
             dropCandidates =
               [ DropMove pt sq
               | pt <- handPts
               , sq <- dropTargetSquares pt ]

             allCands = boardCandidates ++ dropCandidates
         in [ m
            | m <- allCands
            , let State { board = nextBd } = applyMove m st
            , isSquareUnderAttack turn kSq nextBd
            , not (isKingUnderAttack turn nextBd)
            ]
  where
    isFriendly sq = case Map.lookup sq board of
      Just (Piece p _) -> p == turn
      Nothing          -> False

generateEvasionMoves :: State -> [Move]
generateEvasionMoves st@State{..} =
  case findKing turn board of
    Nothing  -> []
    Just kSq@(kx, ky) ->
      let kingSteps =
            [ NormalMove kSq tgt False
            | dir <- [minBound .. maxBound]
            , let (dx, dy) = dirVector turn dir
                  tgt = (kx + dx, ky + dy)
            , not (isOutOfBounds tgt)
            , not (isFriendly tgt)
            ]
          otherPieces = [(sq, pc) | (sq, pc@(Piece p pt)) <- Map.toList board, p == turn, pt /= King]
          defBoardMoves = concatMap (\(sq, pc@(Piece _ pt)) ->
            let targets = filter (not . isFriendly) (pieceAttacks sq pc board)
            in concatMap (\tgt ->
                 let canP = canPromote turn pt (snd sq) (snd tgt)
                 in if canP then [NormalMove sq tgt True, NormalMove sq tgt False]
                            else [NormalMove sq tgt False]
               ) targets
            ) otherPieces
          myHand = fromMaybe emptyHand (Map.lookup turn hands)
          handPts = [pt | (pt, cnt) <- Map.toList myHand, cnt > 0]
          interposeSquares =
            [ (x, y)
            | x <- [max 1 (kx - 2) .. min 9 (kx + 2)]
            , y <- [max 1 (ky - 2) .. min 9 (ky + 2)]
            , not (Map.member (x, y) board)
            , max (abs (x - kx)) (abs (y - ky)) <= 2
            ]
          interposeDrops =
            [ DropMove pt sq
            | pt <- handPts
            , sq <- interposeSquares
            , not (mustPromote turn pt (snd sq))
            ]
          candidates = kingSteps ++ defBoardMoves ++ interposeDrops
      in filter (\m -> let State { board = nextBd } = applyMove m st in not (isKingUnderAttack turn nextBd)) candidates
  where
    isFriendly sq = case Map.lookup sq board of
      Just (Piece p _) -> p == turn
      Nothing          -> False

infPN :: Int
infPN = 100000000

initNodePnDn :: Bool -> State -> (Int, Int)
initNodePnDn isOrNode State{..}
  | isOrNode  = (1, 1)
  | otherwise =
      case findKing turn board of
        Nothing  -> (0, infPN)
        Just kSq ->
          let ch = round (countEscapeChambers turn kSq board)
          in (max 1 ch, 1)

lookupTT :: Bool -> State -> DfpnTable -> (Int, Int, Maybe Move)
lookupTT isOrNode st tt =
  case Map.lookup (zobrist st) tt of
    Just (TTEntry p d bm) -> (p, d, bm)
    Nothing ->
      let (p0, d0) = initNodePnDn isOrNode st
      in (p0, d0, Nothing)

addPN :: Int -> Int -> Int
addPN a b
  | a >= infPN || b >= infPN = infPN
  | otherwise                = min infPN (a + b)

computeChildrenStats :: Bool -> [(Move, State)] -> Set.Set Word64 -> DfpnTable
                     -> (Int, Int, Maybe (Move, State, Int, Int), Int, Int)
computeChildrenStats isOrNode children pathSet tt =
  foldl' step (if isOrNode then infPN else 0, if isOrNode then 0 else infPN, Nothing, infPN, infPN) children
  where
    step (!accPn, !accDn, !bestChild, !bestMetric, !secondMetric) (mv, chSt) =
      let (cPn, cDn, _) =
            if Set.member (zobrist chSt) pathSet
            then (infPN, 0, Nothing)
            else lookupTT (not isOrNode) chSt tt
      in if isOrNode
         then let newPn = min accPn cPn
                  newDn = addPN accDn cDn
                  (bestC, bM, sM) =
                    if cPn < bestMetric
                    then (Just (mv, chSt, cPn, cDn), cPn, bestMetric)
                    else if cPn < secondMetric
                         then (bestChild, bestMetric, cPn)
                         else (bestChild, bestMetric, secondMetric)
              in (newPn, newDn, bestC, bM, sM)
         else let newPn = addPN accPn cPn
                  newDn = min accDn cDn
                  (bestC, bM, sM) =
                    if cDn < bestMetric
                    then (Just (mv, chSt, cPn, cDn), cDn, bestMetric)
                    else if cDn < secondMetric
                         then (bestChild, bestMetric, cDn)
                         else (bestChild, bestMetric, secondMetric)
              in (newPn, newDn, bestC, bM, sM)

dfpnMID :: Bool -> State -> Int -> Int -> Int -> Int -> Set.Set Word64 -> DfpnTable -> Int
        -> (Int, Int, Maybe Move, DfpnTable, Int)
dfpnMID isOrNode st thPn thDn depth maxDepth pathSet tt nodeBudget
  | nodeBudget <= 0 =
      let (p, d, bm) = lookupTT isOrNode st tt
      in (p, d, bm, tt, 0)
  | depth >= maxDepth =
      let tt' = Map.insert (zobrist st) (TTEntry infPN 0 Nothing) tt
      in (infPN, 0, Nothing, tt', nodeBudget - 1)
  | otherwise =
      let moves = if isOrNode then generateCheckMoves st else generateEvasionMoves st
      in if null moves
         then if isOrNode
              then let tt' = Map.insert (zobrist st) (TTEntry infPN 0 Nothing) tt
                   in (infPN, 0, Nothing, tt', nodeBudget - 1)
              else let tt' = Map.insert (zobrist st) (TTEntry 0 infPN Nothing) tt
                   in (0, infPN, Nothing, tt', nodeBudget - 1)
         else
           let children = [(m, applyMove m st) | m <- moves]
               pathSet' = Set.insert (zobrist st) pathSet
           in loopMID isOrNode st children thPn thDn depth maxDepth pathSet' tt (nodeBudget - 1)

loopMID :: Bool -> State -> [(Move, State)] -> Int -> Int -> Int -> Int -> Set.Set Word64 -> DfpnTable -> Int
        -> (Int, Int, Maybe Move, DfpnTable, Int)
loopMID isOrNode st children thPn thDn depth maxDepth pathSet tt budget =
  let (curPn, curDn, bestChild, _, secondMetric) = computeChildrenStats isOrNode children pathSet tt
      bestMv = fmap (\(m, _, _, _) -> m) bestChild
  in if curPn >= thPn || curDn >= thDn || budget <= 0
     then
       let tt' = Map.insert (zobrist st) (TTEntry curPn curDn bestMv) tt
       in (curPn, curDn, bestMv, tt', budget)
     else case bestChild of
       Nothing ->
         let tt' = Map.insert (zobrist st) (TTEntry curPn curDn Nothing) tt
         in (curPn, curDn, Nothing, tt', budget)
       Just (_, childSt, cPn, cDn) ->
         let (childThPn, childThDn) =
               if isOrNode
               then (min thPn (addPN secondMetric 1), addPN (thDn - curDn) cDn)
               else (addPN (thPn - curPn) cPn, min thDn (addPN secondMetric 1))
             (_, _, _, ttAfterChild, budget') =
               dfpnMID (not isOrNode) childSt childThPn childThDn (depth + 1) maxDepth pathSet tt budget
         in loopMID isOrNode st children thPn thDn depth maxDepth pathSet ttAfterChild budget'

solveMateDfpn :: Int -> Int -> State -> (Bool, [Move], Int, DfpnTable)
solveMateDfpn maxPlies maxNodes rootState =
  let (pn, _, _, finalTT, remBudget) =
        dfpnMID True rootState infPN infPN 0 maxPlies Set.empty (dfpnTT rootState) maxNodes
      usedNodes = maxNodes - remBudget
      pv = if pn == 0 then extractMatePV True rootState finalTT Set.empty maxPlies else []
  in (pn == 0, pv, usedNodes, finalTT)

extractMatePV :: Bool -> State -> DfpnTable -> Set.Set Word64 -> Int -> [Move]
extractMatePV _ _ _ _ 0 = []
extractMatePV isOrNode st tt visited remDepth
  | Set.member (zobrist st) visited = []
  | isOrNode =
      case Map.lookup (zobrist st) tt >>= ttBestMove of
        Just mv ->
          let nextSt = applyMove mv st
          in mv : extractMatePV False nextSt tt (Set.insert (zobrist st) visited) (remDepth - 1)
        Nothing ->
          let cands = [(m, applyMove m st) | m <- generateCheckMoves st]
              proved = [ (m, ch) | (m, ch) <- cands
                       , let (p, _, _) = lookupTT False ch tt
                       , p == 0 ]
          in case proved of
               ((m, ch):_) -> m : extractMatePV False ch tt (Set.insert (zobrist st) visited) (remDepth - 1)
               []          -> []
  | otherwise =
      let evasions = [(m, applyMove m st) | m <- generateEvasionMoves st]
      in case evasions of
           [] -> []
           _  ->
             let scored = [ (length subPv, m : subPv)
                          | (m, ch) <- evasions
                          , let subPv = extractMatePV True ch tt (Set.insert (zobrist st) visited) (remDepth - 1)
                          , not (null subPv) ]
                 sorted = sortBy (\(l1, _) (l2, _) -> compare l2 l1) scored
             in case sorted of
                  ((_, fullLine):_) -> fullLine
                  []                -> []

-- ============================================================================
-- 6. 通常合法手生成 ＆ ハイブリッド最善手探索 (Df-pn 512 + Garside置換表 + F5)
-- ============================================================================

generateMoves :: State -> [Move]
generateMoves State{..} =
  let myPieces = Map.toList $ Map.filter (\(Piece p _) -> p == turn) board
      boardMoves = concatMap (\(sq, pc@(Piece _ pt)) ->
                     let targets = pieceAttacks sq pc board
                         validTargets = filter (not . isFriendly) targets
                     in map (\tgt -> NormalMove sq tgt (shouldPromote pt (snd sq) (snd tgt))) validTargets
                   ) myPieces
      enemyKing = fromMaybe (5, if turn == Sente then 1 else 9) (findKing (opponent turn) board)
      emptySquares =
        sortBy (\s1 s2 -> compare (dist s1 enemyKing) (dist s2 enemyKing))
               [(x, y) | x <- [1..9], y <- [1..9], not (Map.member (x, y) board)]
      myHand = fromMaybe emptyHand (Map.lookup turn hands)
      dropMoves = [ DropMove pt sq
                  | (pt, count) <- Map.toList myHand, count > 0
                  , sq <- take 12 emptySquares
                  , not (mustPromote turn pt (snd sq)) ]
  in boardMoves ++ dropMoves
  where
    dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
    isFriendly sq = case Map.lookup sq board of
      Just (Piece p _) -> p == turn
      Nothing          -> False
    shouldPromote pt srcY tgtY =
      let inZone y = if turn == Sente then y <= 3 else y >= 7
      in (inZone srcY || inZone tgtY) && pt `elem` [Pawn, Lance, Knight, Silver, Bishop, Rook]

isSennichite :: Word64 -> Map.Map Word64 Int -> Bool
isSennichite z hist =
  case Map.lookup z hist of
    Just count -> count >= 4
    Nothing    -> False

-- | 1. まずモース誘導型 Df-pn（最大512手）で強制詰みの有無を高速チェック
--   2. 詰みがあればその最短・最善王手を返し、なければ Garside Zobrist 置換表 + モース勾配探索を実行
searchBestMoveWithCache :: State -> (Either String (Move, Maybe ([Move], Int)), State)
searchBestMoveWithCache state =
  let (isMate, matePv, usedNodes, updatedTT) = solveMateDfpn 512 6000 state
      stateWithTT = state { dfpnTT = updatedTT }
  in case matePv of
       (firstMateMove : _) | isMate ->
         (Right (firstMateMove, Just (matePv, usedNodes)), stateWithTT)
       _ ->
         case Map.lookup (zobrist stateWithTT) (cache stateWithTT) of
           Just (_, mv) ->
             let nextState = applyMove mv stateWithTT
             in if isSennichite (zobrist nextState) (history nextState)
                then evaluateAndFilterNoCache stateWithTT
                else (Right (mv, Nothing), stateWithTT)
           Nothing ->
             evaluateAndFilterNoCache stateWithTT
  where
    evaluateAndFilterNoCache :: State -> (Either String (Move, Maybe ([Move], Int)), State)
    evaluateAndFilterNoCache st =
      let moves = generateMoves st
          f5FilteredMoves = f5SyzygyFilter st moves
          (_, safeMoves) =
            partition (\m ->
              let nextState = applyMove m st
              in isSennichite (zobrist nextState) (history nextState)
            ) f5FilteredMoves
      in case f5FilteredMoves of
           [] ->
             let fallback = case moves of
                              (m:_) -> m
                              []    -> NormalMove (5, 9) (5, 8) False
             in (Right (fallback, Nothing), st)
           (firstF5:_) ->
             case safeMoves of
               [] -> (Left "draw", st)
               _  ->
                 let scoredMoves = [ (evaluateState (turn st) (applyMove m st), m) | m <- safeMoves ]
                     sorted = sortBy (\(s1, _) (s2, _) -> compare s2 s1) scoredMoves
                     (bestScore, bestMove) = case sorted of
                       (x:_) -> x
                       []    -> (0.0, firstF5)
                     newCache = Map.insert (zobrist st) (bestScore, bestMove) (cache st)
                 in (Right (bestMove, Nothing), st { cache = newCache })

-- ============================================================================
-- 7. USIメインループ（go / go mate / eval 対応）
-- ============================================================================

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  usiLoop initState

usiLoop :: State -> IO ()
usiLoop currentState = do
  eof <- isEOF
  if eof
    then exitSuccess
    else do
      line <- getLine
      let tokens = words line
      case tokens of
        [] -> usiLoop currentState
        ("usi":_) -> do
          putStrLn "id name ShogiMorseEngine_Haskell_v7_Dfpn512"
          putStrLn "id author Shogi_Topology_Project"
          putStrLn "usiok"
          usiLoop currentState

        ("isready":_) -> do
          putStrLn "readyok"
          usiLoop currentState

        ("usinewgame":_) -> do
          usiLoop initState

        ("position":remaining) -> do
          let (posType, movesTokens) =
                case remaining of
                  ("startpos":"moves":rest) -> (initState, rest)
                  ("startpos":_)            -> (initState, [])
                  ("sfen":bStr:tStr:hStr:_mNum:rest) ->
                    let mvToks = case rest of
                                   ("moves":ms) -> ms
                                   ms           -> ms
                    in (parseSfen bStr tStr hStr, mvToks)
                  _ -> (initState, remaining)
              finalState = foldl' (\st mvStr -> applyMove (parseMove mvStr) st) posType movesTokens
          usiLoop finalState { cache = cache currentState, dfpnTT = dfpnTT currentState }

        ("go":"mate":_) -> do
          let (isMate, matePv, usedNodes, updatedTT) = solveMateDfpn 512 50000 currentState
              nextState = currentState { dfpnTT = updatedTT }
          putStrLn $ "info string [Morse-Dfpn512] mate=" ++ show isMate
                  ++ " plies=" ++ show (length matePv)
                  ++ " nodes=" ++ show usedNodes
                  ++ " ttSize=" ++ show (Map.size updatedTT)
          if isMate && not (null matePv)
            then putStrLn $ "checkmate " ++ unwords (map showMove matePv)
            else putStrLn "checkmate nomate"
          usiLoop nextState

        ("go":_) -> do
          let (res, nextState) = searchBestMoveWithCache currentState
          case res of
            Left "draw" -> do
              putStrLn "bestmove draw"
              usiLoop nextState
            Left _ -> do
              putStrLn "bestmove resign"
              usiLoop nextState
            Right (bestMv, mMateInfo) -> do
              case mMateInfo of
                Just (pv, nodes) ->
                  putStrLn $ "info score mate " ++ show (length pv)
                          ++ " nodes " ++ show nodes
                          ++ " pv " ++ unwords (map showMove pv)
                Nothing -> return ()
              putStrLn $ "bestmove " ++ showMove bestMv
              usiLoop nextState

        ("eval":_) -> do
          let bd   = board currentState
              kS   = fromMaybe (5, 9) (findKing Sente bd)
              kG   = fromMaybe (5, 1) (findKing Gote  bd)
              syzS = evalSyzygyTensorShield Sente kS bd
              syzG = evalSyzygyTensorShield Gote  kG bd
              dS   = evalKingDistance Sente currentState
              dG   = evalKingDistance Gote  currentState
              ev   = evaluateState (turn currentState) currentState
          putStrLn $ "info string syzygy(S)=" ++ show syzS
                  ++ " syzygy(G)=" ++ show syzG
                  ++ " D(S)=" ++ show dS
                  ++ " D(G)=" ++ show dG
                  ++ " zobrist=" ++ show (zobrist currentState)
                  ++ " eval=" ++ show ev
          usiLoop currentState

        ("quit":_) -> exitSuccess
        _          -> usiLoop currentState
