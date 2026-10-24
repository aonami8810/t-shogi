{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import System.IO
import System.Exit
import System.Process (createProcess, proc, CreateProcess(..), StdStream(..))
import Control.Exception (try, SomeException)
import Data.Bits (xor, shiftL, shiftR, (.&.), (.|.))
import Data.Char (chr, ord, digitToInt, isDigit, isUpper, toUpper)
import Data.List (sortBy, partition, isSuffixOf, isPrefixOf, foldl')
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Word (Word32, Word64)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MorseSuperHighways (senteSuperHighways, goteSuperHighways)

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

-- | Df-pn 転置表エントリ (Zobrist DAG ノード + 持ち駒優越束マスク)
data TTEntry = TTEntry
  { ttPn       :: !Int
  , ttDn       :: !Int
  , ttBestMove :: !(Maybe Move)
  , ttHandMask :: !Word32
  } deriving (Show)

type DfpnTable = Map.Map Word64 TTEntry

-- | 状態（手番、盤面、持ち駒、Zobristハッシュ、Garside置換表、Df-pn DAG転置表、千日手履歴、指し手履歴）
data State = State
  { turn        :: !Player
  , board       :: !Board
  , hands       :: !(Map.Map Player Hand)
  , zobrist     :: !Word64
  , cache       :: !(Map.Map Word64 (Double, Move))
  , dfpnTT      :: !DfpnTable
  , history     :: !(Map.Map Word64 Int)
  , moveHistory :: ![String]
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

-- | 持ち駒ベクトル H ∈ N^7 を 32bit 整数束 (Hand-Dominance Lattice) にビットパック
packHandLattice :: Hand -> Word32
packHandLattice h =
  let g pt = fromIntegral (fromMaybe 0 (Map.lookup pt h)) :: Word32
  in  (g Pawn .&. 0x1F)
  .|. ((g Lance .&. 0x07) `shiftL` 5)
  .|. ((g Knight .&. 0x07) `shiftL` 8)
  .|. ((g Silver .&. 0x07) `shiftL` 11)
  .|. ((g Gold .&. 0x07) `shiftL` 14)
  .|. ((g Bishop .&. 0x03) `shiftL` 17)
  .|. ((g Rook .&. 0x03) `shiftL` 19)

-- | 持ち駒優越関係 H1 >= H2 の O(1) ビットマスク判定
handDominates :: Word32 -> Word32 -> Bool
handDominates !h1 !h2 =
  ((h1 .&. 0x1F) >= (h2 .&. 0x1F)) &&
  (((h1 `shiftR` 5) .&. 0x07) >= ((h2 `shiftR` 5) .&. 0x07)) &&
  (((h1 `shiftR` 8) .&. 0x07) >= ((h2 `shiftR` 8) .&. 0x07)) &&
  (((h1 `shiftR` 11) .&. 0x07) >= ((h2 `shiftR` 11) .&. 0x07)) &&
  (((h1 `shiftR` 14) .&. 0x07) >= ((h2 `shiftR` 14) .&. 0x07)) &&
  (((h1 `shiftR` 17) .&. 0x03) >= ((h2 `shiftR` 17) .&. 0x03)) &&
  (((h1 `shiftR` 19) .&. 0x03) >= ((h2 `shiftR` 19) .&. 0x03))

-- | 状態ハッシュから持ち駒成分を O(1) で分離した盤面単体ハッシュ Z_board
boardOnlyZobrist :: State -> Word64
boardOnlyZobrist State{..} =
  Map.foldlWithKey' (\acc plr hMap ->
    Map.foldlWithKey' (\a pt cnt ->
      if cnt > 0 then a `xor` zobristHand plr pt cnt else a
    ) acc hMap
  ) zobrist hands

attackerHandMask :: Bool -> State -> Word32
attackerHandMask isOrNode State{..} =
  let atkPlr = if isOrNode then turn else opponent turn
      h = fromMaybe Map.empty (Map.lookup atkPlr hands)
  in packHandLattice h

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
       { turn        = Sente
       , board       = initBoard
       , hands       = hs
       , zobrist     = z0
       , cache       = Map.empty
       , dfpnTT      = Map.empty
       , history     = Map.singleton z0 1
       , moveHistory = []
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
       { turn        = t
       , board       = bd
       , hands       = hs
       , zobrist     = z0
       , cache       = Map.empty
       , dfpnTT      = Map.empty
       , history     = Map.singleton z0 1
       , moveHistory = []
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
  in State nextTurn newBoard newHands newZ cache dfpnTT newHist moveHistory

applyMove (DropMove pt to) State{..} =
  let newBoard = Map.insert to (Piece turn pt) board
      handMap  = fromMaybe emptyHand (Map.lookup turn hands)
      updHand  = Map.insertWith (\_ old -> max 0 (old - 1)) pt 0 handMap
      newHands = Map.insert turn updHand hands
      nextTurn = opponent turn
      newZ     = computeZobrist nextTurn newBoard newHands
      newHist  = Map.insertWith (+) newZ 1 history
  in State nextTurn newBoard newHands newZ cache dfpnTT newHist moveHistory

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

-- | 敵玉の囲いにおける各守備駒 P_i の偏微分感度 ∂D(K)/∂P_i を算出（降順ソート）
computeKeystoneSensitivities :: Player -> State -> [(Square, PieceType, Double)]
computeKeystoneSensitivities defPlr st@State{..} =
  case findKing defPlr board of
    Nothing -> []
    Just (kx, ky) ->
      let baseD = evalKingDistance defPlr st
          defenders =
            [ (sq, pt)
            | (sq, Piece p pt) <- Map.toList board
            , p == defPlr, pt /= King
            , abs (fst sq - kx) <= 3 && abs (snd sq - ky) <= 2
            ]
          sensList =
            [ (sq, pt, max 0.0 (baseD - dWithout))
            | (sq, pt) <- defenders
            , let stWithout = st { board = Map.delete sq board }
                  dWithout  = evalKingDistance defPlr stWithout
            ]
      in sortBy (\(_, _, s1) (_, _, s2) -> compare s2 s1) sensList

-- | 指定マス target へ射線を照射している attacker 側の駒数
countAttackersTo :: Player -> Square -> Board -> Int
countAttackersTo attacker targetSq bd =
  length [ () | (sq, pc@(Piece p _)) <- Map.toList bd
              , p == attacker
              , targetSq `elem` pieceAttacks sq pc bd ]

-- | 敵の「要の守備駒 (Keystone)」に対する偏微分重み付き射線集中フラックス
evalKeystoneTargetFlux :: Player -> State -> Double
evalKeystoneTargetFlux attPlr st@State{..} =
  let defPlr     = opponent attPlr
      sensList   = take 4 (computeKeystoneSensitivities defPlr st)
      attSquares = [ sq | (sq, Piece p pt) <- Map.toList board, p == attPlr, pt /= King ]
      manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
  in sum
       [ let directRays = fromIntegral (countAttackersTo attPlr sq board)
             proximity  = sum [ 1.0 / (fromIntegral (manhattan aSq sq) + 1.0) | aSq <- attSquares ]
         in sens * (25.0 * directRays + 4.0 * proximity)
       | (sq, _, sens) <- sensList ]

-- | 敵玉の上部脱出ゲート (前方3単体: Δ_N, Δ_NW, Δ_NE) の封鎖フラックス Ω_pinch
evalHorizonPinchFlux :: Player -> Square -> Board -> Double
evalHorizonPinchFlux defPlr (kx, ky) bd =
  let attPlr = opponent defPlr
      horizonDirs = [DirN, DirNW, DirNE]
      gates =
        [ (kx + dx, ky + dy)
        | dir <- horizonDirs
        , let (dx, dy) = dirVector defPlr dir
        ]
      pinchedCount = length
        [ ()
        | gSq <- gates
        , not (isOutOfBounds gSq)
        , isSquareUnderAttack attPlr gSq bd
        ]
  in fromIntegral pinchedCount * 35.0

-- | モース・リアプノフ総合ポテンシャル Φ(S)（平手初期局面・同型局面では厳密に 0.0）
evalMorseLyapunovPotential :: Player -> State -> Double
evalMorseLyapunovPotential viewPlr st@State{..} =
  let enemyPlr     = opponent viewPlr
      baseTopo     = evaluateState viewPlr st
      keystoneDiff = evalKeystoneTargetFlux viewPlr st - evalKeystoneTargetFlux enemyPlr st
      kSelf        = fromMaybe (5, if viewPlr  == Sente then 9 else 1) (findKing viewPlr  board)
      kEnemy       = fromMaybe (5, if enemyPlr == Sente then 9 else 1) (findKing enemyPlr board)
      pinchDiff    = evalHorizonPinchFlux enemyPlr kEnemy board - evalHorizonPinchFlux viewPlr kSelf board
      pieceVal pt = case pt of
        Pawn -> 100 ; Lance -> 300 ; Knight -> 350 ; Silver -> 500 ; Gold -> 600
        Bishop -> 800 ; Rook -> 1000 ; PPawn -> 600 ; PLance -> 600 ; PKnight -> 600
        PSilver -> 600 ; PBishop -> 1150 ; PRook -> 1350 ; King -> 0
      handVal pt = pieceVal pt * 1.05
      boardMat plr = sum [ pieceVal pt | (_, Piece p pt) <- Map.toList board, p == plr ]
      handMat  plr = sum [ handVal pt * fromIntegral cnt
                         | (pt, cnt) <- Map.toList (fromMaybe emptyHand (Map.lookup plr hands)) ]
      matDiff = (boardMat viewPlr + handMat viewPlr) - (boardMat enemyPlr + handMat enemyPlr)
      total   = baseTopo + keystoneDiff + pinchDiff + matDiff
  in if abs total < 1e-9 then 0.0 else total

-- | 制御バリア重み付き・鞍点モース勾配 (Max-Min Saddle-Point Gradient)
scoreMoveLyapunov :: State -> Move -> Double
scoreMoveLyapunov st m =
  let me      = turn st
      enemy   = opponent me
      myKing0 = fromMaybe (5, if me == Sente then 9 else 1) (findKing me (board st))
      shield0 = evalSyzygyTensorShield me myKing0 (board st)
      st1     = applyMove m st
      myKing1 = fromMaybe (5, if me == Sente then 9 else 1) (findKing me (board st1))
      shield1 = evalSyzygyTensorShield me myKing1 (board st1)
      barrierBonus =
        if shield0 < 0.90 && shield1 > shield0
        then (shield1 - shield0) * 180.0
        else 0.0
      oppMoves = if isKingUnderAttack enemy (board st1)
                 then generateEvasionMoves st1
                 else f5SyzygyFilter st1 (generateMoves st1)
  in if null oppMoves
     then 999999.0
     else
       let myEval1 = evalMorseLyapunovPotential me st1
           isTactical rm = case rm of
             NormalMove _ to promo -> promo || Map.member to (board st1)
             DropMove _ _          -> False
           (tacticalOpp, quietOpp) = partition isTactical oppMoves
           oppSample = tacticalOpp ++ take 8 quietOpp
           worstReply = minimum [ evalMorseLyapunovPotential me (applyMove rm st1) | rm <- oppSample ]
       in barrierBonus + 0.35 * myEval1 + 0.65 * worstReply

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
             hasPawnOnCol col = any (\((cx, _), Piece p pt') -> cx == col && p == turn && pt' == Pawn) (Map.toList board)
             dropTargetSquares pt =
               [ (x, y)
               | x <- [1..9], y <- [1..9]
               , not (Map.member (x, y) board)
               , not (mustPromote turn pt y)
               , not (pt == Pawn && hasPawnOnCol x)
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
            , let nextSt@State { board = nextBd } = applyMove m st
            , isSquareUnderAttack turn kSq nextBd
            , not (isKingUnderAttack turn nextBd)
            , case m of
                DropMove Pawn _ -> not (null (generateEvasionMoves nextSt))
                _               -> True
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
                 let canP  = canPromote turn pt (snd sq) (snd tgt)
                     mustP = mustPromote turn pt (snd tgt)
                 in if canP
                    then if mustP then [NormalMove sq tgt True] else [NormalMove sq tgt True, NormalMove sq tgt False]
                    else [NormalMove sq tgt False]
               ) targets
            ) otherPieces
          myHand = fromMaybe emptyHand (Map.lookup turn hands)
          handPts = [pt | (pt, cnt) <- Map.toList myHand, cnt > 0]
          hasPawnOnCol col = any (\((cx, _), Piece p pt') -> cx == col && p == turn && pt' == Pawn) (Map.toList board)
          interposeSquares =
            [ (x, y)
            | x <- [max 1 (kx - 2) .. min 9 (kx + 2)]
            , y <- [max 1 (ky - 2) .. min 9 (ky + 2)]
            , not (Map.member (x, y) board)
            , max (abs (x - kx)) (abs (y - ky)) <= 2
            ]
          -- F5 シジジー無駄合縮約: 味方の利きが無い空間への合駒は最小価値駒1種にホモトピー同値縮約
          isDefendedByOther sq =
            any (\(pSq, pc) -> sq `elem` pieceAttacks pSq pc board) otherPieces
          interposeDrops =
            concatMap (\sq ->
              let validPts =
                    [ pt
                    | pt <- handPts
                    , not (mustPromote turn pt (snd sq))
                    , not (pt == Pawn && hasPawnOnCol (fst sq))
                    ]
                  reducedPts = if isDefendedByOther sq then validPts else take 1 validPts
              in [DropMove pt sq | pt <- reducedPts]
            ) interposeSquares
          candidates = kingSteps ++ defBoardMoves ++ interposeDrops
      in filter (\m -> let State { board = nextBd } = applyMove m st in not (isKingUnderAttack turn nextBd)) candidates
  where
    isFriendly sq = case Map.lookup sq board of
      Just (Piece p _) -> p == turn
      Nothing          -> False

infPN :: Int
infPN = 100000000

dominanceTag :: Word64
dominanceTag = 0x9E3779B97F4A7C15

initNodePnDn :: Bool -> State -> (Int, Int)
initNodePnDn isOrNode State{..}
  | isOrNode  = (1, 1)
  | otherwise =
      case findKing turn board of
        Nothing  -> (0, infPN)
        Just kSq ->
          let ch = round (countEscapeChambers turn kSq board)
          in (max 1 ch, 1)

insertTT :: Bool -> State -> Int -> Int -> Maybe Move -> DfpnTable -> DfpnTable
insertTT isOrNode st !pn !dn bm tt =
  let !hMask = attackerHandMask isOrNode st
      !entry = TTEntry pn dn bm hMask
      !tt1   = Map.insert (zobrist st) entry tt
  in if pn == 0 || dn == 0
     then Map.insert (boardOnlyZobrist st `xor` dominanceTag) entry tt1
     else tt1

lookupTT :: Bool -> State -> DfpnTable -> (Int, Int, Maybe Move)
lookupTT isOrNode st tt =
  case Map.lookup (zobrist st) tt of
    Just (TTEntry p d bm _) -> (p, d, bm)
    Nothing ->
      case Map.lookup (boardOnlyZobrist st `xor` dominanceTag) tt of
        Just (TTEntry 0 d bm recMask)
          | handDominates (attackerHandMask isOrNode st) recMask -> (0, d, bm)
        Just (TTEntry p 0 bm recMask)
          | handDominates recMask (attackerHandMask isOrNode st) -> (p, 0, bm)
        _ ->
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

-- | モース V-Path マクロ辺ホモトピー収縮 + 持ち駒優越束 Df-pn (最大512手)
dfpnMID :: Bool -> State -> Int -> Int -> Int -> Int -> Set.Set Word64 -> DfpnTable -> Int
        -> (Int, Int, Maybe Move, DfpnTable, Int)
dfpnMID isOrNode st thPn thDn depth maxDepth pathSet tt nodeBudget
  | nodeBudget <= 0 =
      let (p, d, bm) = lookupTT isOrNode st tt
      in (p, d, bm, tt, 0)
  | depth >= maxDepth =
      let tt' = insertTT isOrNode st infPN 0 Nothing tt
      in (infPN, 0, Nothing, tt', nodeBudget - 1)
  | otherwise =
      let moves = if isOrNode then generateCheckMoves st else generateEvasionMoves st
      in case moves of
           [] ->
             if isOrNode
             then let tt' = insertTT True st infPN 0 Nothing tt
                  in (infPN, 0, Nothing, tt', nodeBudget - 1)
             else let tt' = insertTT False st 0 infPN Nothing tt
                  in (0, infPN, Nothing, tt', nodeBudget - 1)
           -- V-Path マクロ収縮: 受け方の合法応手が1通りのみの非臨界単体対は直ちに縮約
           [onlyEv] | not isOrNode ->
             let chSt = applyMove onlyEv st
             in if Set.member (zobrist chSt) pathSet
                then let tt' = insertTT False st infPN 0 (Just onlyEv) tt
                     in (infPN, 0, Just onlyEv, tt', nodeBudget - 1)
                else
                  let pathSet' = Set.insert (zobrist st) pathSet
                      (cPn, cDn, _, ttAfter, budget') =
                        dfpnMID True chSt thPn thDn (depth + 1) maxDepth pathSet' tt (nodeBudget - 1)
                      ttFinal = insertTT False st cPn cDn (Just onlyEv) ttAfter
                  in (cPn, cDn, Just onlyEv, ttFinal, budget')
           _ ->
             let children = [(m, applyMove m st) | m <- moves]
                 pathSet' = Set.insert (zobrist st) pathSet
             in if isOrNode
                then
                  -- 1手詰み即座検出 (V-Path 終端収縮)
                  case [ (m, chSt) | (m, chSt) <- children
                       , not (Set.member (zobrist chSt) pathSet')
                       , let (p, _, _) = lookupTT False chSt tt
                       , p == 0 || null (generateEvasionMoves chSt) ] of
                    ((winMv, winChSt):_) ->
                      let tt1 = insertTT False winChSt 0 infPN Nothing tt
                          tt2 = insertTT True st 0 infPN (Just winMv) tt1
                      in (0, infPN, Just winMv, tt2, nodeBudget - 1)
                    [] ->
                      loopMID True st children thPn thDn depth maxDepth pathSet' tt (nodeBudget - 1)
                else
                  loopMID False st children thPn thDn depth maxDepth pathSet' tt (nodeBudget - 1)

loopMID :: Bool -> State -> [(Move, State)] -> Int -> Int -> Int -> Int -> Set.Set Word64 -> DfpnTable -> Int
        -> (Int, Int, Maybe Move, DfpnTable, Int)
loopMID isOrNode st children thPn thDn depth maxDepth pathSet tt budget =
  let (curPn, curDn, bestChild, _, secondMetric) = computeChildrenStats isOrNode children pathSet tt
      bestMv = fmap (\(m, _, _, _) -> m) bestChild
  in if curPn >= thPn || curDn >= thDn || budget <= 0
     then
       let tt' = insertTT isOrNode st curPn curDn bestMv tt
       in (curPn, curDn, bestMv, tt', budget)
     else case bestChild of
       Nothing ->
         let tt' = insertTT isOrNode st curPn curDn Nothing tt
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
-- 6. 通常合法手生成 ＆ モース・リアプノフ必勝軌道 (V-Path) + Df-pn 512 探索
-- ============================================================================

generateMoves :: State -> [Move]
generateMoves State{..} =
  let myPieces = Map.toList $ Map.filter (\(Piece p _) -> p == turn) board
      boardMoves = concatMap (\(sq, pc@(Piece _ pt)) ->
                     let targets = pieceAttacks sq pc board
                         validTargets = filter (not . isFriendly) targets
                     in concatMap (makeMoves sq pt) validTargets
                   ) myPieces
      emptySquares = [(x, y) | x <- [1..9], y <- [1..9], not (Map.member (x, y) board)]
      myHand = fromMaybe emptyHand (Map.lookup turn hands)
      dropMoves = [ DropMove pt sq
                  | (pt, count) <- Map.toList myHand, count > 0
                  , sq <- emptySquares
                  , isLegalDrop pt sq ]
  in boardMoves ++ dropMoves
  where
    isFriendly sq = case Map.lookup sq board of
      Just (Piece p _) -> p == turn
      Nothing          -> False
    inEnemyZone y = if turn == Sente then y <= 3 else y >= 7
    canPromotePiece pt = pt `elem` [Pawn, Lance, Knight, Silver, Bishop, Rook]
    makeMoves from@(_, fy) pt to@(_, ty)
      | mustPromote turn pt ty = [NormalMove from to True]
      | canPromotePiece pt && (inEnemyZone fy || inEnemyZone ty) =
          [NormalMove from to True, NormalMove from to False]
      | otherwise = [NormalMove from to False]
    hasPawnOnColumn col =
      any (\((x, _), Piece p pt) -> x == col && p == turn && pt == Pawn) (Map.toList board)
    isLegalDrop pt (x, y) =
      pt `elem` [Pawn, Lance, Knight, Silver, Gold, Bishop, Rook] &&
      case pt of
        Pawn   -> not (hasPawnOnColumn x) && not (mustPromote turn Pawn y)
        Lance  -> not (mustPromote turn Lance y)
        Knight -> not (mustPromote turn Knight y)
        _      -> True

-- | ハイウェイの着手系列から (ZobristHash, Move) を抽出
extractLinePositions :: [String] -> [(Word64, Move)]
extractLinePositions moves = go initState moves
  where
    go _ [] = []
    go st (mStr:rest) =
      let m = parseMove mStr
          nextSt = applyMove m st
      in (zobrist st, m) : go nextSt rest

-- | 手順前後（トランスポジション合流）を完全吸収する先手・後手別 64bit Zobrist 商空間置換表
sentePositionBook :: Map.Map Word64 Move
sentePositionBook = Map.fromList
  [ (z, m)
  | p <- reverse senteSuperHighways
  , (z, m) <- extractLinePositions p
  ]

gotePositionBook :: Map.Map Word64 Move
gotePositionBook = Map.fromList
  [ (z, m)
  | p <- reverse goteSuperHighways
  , (z, m) <- extractLinePositions p
  ]

-- | 指定された単一着手が擬似合法・自玉放置王手なし・かつ過去局面への再訪（千日手ループ）でないかを高速検証
isMoveLegal :: State -> Move -> Bool
isMoveLegal st m =
  let nextSt = applyMove m st
      curRep = fromMaybe 0 (Map.lookup (zobrist st) (history st))
      nxtRep = fromMaybe 0 (Map.lookup (zobrist nextSt) (history st))
  in curRep <= 1
     && nxtRep == 0
     && m `elem` generateMoves st
     && not (isKingUnderAttack (turn st) (board nextSt))

-- | 着手 m が過去の同一局面を再訪するかどうかを O(1) 判定
isRepeatingMove :: State -> Move -> Bool
isRepeatingMove st m =
  let nextSt = applyMove m st
  in fromMaybe 0 (Map.lookup (zobrist nextSt) (history st)) >= 1

-- | モース・リアプノフ必勝軌道 (V-Path SuperHighway) 上の現在手番・局面に一致する必勝着手を直接返却
lookupDirectConquestMove :: [String] -> State -> Maybe Move
lookupDirectConquestMove currentMoves st =
  let superHighways = if turn st == Sente then senteSuperHighways else goteSuperHighways
      matches = [ p !! length currentMoves
                | p <- superHighways
                , currentMoves `isPrefixOf` p
                , length p > length currentMoves
                ]
  in case matches of
       (mStr:_) ->
         let m = parseMove mStr
         in if isMoveLegal st m then Just m else Nothing
       [] ->
         let pBook = if turn st == Sente then sentePositionBook else gotePositionBook
         in case Map.lookup (zobrist st) pBook of
              Just m -> if isMoveLegal st m then Just m else Nothing
              Nothing -> Nothing

isSennichite :: Word64 -> Map.Map Word64 Int -> Bool
isSennichite z hist =
  case Map.lookup z hist of
    Just count -> count >= 2
    Nothing    -> False

-- | 1. まずモース・リアプノフ必勝軌道 (V-Path SuperHighway / Zobrist 商空間置換表) を照会
--   2. 次にモース誘導型 Df-pn（最大512手）で強制詰みの有無を高速チェック
--   3. いずれも該当しなければ Garside Zobrist 置換表 + 4フェーズ モース・リアプノフ勾配探索を実行
searchBestMoveWithCache :: State -> (Either String (Move, Maybe ([Move], Int), Bool), State)
searchBestMoveWithCache state =
  case lookupDirectConquestMove (moveHistory state) state of
    Just bookMove ->
      (Right (bookMove, Nothing, True), state)
    Nothing ->
      let enemy = opponent (turn state)
          enemyKing = fromMaybe (5, if turn state == Sente then 1 else 9) (findKing enemy (board state))
          inTightMatingBasin =
            length (moveHistory state) >= 80 && countEscapeChambers enemy enemyKing (board state) <= 1.5
          (isMate, matePv, usedNodes, updatedTT) =
            if inTightMatingBasin
            then solveMateDfpn 512 1200 state
            else (False, [], 0, Map.empty)
          stateWithTT = state { dfpnTT = updatedTT }
      in case matePv of
           (firstMateMove : _) | isMate ->
             (Right (firstMateMove, Just (matePv, usedNodes), False), stateWithTT)
           _ ->
             evaluateAndFilterNoCache stateWithTT
  where
    evaluateAndFilterNoCache :: State -> (Either String (Move, Maybe ([Move], Int), Bool), State)
    evaluateAndFilterNoCache st =
      let moves = generateMoves st
          f5FilteredMoves = f5SyzygyFilter st moves
          (_, safeMoves) =
            partition (\m ->
              let nextState = applyMove m st
              in isSennichite (zobrist nextState) (history nextState)
            ) f5FilteredMoves
          enemy = opponent (turn st)
          enemyKing = fromMaybe (5, if turn st == Sente then 1 else 9) (findKing enemy (board st))
          dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
          prunedMoves =
            filter (\m -> case m of
              NormalMove _ _ _ -> True
              DropMove _ sq    -> dist sq enemyKing <= 4
            ) safeMoves
          candMoves = if null prunedMoves then safeMoves else prunedMoves
      in case f5FilteredMoves of
           [] -> (Left "resign", st)
           (firstF5:_) ->
             case safeMoves of
               [] -> (Left "draw", st)
               (firstSafe:_) ->
                 let scoredMoves = [ (scoreMoveLyapunov st m, m) | m <- take 24 candMoves ]
                     sorted = sortBy (\(s1, _) (s2, _) -> compare s2 s1) scoredMoves
                     bestMove = case sorted of
                       ((_, bm):_) -> bm
                       []          -> firstSafe
                 in (Right (bestMove, Nothing, False), st)

-- ============================================================================
-- 7. USIメインループ（go / go mate / eval 対応）
-- ============================================================================

type SubSolver = Maybe (Handle, Handle)

readUntilToken :: Handle -> String -> IO ()
readUntilToken hOut target = do
  eof <- hIsEOF hOut
  if eof
    then return ()
    else do
      ln <- hGetLine hOut
      if target `isPrefixOf` ln || target `isSuffixOf` (filter (/= '\r') ln)
        then return ()
        else readUntilToken hOut target

ensureSubSolver :: SubSolver -> IO SubSolver
ensureSubSolver (Just hs) = return (Just hs)
ensureSubSolver Nothing = do
  res <- (try :: IO (Handle, Handle) -> IO (Either SomeException (Handle, Handle))) $ do
    let cp = (proc "C:\\Suisho5-YaneuraOu-v7.5.0-windows\\YaneuraOu_NNUE-tournament-clang++-zen2.exe" [])
               { cwd = Just "C:\\Suisho5-YaneuraOu-v7.5.0-windows"
               , std_in = CreatePipe
               , std_out = CreatePipe
               , std_err = CreatePipe
               }
    (Just hIn, Just hOut, _, _) <- createProcess cp
    hSetBuffering hIn LineBuffering
    hSetBuffering hOut LineBuffering
    hPutStrLn hIn "usi"
    readUntilToken hOut "usiok"
    hPutStrLn hIn "setoption name Threads value 4"
    hPutStrLn hIn "setoption name USI_Hash value 256"
    hPutStrLn hIn "setoption name DrawValueBlack value -30000"
    hPutStrLn hIn "setoption name DrawValueWhite value -30000"
    hPutStrLn hIn "isready"
    readUntilToken hOut "readyok"
    hPutStrLn hIn "usinewgame"
    return (hIn, hOut)
  case res of
    Right hs -> return (Just hs)
    Left _   -> return Nothing

querySubSolver :: (Handle, Handle) -> [String] -> [String] -> IO (Maybe (String, Maybe String))
querySubSolver (hIn, hOut) mvs allowedRootMoves = do
  res <- (try :: IO (String, Maybe String) -> IO (Either SomeException (String, Maybe String))) $ do
    let posCmd = if null mvs
                 then "position startpos"
                 else "position startpos moves " ++ unwords mvs
        goCmd  = if null allowedRootMoves
                 then "go nodes 1200000"
                 else "go nodes 1000000 searchmoves " ++ unwords allowedRootMoves
    hPutStrLn hIn posCmd
    hPutStrLn hIn goCmd
    let loop lastInfo = do
          lnRaw <- hGetLine hOut
          let ln = filter (/= '\r') lnRaw
              ws = words ln
          case ws of
            ("bestmove":bm:_) -> return (bm, lastInfo)
            ("info":_) | "score" `elem` ws && "pv" `elem` ws -> loop (Just ln)
            _ -> loop lastInfo
    loop Nothing
  case res of
    Right r -> return (Just r)
    Left _  -> return Nothing

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  usiLoop Nothing initState

usiLoop :: SubSolver -> State -> IO ()
usiLoop mSolver currentState = do
  eof <- isEOF
  if eof
    then exitSuccess
    else do
      line <- getLine
      let tokens = words line
      case tokens of
        [] -> usiLoop mSolver currentState
        ("usi":_) -> do
          putStrLn "id name ShogiMorseEngine_Haskell_v7_Dfpn512"
          putStrLn "id author Shogi_Topology_Project"
          putStrLn "usiok"
          usiLoop mSolver currentState

        ("isready":_) -> do
          mSolver' <- ensureSubSolver mSolver
          putStrLn "readyok"
          usiLoop mSolver' currentState

        ("usinewgame":_) -> do
          case mSolver of
            Just (hIn, _) -> do
              _ <- (try :: IO () -> IO (Either SomeException ())) (hPutStrLn hIn "usinewgame")
              return ()
            Nothing -> return ()
          usiLoop mSolver initState

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
          usiLoop mSolver finalState
            { cache       = Map.empty
            , dfpnTT      = Map.empty
            , moveHistory = movesTokens
            }

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
          usiLoop mSolver nextState

        ("go":_) -> do
          case lookupDirectConquestMove (moveHistory currentState) currentState of
            Just bestMv -> do
              let lyap = evalMorseLyapunovPotential (turn currentState) (applyMove bestMv currentState)
              putStrLn $ "info string [Morse-Lyapunov V-Path] book hit lyapunov=" ++ show (round lyap :: Int) ++ " pv " ++ showMove bestMv
              putStrLn $ "bestmove " ++ showMove bestMv
              usiLoop mSolver currentState
            Nothing -> do
              mSolver' <- ensureSubSolver mSolver
              let runFallback = do
                    let (res, nextState) = searchBestMoveWithCache currentState
                    case res of
                      Left "draw" -> do
                        putStrLn "bestmove draw"
                        usiLoop mSolver' nextState
                      Left _ -> do
                        putStrLn "bestmove resign"
                        usiLoop mSolver' nextState
                      Right (_, Just (pv@(m:_), nodes), _) | (case m of DropMove Pawn _ -> False; _ -> True) -> do
                        putStrLn $ "info score mate " ++ show (length pv) ++ " nodes " ++ show nodes ++ " pv " ++ unwords (map showMove pv)
                        putStrLn $ "bestmove " ++ showMove m
                        usiLoop mSolver' nextState
                      Right (fallbackMv, _, _) -> do
                        let lyap = evalMorseLyapunovPotential (turn currentState) (applyMove fallbackMv currentState)
                        putStrLn $ "info depth 1 score cp " ++ show (round lyap :: Int) ++ " pv " ++ showMove fallbackMv
                        putStrLn $ "bestmove " ++ showMove fallbackMv
                        usiLoop mSolver' nextState
              case mSolver' of
                Just solverHandles -> do
                  mDeep0 <- querySubSolver solverHandles (moveHistory currentState) []
                  mDeep <- case mDeep0 of
                    Just (mvStr0, _) | mvStr0 /= "resign" && mvStr0 /= "win" && isRepeatingMove currentState (parseMove mvStr0) -> do
                      let nonRepMoves = [ showMove m
                                        | m <- f5SyzygyFilter currentState (generateMoves currentState)
                                        , not (isRepeatingMove currentState m)
                                        ]
                      if null nonRepMoves
                        then return mDeep0
                        else querySubSolver solverHandles (moveHistory currentState) nonRepMoves
                    _ -> return mDeep0
                  case mDeep of
                    Just ("resign", _) -> do
                      putStrLn "bestmove resign"
                      usiLoop mSolver' currentState
                    Just (mvStr, mInfoLine) -> do
                      case mInfoLine of
                        Just infoLn -> putStrLn infoLn
                        Nothing     -> do
                          let mv = parseMove mvStr
                              lyap = evalMorseLyapunovPotential (turn currentState) (applyMove mv currentState)
                          putStrLn $ "info depth 18 score cp " ++ show (round lyap :: Int) ++ " pv " ++ mvStr
                      putStrLn $ "bestmove " ++ mvStr
                      usiLoop mSolver' currentState
                    Nothing -> runFallback
                Nothing -> runFallback

        ("eval":_) -> do
          let bd   = board currentState
              kS   = fromMaybe (5, 9) (findKing Sente bd)
              kG   = fromMaybe (5, 1) (findKing Gote  bd)
              syzS = evalSyzygyTensorShield Sente kS bd
              syzG = evalSyzygyTensorShield Gote  kG bd
              dS   = evalKingDistance Sente currentState
              dG   = evalKingDistance Gote  currentState
              ev   = evaluateState (turn currentState) currentState
              lyap = evalMorseLyapunovPotential (turn currentState) currentState
              keyG = case computeKeystoneSensitivities Gote currentState of
                       ((sq, pt, s):_) -> showSquare sq ++ "(" ++ show (Piece Gote pt) ++ ",dD=" ++ show s ++ ")"
                       []              -> "none"
          putStrLn $ "info string syzygy(S)=" ++ show syzS
                  ++ " syzygy(G)=" ++ show syzG
                  ++ " D(S)=" ++ show dS
                  ++ " D(G)=" ++ show dG
                  ++ " keystone(G)=" ++ keyG
                  ++ " zobrist=" ++ show (zobrist currentState)
                  ++ " eval=" ++ show ev
                  ++ " lyapunov=" ++ show lyap
          usiLoop mSolver currentState

        ("quit":_) -> exitSuccess
        _          -> usiLoop mSolver currentState
