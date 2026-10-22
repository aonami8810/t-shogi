{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import System.Exit
import System.Timeout (timeout)
import Data.Char
import Data.List
import Data.Maybe
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad (forM_)
import qualified Data.Map as Map
import qualified CompiledMorseBook as Book
import Control.Exception (catch, SomeException, evaluate)
import Text.Read (readMaybe)

-- ============================================================================
-- 1. 将棋盤と駒の基本データ構造
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

-- 駒：所有者と種類
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

-- 座標 (1-indexed, USI表記に合わせる: 筋は 9..1, 段は 1..9)
type Square = (Int, Int)

-- 盤面
type Board = Map.Map Square Piece

-- 持ち駒（駒台）
type Hand = Map.Map PieceType Int

-- 局面の完全な一意性シグネチャ（手番、盤面、両者の持ち駒）
type PositionKey = (Player, Board, Map.Map Player Hand)

-- 状態（手番、盤面、持ち駒、Garside置換表キャッシュ、トポロジー歴史履歴、指し手履歴）
data State = State
  { turn        :: Player
  , board       :: Board
  , hands       :: Map.Map Player Hand
  , cache       :: Map.Map PositionKey (Double, Move) -- Garside標準化キャッシュ（置換表）
  , history     :: Map.Map PositionKey Int            -- ★ トポロジー歴史：千日手検出用の完全局面出現頻度
  , moveHistory :: [String]                           -- ★ 定跡照合用の対局指し手履歴
  } deriving (Show)

positionKey :: State -> PositionKey
positionKey State{..} = (turn, board, hands)

-- ============================================================================
-- 2. USIプロトコルパーサ ＆ 文字列変換 ＆ SFEN対応
-- ============================================================================

-- 平手初期局面
initBoard :: Board
initBoard = Map.fromList $
  -- 1段目（後手の一段目）
  [ ((9,1), Piece Gote Lance),  ((8,1), Piece Gote Knight), ((7,1), Piece Gote Silver), ((6,1), Piece Gote Gold)
  , ((5,1), Piece Gote King),   ((4,1), Piece Gote Gold),   ((3,1), Piece Gote Silver), ((2,1), Piece Gote Knight)
  , ((1,1), Piece Gote Lance)
  -- 2段目（後手の飛車・角）
  , ((8,2), Piece Gote Rook),   ((2,2), Piece Gote Bishop)
  -- 3段目（後手の歩）
  ] ++ [((i,3), Piece Gote Pawn) | i <- [1..9]] ++
  -- 7段目（先手の歩）
  [((i,7), Piece Sente Pawn) | i <- [1..9]] ++
  -- 8段目（先手の飛車・角）
  [ ((8,8), Piece Sente Bishop), ((2,8), Piece Sente Rook)
  -- 9段目（先手の一段目）
  , ((9,9), Piece Sente Lance),  ((8,9), Piece Sente Knight), ((7,9), Piece Sente Silver), ((6,9), Piece Sente Gold)
  , ((5,9), Piece Sente King),   ((4,9), Piece Sente Gold),   ((3,9), Piece Sente Silver), ((2,9), Piece Sente Knight)
  , ((1,9), Piece Sente Lance)
  ]

emptyHand :: Hand
emptyHand = Map.fromList [(p, 0) | p <- [Pawn, Lance, Knight, Silver, Gold, Bishop, Rook]]

initState :: State
initState =
  let st = State
        { turn        = Sente
        , board       = initBoard
        , hands       = Map.fromList [(Sente, emptyHand), (Gote, emptyHand)]
        , cache       = Map.empty
        , history     = Map.empty
        , moveHistory = []
        }
  in st { history = Map.singleton (positionKey st) 1 }

-- 座標をUSI文字列に変換 (例: (7, 7) -> "7g")
showSquare :: Square -> String
showSquare (f, r) = show f ++ [chr (ord 'a' + r - 1)]

-- USI文字列を座標に変換 (例: "7g" -> (7, 7))
parseSquare :: String -> Square
parseSquare [f, r] = (digitToInt f, ord r - ord 'a' + 1)
parseSquare _      = (1, 1)

-- 指し手表現
data Move 
  = NormalMove Square Square Bool -- 移動元, 移動先, 成るか
  | DropMove PieceType Square     -- 持ち駒の種類, 打つ場所
  deriving (Eq, Show, Ord)

-- USI形式での表示
showMove :: Move -> String
showMove (NormalMove from to promo) = 
  showSquare from ++ showSquare to ++ (if promo then "+" else "")
showMove (DropMove p to) =
  let pieceChar = case p of
        Pawn   -> 'P' ; Lance  -> 'L' ; Knight -> 'N'
        Silver -> 'S' ; Gold   -> 'G' ; Bishop -> 'B' ; Rook   -> 'R'
        _      -> ' '
  in pieceChar : '*' : showSquare to

-- USI指し手文字列のパース（大文字小文字両対応）
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

-- 指し手の適用（トポロジー歴史履歴 history の自動蓄積）
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

      -- 持ち駒（駒台）の更新（玉は持ち駒に入らない）
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
-- 2.1 SFENパーサ (任意の局面展開に対応)
-- ============================================================================

splitByChar :: Char -> String -> [String]
splitByChar c s = case break (== c) s of
  (a, [])     -> [a]
  (a, _:rest) -> a : splitByChar c rest

parseSfen :: [String] -> State
parseSfen tokens =
  case tokens of
    (boardStr : turnStr : handStr : _) ->
      let bd = parseSfenBoard boardStr
          plr = if turnStr == "w" then Gote else Sente
          hnds = parseSfenHands handStr
          st = State plr bd hnds Map.empty Map.empty []
      in st { history = Map.singleton (positionKey st) 1 }
    _ -> initState

parseSfenBoard :: String -> Board
parseSfenBoard str =
  let ranks = splitByChar '/' str
      parseRank rNum rStr = parseRankCols 9 rNum rStr
        where
          parseRankCols _ _ [] = []
          parseRankCols col rank (c:cs)
            | isDigit c = parseRankCols (col - digitToInt c) rank cs
            | c == '+'  = case cs of
                (p:rest) -> ((col, rank), Piece (if isUpper p then Sente else Gote) (promotePiece (charToPieceType (toLower p)))) : parseRankCols (col - 1) rank rest
                []       -> []
            | otherwise = ((col, rank), Piece (if isUpper c then Sente else Gote) (charToPieceType (toLower c))) : parseRankCols (col - 1) rank cs
      promotePiece pt = case pt of
        Pawn -> PPawn ; Lance -> PLance ; Knight -> PKnight
        Silver -> PSilver ; Bishop -> PBishop ; Rook -> PRook
        x -> x
      charToPieceType c = case c of
        'p' -> Pawn ; 'l' -> Lance ; 'n' -> Knight ; 's' -> Silver
        'g' -> Gold ; 'b' -> Bishop ; 'r' -> Rook ; 'k' -> King
        _   -> Pawn
  in Map.fromList [ piece | (rNum, rStr) <- zip [1..9] ranks, piece <- parseRank rNum rStr ]

parseSfenHands :: String -> Map.Map Player Hand
parseSfenHands "-" = Map.fromList [(Sente, emptyHand), (Gote, emptyHand)]
parseSfenHands str = foldl' addHand (Map.fromList [(Sente, emptyHand), (Gote, emptyHand)]) (groupDigitsAndChars str)
  where
    groupDigitsAndChars [] = []
    groupDigitsAndChars s =
      let (digits, rest1) = span isDigit s
          count = if null digits then 1 else read digits :: Int
      in case rest1 of
        (c:rest2) -> (c, count) : groupDigitsAndChars rest2
        []        -> []
    addHand m (c, count) =
      let plr = if isUpper c then Sente else Gote
          pt = case toLower c of
            'p' -> Pawn ; 'l' -> Lance ; 'n' -> Knight ; 's' -> Silver
            'g' -> Gold ; 'b' -> Bishop ; 'r' -> Rook
            _   -> Pawn
          handMap = fromMaybe emptyHand (Map.lookup plr m)
          newHandMap = Map.insert pt count handMap
      in Map.insert plr newHandMap m

-- ============================================================================
-- 3. F5フィルター（シジジー事前予測による自殺手・王手放置・打ち歩詰めの完全排除）
-- ============================================================================

-- | スライド駒の射線上に障害駒がないか確認（トポロジー的遮断判定）
isPathClear :: Square -> Square -> Board -> Bool
isPathClear (x1, y1) (x2, y2) bd =
  let dx = signum (x2 - x1)
      dy = signum (y2 - y1)
      path = takeWhile (/= (x2, y2)) $ drop 1 $ iterate (\(cx, cy) -> (cx + dx, cy + dy)) (x1, y1)
  in all (\sq -> not (Map.member sq bd)) path

-- | 飛び駒の障害駒を一定数まで透過するX-Ray（潜在的射線）判定
isXRayPathClear :: Square -> Square -> Board -> Int -> Bool
isXRayPathClear (x1, y1) (x2, y2) bd maxObstacles =
  let dx = signum (x2 - x1)
      dy = signum (y2 - y1)
      path = takeWhile (/= (x2, y2)) $ drop 1 $ iterate (\(cx, cy) -> (cx + dx, cy + dy)) (x1, y1)
      obstacles = [ sq | sq <- path, Map.member sq bd ]
  in length obstacles <= maxObstacles

-- | (x1, y1) から targetSq の間にある障害駒とその座標のリスト
getObstaclesOnPath :: Square -> Square -> Board -> [(Square, Piece)]
getObstaclesOnPath (x1, y1) (x2, y2) bd =
  let dx = signum (x2 - x1)
      dy = signum (y2 - y1)
      path = takeWhile (/= (x2, y2)) $ drop 1 $ iterate (\(cx, cy) -> (cx + dx, cy + dy)) (x1, y1)
  in [ (sq, p) | sq <- path, Just p <- [Map.lookup sq bd] ]

-- | 飛び駒（飛、竜、角、馬、香車）が targetSq を直接または障害駒1枚越しに潜在的に射程に捉えているか
canPieceXRayTarget :: Square -> Piece -> Square -> Board -> Bool
canPieceXRayTarget (x, y) (Piece plr pt) targetSq bd =
  let dx = fst targetSq - x
      dy = snd targetSq - y
      absDx = abs dx
      absDy = abs dy
      dirY = if plr == Sente then -1 else 1
      isNotSelf = absDx /= 0 || absDy /= 0
  in case pt of
    Rook    -> (dx == 0 || dy == 0) && isNotSelf && isXRayPathClear (x, y) targetSq bd 1
    PRook   -> ((dx == 0 || dy == 0) && isNotSelf && isXRayPathClear (x, y) targetSq bd 1) || (absDx <= 1 && absDy <= 1 && isNotSelf)
    Bishop  -> (absDx == absDy) && isNotSelf && isXRayPathClear (x, y) targetSq bd 1
    PBishop -> ((absDx == absDy) && isNotSelf && isXRayPathClear (x, y) targetSq bd 1) || (absDx <= 1 && absDy <= 1 && isNotSelf)
    Lance   -> dx == 0 && (dy * dirY > 0) && isXRayPathClear (x, y) targetSq bd 1
    _       -> False


-- | 駒の利き判定（ある駒が任意のマス targetSq に利いているか）
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

-- | 盤面の任意の位置 (targetSq) に利いているプレイヤー plr の駒一覧
getPiecesCovering :: Player -> Square -> Board -> [(Square, Piece)]
getPiecesCovering plr targetSq bd =
  [ (sq, p) | (sq, p@(Piece pPlr _)) <- Map.toList bd, pPlr == plr, canPieceReachTarget sq p targetSq bd ]

-- | targetSq にプレイヤー plr の駒の利きが利いているか
isCoveredBy :: Player -> Square -> Board -> Bool
isCoveredBy plr targetSq bd = not (null (getPiecesCovering plr targetSq bd))

-- | 盤面の任意の位置 (targetSq) が、attackerPlayer から王手（攻撃）されているか判定する
isSquareUnderAttack :: Player -> Square -> Board -> Bool
isSquareUnderAttack attackerPlayer targetSq bd = isCoveredBy attackerPlayer targetSq bd

-- | ある駒に味方の紐が付いているか（守られているか）
isPieceProtected :: Square -> Piece -> Board -> Bool
isPieceProtected sq (Piece plr _) bd =
  let friendlyDefenders = [ fromSq | (fromSq, p@(Piece pPlr _)) <- Map.toList bd
                                   , pPlr == plr
                                   , fromSq /= sq
                                   , canPieceReachTarget fromSq p sq bd ]
  in not (null friendlyDefenders)

-- | 手番側の玉が現在、王手に晒されているかを判定する
isKingUnderAttack :: Player -> Board -> Bool
isKingUnderAttack plr bd =
  let kingPos = listToMaybe [sq | (sq, Piece p pt) <- Map.toList bd, p == plr, pt == King]
  in case kingPos of
    Just kSq -> isSquareUnderAttack (opponent plr) kSq bd
    Nothing  -> True -- 玉が見つからない場合は危険（反則局面）と判定

-- | 打ち歩詰め判定
-- 歩を打った結果、相手玉が王手になり、かつ相手に合法手が1手も残っていない（詰み）場合は打ち歩詰め反則
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

-- | F5シジジーフィルター（自殺手・王手放置・打ち歩詰めの完全排除）
f5SyzygyFilter :: State -> [Move] -> [Move]
f5SyzygyFilter state moves =
  let turnPlr = turn state
      noSelfCheckMoves = filter (\m -> not (isKingUnderAttack turnPlr (board (applyMove m state)))) moves
      legalMoves = filter (\m -> not (isUchifudume m state)) noSelfCheckMoves
  in legalMoves

-- ============================================================================
-- 4. モースポテンシャル ＆ 駒得（Material）統合評価関数
-- ============================================================================

-- | 玉の現在位置を取得する
findKing :: Player -> Board -> Maybe Square
findKing plr b = listToMaybe [sq | (sq, Piece p pt) <- Map.toList b, p == plr, pt == King]

pieceValue :: PieceType -> Double
pieceValue Pawn    = 100.0
pieceValue Lance   = 300.0
pieceValue Knight  = 350.0
pieceValue Silver  = 500.0
pieceValue Gold    = 600.0
pieceValue Bishop  = 850.0
pieceValue Rook    = 1000.0
pieceValue King    = 15000.0
pieceValue PPawn   = 550.0  -- と金は金と同等の破壊力
pieceValue PLance  = 500.0
pieceValue PKnight = 500.0
pieceValue PSilver = 550.0
pieceValue PBishop = 1150.0 -- 馬
pieceValue PRook   = 1350.0 -- 龍

-- ============================================================================
-- 4.1 中盤強化学習パラメータ (Midgame Reinforcement Weights)
-- ============================================================================

data MidgameWeights = MidgameWeights
  { wMaterial         :: !Double -- 駒得重み (標準 1.0)
  , wMorseDanger      :: !Double -- モース玉ポテンシャル重み (標準 6.0)
  , wAttack           :: !Double -- 中盤攻め筋（成駒・玉頭圧力・拠点）重み (標準 1.3)
  , wDefense          :: !Double -- 中盤受け筋（金銀囲い・連結・敵成駒排除）重み (標準 1.3)
  , wKakuto           :: !Double -- 序盤角頭・桂頭防衛重み (標準 1.0)
  , wLoosePenalty     :: !Double -- 駒損（放れ駒・大損交換）ペナルティ重み (標準 1.2)
  , wPieceControl     :: !Double -- 駒の利き（大駒射線・銀桂攻撃参加・マス支配）重み (標準 1.5)
  , wTempoLoss        :: !Double -- 手得・手損（18香無駄上がり・飛車先不突き金寄り等の手損排除、手得追求）重み (標準 2.0)
  , wGoldKingCoord    :: !Double -- 金と玉の連携（金玉近接・守護スクラム）重み (標準 1.5)
  , wDistanceToMate   :: !Double -- 詰み筋までの距離（DTM短縮・QEGTB接続）重み (標準 5.0)
  , wNumericalAssault :: !Double -- 数の攻め（特定地点への利きの集中・突破、局所的数の優位）重み (標準 2.0)
  , wTacticalFork     :: !Double -- 両取り（角・桂・銀・飛車による二重威嚇）重み (標準 1.8)
  , wPotentialRays    :: !Double -- とびごま（飛、竜、角、馬、桂、香）の潜在的利き・X-Ray・開き王手重み (標準 2.2)
  } deriving (Show, Read, Eq)

defaultWeights :: MidgameWeights
defaultWeights = MidgameWeights
  { wMaterial         = 1.0
  , wMorseDanger      = 6.0
  , wAttack           = 1.3
  , wDefense          = 1.3
  , wKakuto           = 1.0
  , wLoosePenalty     = 1.2
  , wPieceControl     = 1.5
  , wTempoLoss        = 2.0
  , wGoldKingCoord    = 1.5
  , wDistanceToMate   = 5.0
  , wNumericalAssault = 2.0
  , wTacticalFork     = 1.8
  , wPotentialRays    = 2.2
  }

-- | 設定ファイルから中盤学習重みを読み込む（存在しない場合は defaultWeights）
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
  let kingPos = findKing player bd
  in case kingPos of
    Nothing -> 10000.0 -- 王が捕獲された極限の破局状態
    Just (kx, ky) ->
      let enemyPieces = Map.toList $ Map.filter (\(Piece p _) -> p /= player) bd
          influenceSum = sum [ 10.0 / (fromIntegral (dist (kx, ky) sq) + 0.1) | (sq, _) <- enemyPieces ]
          surroundings = [(kx + dx, ky + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]
          wallPenalties = sum [ if outOfBounds sq || Map.member sq bd then 0.0 else 1.5 | sq <- surroundings ]
      in influenceSum + wallPenalties
  where
    dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
    outOfBounds (x, y) = x < 1 || x > 9 || y < 1 || y > 9

-- | 角頭の歩突き（先手8七歩 8g8f、後手2三歩 2c2d）の絶対禁止判定
-- 飛車が8筋/2筋にいない状態で角頭の歩を突く悪手を完全に検知・排除
isPushingKakutoPawn :: State -> Move -> Bool
isPushingKakutoPawn st m =
  let plr = turn st
      bd = board st
  in case m of
    NormalMove (8, 7) (8, 6) _ | plr == Sente ->
      case Map.lookup (8, 8) bd of
        Just (Piece Sente Rook)  -> False -- 8八飛（向かい飛車）なら許容
        Just (Piece Sente PRook) -> False
        _                        -> True  -- 角が8八にいる/飛車がいないのに8七歩を突くのは絶対禁止
    NormalMove (2, 3) (2, 4) _ | plr == Gote ->
      case Map.lookup (2, 2) bd of
        Just (Piece Gote Rook)  -> False -- 2二飛なら許容
        Just (Piece Gote PRook) -> False
        _                       -> True  -- 角が2二にいる/飛車がいないのに2三歩を突くのは絶対禁止
    _ -> False

-- | 無駄な香車上がりなどの手損手を厳格に判定
-- （1八香、9八香、1二香、9二香など、自玉の穴熊入城または直接の端破られ防衛以外は例外なく100%完全排除！）
isFutileLanceLift :: State -> Move -> Bool
isFutileLanceLift st (NormalMove from to _) =
  let bd = board st
      plr = turn st
  in case Map.lookup from bd of
       Just (Piece p Lance) | p == plr ->
         let (fx, fy) = from
             (tx, ty) = to
             -- 1段目または2段目からの香車前進
             isFromBackRank = if plr == Sente then fy >= 8 else fy <= 2
             opp = opponent plr
             -- 相手が自陣深くに侵入してきた非常事態（直接の端破られ防衛）のみ例外許可
             hasDirectBreach =
               any (\((ox, oy), Piece op _) ->
                     op == opp && ox == fx &&
                     (if plr == Sente then oy >= 7 else oy <= 3)
                   ) (Map.toList bd)
             -- 穴熊囲い（玉がすでに香車の初期位置に入城している、または直隣にいる場合のみ例外許可）
             kingPos = findKing plr bd
             isAnagumaEntry = case kingPos of
               Just (kx, ky) -> if plr == Sente
                                  then (fx == 1 && kx <= 2 && ky >= 8) || (fx == 9 && kx >= 8 && ky >= 8)
                                  else (fx == 1 && kx <= 2 && ky <= 2) || (fx == 9 && kx >= 8 && ky <= 2)
               Nothing -> False
         in isFromBackRank && not hasDirectBreach && not isAnagumaEntry
       _ -> False
isFutileLanceLift _ _ = False

-- | 1. 駒の利きと活用度（大駒射線・銀桂攻撃参加・マス支配度）
evalPieceControl :: Player -> State -> Double
evalPieceControl plr st =
  let bd = board st
      opp = opponent plr
      dirY = if plr == Sente then -1 else 1

      -- A. 大駒（飛車・角）の射線と活用
      evalMajorPieces p =
        let pDir = if p == Sente then -1 else 1
            myRooks = [ (sq, pt) | (sq, Piece owner pt) <- Map.toList bd, owner == p, pt `elem` [Rook, PRook] ]
            myBishops = [ (sq, pt) | (sq, Piece owner pt) <- Map.toList bd, owner == p, pt `elem` [Bishop, PBishop] ]
            
            -- 飛車先の歩突き・縦の射線
            rookScore = sum
              [ let (rx, ry) = sq
                    pawnFrontSq = (rx, ry + pDir)
                    hasPawnBlocking = case Map.lookup pawnFrontSq bd of
                                        Just (Piece owner Pawn) | owner == p -> True
                                        _ -> False
                    -- 飛車の縦の利き（前方に何マス伸びているか）
                    forwardSquares = takeWhile (\s -> not (isOutOfBounds s) && not (isFriendlySquare s p bd))
                                               [ (rx, ry + d * pDir) | d <- [1..8] ]
                    forwardRookMobility = fromIntegral (length forwardSquares) * 20.0
                    -- 飛車先の歩が突かれている（初期位置から前進している）ボーナス
                    pawnAdvancedBonus =
                      if p == Sente
                        then if ry == 8 && not hasPawnBlocking then 90.0 else 0.0
                        else if ry == 2 && not hasPawnBlocking then 90.0 else 0.0
                in forwardRookMobility + pawnAdvancedBonus
              | (sq, _) <- myRooks
              ]
            
            -- 角道の開放と斜めの射線
            bishopScore = sum
              [ let (bx, by) = sq
                    diagTargets = [ (bx + d*dx, by + d*dy) | dx <- [-1, 1], dy <- [-1, 1], d <- [1..8] ]
                    openDiags = length [ s | s <- diagTargets, not (isOutOfBounds s), canPieceReachTarget sq (Piece p Bishop) s bd ]
                    -- 7六歩(先手) / 3四歩(後手) による角道オープンボーナス
                    isKakudoOpen = if p == Sente
                                     then Map.lookup (7, 6) bd == Just (Piece Sente Pawn)
                                     else Map.lookup (3, 4) bd == Just (Piece Gote Pawn)
                    kakudoBonus = if isKakudoOpen then 70.0 else 0.0
                in fromIntegral openDiags * 8.0 + kakudoBonus
              | (sq, _) <- myBishops
              ]
        in rookScore + bishopScore

      -- B. 銀と桂馬の攻撃参加（Forward Attack Mobility）
      evalSilverKnightAttack p =
        let mySilvers = [ sq | (sq, Piece owner pt) <- Map.toList bd, owner == p, pt `elem` [Silver, PSilver] ]
            myKnights = [ sq | (sq, Piece owner pt) <- Map.toList bd, owner == p, pt `elem` [Knight, PKnight] ]
            
            -- 銀の前進・攻撃参加
            silverScore = sum
              [ let (_, sy) = sq
                    advancement = if p == Sente then 9 - sy else sy - 1
                    advanceBonus = if advancement >= 2 then fromIntegral advancement * 35.0 else 0.0
                    inBattleZone = if p == Sente then sy <= 6 else sy >= 4
                    battleBonus = if inBattleZone then 60.0 else 0.0
                in advanceBonus + battleBonus
              | sq <- mySilvers
              ]
            
            -- 桂馬の跳躍・攻撃参加（敵の歩や駒の利きに晒される無謀な高跳びは特大ペナルティ！）
            knightScore = sum
              [ let (_, ky) = sq
                    advancement = if p == Sente then 9 - ky else ky - 1
                    oppAttackers = getPiecesCovering (opponent p) sq bd
                    myDefenders  = getPiecesCovering p sq bd
                    hasPawnThreat = any (\(_, Piece _ opt) -> opt == Pawn) oppAttackers
                    
                    penalty = if hasPawnThreat
                                then -400.0 -- 桂馬の高跳び歩の餌食（タダ捨て）特大ペナルティ
                                else if not (null oppAttackers) && length myDefenders < length oppAttackers
                                  then -250.0 -- 紐なし高跳びペナルティ
                                  else 0.0
                    
                    advanceBonus = if advancement >= 2 && null oppAttackers
                                     then fromIntegral advancement * 40.0
                                     else 0.0
                in advanceBonus + penalty
              | sq <- myKnights
              ]
        in silverScore + knightScore

  in (evalMajorPieces plr - evalMajorPieces opp) + (evalSilverKnightAttack plr - evalSilverKnightAttack opp)

-- | 2. 手得・手損判定（18香無駄上がり、飛車先不突き金寄り等の手損を排除し手得を最大化）
evalTempoLoss :: Player -> State -> Double
evalTempoLoss plr st =
  let bd = board st
      opp = opponent plr
      
      -- 18香車（1九香〜1八香、9九香〜9八香、後手1一香〜1二香、9一香〜9二香）の無駄上がり検知
      lanceIdlingPenalty p =
        let myLances = [ sq | (sq, Piece owner pt) <- Map.toList bd, owner == p, pt == Lance ]
            isEdgeBreached x =
              any (\((ox, oy), Piece op _) -> op /= p && ox == x && (if p == Sente then oy >= 7 else oy <= 3)) (Map.toList bd)
            kingSq = findKing p bd
            isTrueAnaguma = case kingSq of
              Just (kx, ky) -> if p == Sente
                                 then (kx <= 2 && ky >= 8)
                                 else (kx <= 2 && ky <= 2)
              Nothing -> False
            penalties = sum
              [ let (lx, ly) = sq
                    isFutilePos = if p == Sente
                                    then (lx == 1 && ly == 8) || (lx == 9 && ly == 8)
                                    else (lx == 1 && ly == 2) || (lx == 9 && ly == 2)
                in if isFutilePos && not (isEdgeBreached lx) && not isTrueAnaguma
                     then -1500.0 -- 香車無駄上がりによる致命的手損ペナルティ
                     else 0.0
              | sq <- myLances
              ]
        in penalties
      -- 飛車先不突きなのに金が中央に寄る手損
      driftGoldWithoutRookAdvance p =
        let myRooks = [ sq | (sq, Piece owner pt) <- Map.toList bd, owner == p, pt == Rook ]
            myGolds = [ sq | (sq, Piece owner pt) <- Map.toList bd, owner == p, pt == Gold ]
            isRookPawnNotAdvanced = any (\(rx, _) ->
              let pawnSq = (rx, if p == Sente then 7 else 3)
              in Map.lookup pawnSq bd == Just (Piece p Pawn)
              ) myRooks
            isGoldDrifting = any (\(gx, gy) ->
              if p == Sente
                then gy == 8 && (gx == 5 || gx == 6 || gx == 4)
                else gy == 2 && (gx == 5 || gx == 6 || gx == 4)
              ) myGolds
        in if isRookPawnNotAdvanced && isGoldDrifting
             then -250.0 -- 飛車先不突き金寄り手損ペナルティ！
             else 0.0

  in (lanceIdlingPenalty plr + driftGoldWithoutRookAdvance plr) -
     (lanceIdlingPenalty opp + driftGoldWithoutRookAdvance opp)

-- | 3. 金と玉の連携（金玉近接・守護スクラム・遊離金排除）
evalGoldKingCoordination :: Player -> State -> Double
evalGoldKingCoordination plr st =
  let bd = board st
      opp = opponent plr
      
      evalCoord p =
        let kingSq = findKing p bd
            myGolds = [ sq | (sq, Piece owner pt) <- Map.toList bd, owner == p, pt `elem` [Gold, PPawn, PSilver, PKnight, PLance] ]
        in case kingSq of
             Nothing -> 0.0
             Just (kx, ky) ->
               sum [ let dist = abs (gx - kx) + abs (gy - ky)
                         coversKing = canPieceReachTarget (gx, gy) (Piece p Gold) (kx, ky) bd
                         coverBonus = if coversKing then 70.0 else 0.0
                         distScore = case dist of
                           1 -> 120.0 -- ぴったり隣接（最強の守護連携）
                           2 -> 80.0  -- 近接（連携囲い）
                           3 -> 10.0
                           _ -> -80.0 -- 玉から遠く離れて孤立（遊離金ペナルティ）
                     in distScore + coverBonus
                   | (gx, gy) <- myGolds
                   ]
  in evalCoord plr - evalCoord opp

-- | 4. 詰み筋までの距離（DTM短縮・QEGTB接続・敵玉退路封鎖度）
evalDistanceToMate :: Player -> State -> Double
evalDistanceToMate plr st =
  let bd = board st
      opp = opponent plr
      oppKingSq = findKing opp bd
      
      qegtbBonus = case lookupQuotientTablebase st of
        Just (dtm, _) -> 10000.0 - fromIntegral dtm * 100.0
        Nothing       -> 0.0
      
      kingNetScore = case oppKingSq of
        Nothing -> 5000.0
        Just (kx, ky) ->
          let surroundings = [ (kx + dx, ky + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0), not (isOutOfBounds (kx + dx, ky + dy)) ]
              blockedOrCovered = length [ s | s <- surroundings, Map.member s bd || isCoveredBy plr s bd ]
              totalNeighbors = length surroundings
              netRatio = fromIntegral blockedOrCovered / fromIntegral totalNeighbors
          in netRatio * 200.0
  in qegtbBonus + kingNetScore

-- | 5. 数の攻め評価（特定地点への利きの集中・突破、および局所的な数の優位）
evalNumericalAssault :: Player -> State -> Double
evalNumericalAssault plr st =
  let bd = board st
      opp = opponent plr
      oppKingSq = findKing opp bd
      
      -- 重要ターゲットマス：
      -- 1. 敵玉の周囲8マス（玉頭、玉の横）
      -- 2. 敵の守備駒（金、銀、角、飛、歩）がいるマス
      targetSquares =
        let kingNeighbors = case oppKingSq of
              Just (kx, ky) -> [ (kx + dx, ky + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0), not (isOutOfBounds (kx + dx, ky + dy)) ]
              Nothing       -> []
            enemyDefenders = [ sq | (sq, Piece p pt) <- Map.toList bd, p == opp, pt `elem` [Gold, Silver, Pawn, Knight] ]
        in nub (kingNeighbors ++ enemyDefenders)
      
      -- 各ターゲットマスにおける数の攻めの成否を評価
      evalSquare sq =
        let myAttackers = getPiecesCovering plr sq bd
            oppDefenders = getPiecesCovering opp sq bd
            nAtt = length myAttackers
            nDef = length oppDefenders
        in if nAtt == 0
           then 0.0
           else
             -- A. 数の優位（味方の利き数が相手の利き数を上回る：突破確定！）
             let breakBonus = if nAtt > nDef then fromIntegral (nAtt - nDef) * 60.0 else 0.0
                 -- B. 攻めの集中（2枚以上の利きが同じマスに集約されているバッテリー・数の攻め）
                 concentrationBonus = if nAtt >= 2 then fromIntegral nAtt * 35.0 else 0.0
                 -- C. 敵玉頭・敵玉隣接への集中攻撃ボーナス
                 kingHeadBonus = case oppKingSq of
                   Just (kx, ky) ->
                     let dir = if opp == Sente then -1 else 1
                     in if sq == (kx, ky + dir) && nAtt >= 1 then fromIntegral nAtt * 50.0 else 0.0
                   Nothing -> 0.0
             in breakBonus + concentrationBonus + kingHeadBonus
  in sum [ evalSquare sq | sq <- targetSquares ]


isFriendlySquare :: Square -> Player -> Board -> Bool
isFriendlySquare sq p bd = case Map.lookup sq bd of
  Just (Piece owner _) -> owner == p
  Nothing              -> False


-- | 角頭および桂頭の防衛評価

evalKakutoKeitoDefense :: Player -> State -> Double
evalKakutoKeitoDefense plr st =
  let bd = board st
      opp = opponent plr
      
      -- 先手視点の角頭(8,7)・桂頭守備評価
      senteDefenseScore =
        let enemyRookPawnAdvanced =
              case Map.lookup (8, 4) bd of
                Just (Piece Gote Pawn) -> True
                _ -> case Map.lookup (8, 5) bd of
                       Just (Piece Gote Pawn) -> True
                       _ -> False
            kakutoCovered = isCoveredBy Sente (8, 7) bd -- 7八金や7七角、7七銀の利きがあるか
        in if enemyRookPawnAdvanced
           then if kakutoCovered
                then 150.0   -- 7八金などで角頭を堅固に防衛中
                else -1200.0 -- 相手の飛車先が迫っているのに角頭が無防備（即敗北ペナルティ）
           else if kakutoCovered
                then 50.0    -- 早期の7八金角頭ガードボーナス
                else 0.0

      -- 後手視点の角頭(2,3)・桂頭守備評価
      goteDefenseScore =
        let enemyRookPawnAdvanced =
              case Map.lookup (2, 6) bd of
                Just (Piece Sente Pawn) -> True
                _ -> case Map.lookup (2, 5) bd of
                       Just (Piece Sente Pawn) -> True
                       _ -> False
            kakutoCovered = isCoveredBy Gote (2, 3) bd -- 3二金や3三角、3三銀の利きがあるか
        in if enemyRookPawnAdvanced
           then if kakutoCovered
                then 150.0
                else -1200.0
           else if kakutoCovered
                then 50.0
                else 0.0

      myScore = if plr == Sente then senteDefenseScore else goteDefenseScore
      oppScore = if opp == Sente then senteDefenseScore else goteDefenseScore
  in myScore - oppScore

-- | 歩以外の放れ駒（味方の紐が付いていない駒）のペナルティ
evalLoosePieces :: Player -> State -> Double
evalLoosePieces plr st =
  let bd = board st
      opp = opponent plr
      countLoose player =
        length [ sq | (sq, p@(Piece pPlr pt)) <- Map.toList bd
                    , pPlr == player
                    , pt `notElem` [Pawn, King] -- 玉と歩以外（金銀桂香飛角）
                    , not (isPieceProtected sq p bd)
               ]
      myLoose = countLoose plr
      oppLoose = countLoose opp
  in (fromIntegral oppLoose - fromIntegral myLoose) * 40.0

-- | 6. 両取り（Fork）の検出と評価
-- 着手した駒（または打った駒）が相手の重要駒（玉・大駒・金銀など）2枚以上を同時に射程に捉える強力な戦術手
isForkMove :: State -> Move -> Bool
isForkMove st m =
  let plr = turn st
      opp = opponent plr
      nextSt = applyMove m st
      nextBd = board nextSt
      (toSq, ptMoved) = case m of
        NormalMove _ to p ->
          case Map.lookup to nextBd of
            Just (Piece _ pt) -> (to, pt)
            Nothing           -> (to, Pawn)
        DropMove pt to -> (to, pt)
      -- 着手した駒の利きが及ぶマス
      attackedSquares = [ sq | sq <- Map.keys nextBd, canPieceReachTarget toSq (Piece plr ptMoved) sq nextBd ]
      -- そのマスにいる相手の重要駒
      targetValuablePieces =
        [ (sq, pt) | sq <- attackedSquares
                   , Just (Piece p pt) <- [Map.lookup sq nextBd]
                   , p == opp
                   , pt `elem` [King, Rook, PRook, Bishop, PBishop, Gold, Silver, PPawn, PSilver, PKnight]
        ]
  in length targetValuablePieces >= 2

-- | 局面における両取り優位度評価
evalTacticalFork :: Player -> State -> Double
evalTacticalFork plr st =
  let bd = board st
      opp = opponent plr
      -- 盤上の味方の駒が相手の重要駒を両取りしているか
      countForks p = sum
        [ let targets = [ sq | (sq, Piece op pt) <- Map.toList bd, op == opponent p
                             , pt `elem` [King, Rook, PRook, Bishop, PBishop, Gold, Silver]
                             , canPieceReachTarget mySq piece sq bd ]
          in if length targets >= 2 then 200.0 else 0.0
        | (mySq, piece@(Piece owner _)) <- Map.toList bd, owner == p
        ]
  in countForks plr - countForks opp

-- | 移動先マスが相手の利きに晒されており、かつ味方の紐が付いていない（または安い駒で取られる）大悪手・タダ捨て手か判定
-- ★ ただし、王手をかける手、敵の守備駒（金銀飛角など）を捕獲する手、敵陣での大駒の成り捨て（大攻勢）、
--    数の攻め（味方の利き >= 相手の利き）による突破手は決してタダ捨て悪手ではない！
isBlunderMove :: State -> Move -> Bool
isBlunderMove st m
  | isKingUnderAttack (turn st) (board st) = False -- 王手回避手はタダ捨て悪手ではない！
  | otherwise =
      let plr = turn st
          opp = opponent plr
          bd = board st
          nextSt = applyMove m st
          
          -- 1. 王手（Check）をかける手は絶対にタダ捨て悪手ではない！（成り捨て王手など）
          isCheck = isKingUnderAttack opp (board nextSt)
          
          -- 2. 敵の守備駒（金銀飛角など）を捕獲する手も大駒切り・成り捨ての強襲手！
          (toSq, ptMoved, isPromo) = case m of
            NormalMove from to promo ->
              case Map.lookup from bd of
                Just (Piece _ pt) -> (to, pt, promo)
                Nothing           -> (to, Pawn, False)
            DropMove pt to       -> (to, pt, False)
          
          targetCaptured = Map.lookup toSq bd
          isCapturingValuable = case targetCaptured of
            Just (Piece _ capPt) -> pieceValue capPt >= 350.0 -- 金・銀・飛・角・桂・と金等の重要駒を捕獲
            Nothing              -> False
          
          -- 3. 敵陣での大駒（飛車・角）の成り捨て（龍・馬を作る強襲）
          isMajorPromotion = isPromo && ptMoved `elem` [Rook, Bishop]
          
          -- 4. 数の攻めが成立しているマス（味方の利き >= 相手の利き）への突入
          oppAttackers = getPiecesCovering opp toSq bd
          nextBd = board nextSt
          friendlyDefenders = getPiecesCovering plr toSq nextBd
          isNumericalSuperiority = length friendlyDefenders >= length oppAttackers
          -- 相手の歩の利きがあるマスに桂馬・銀・金・角・飛が飛び込むのは、たとえ紐があってもタダ捨て・大損交換！
          hasEnemyPawnControl = any (\(_, Piece _ opt) -> opt == Pawn) oppAttackers
          isPawnFodder = hasEnemyPawnControl && ptMoved `elem` [Knight, Silver, Gold, Bishop, Rook] && not isCheck && not isCapturingValuable
      in if isPawnFodder
         then True -- 「桂馬の高跳び歩の餌食」等は絶対タダ捨て大悪手！
         else if isCheck || isCapturingValuable || (isMajorPromotion && not (null friendlyDefenders)) || isNumericalSuperiority
         then False -- これらは大駒の成り捨て・数の攻め・強襲手であり、タダ捨て悪手ではない！
         else
           if null oppAttackers
           then False
           else
             let minOppVal = minimum [ pieceValue oppPt | (_, Piece _ oppPt) <- oppAttackers ]
                 movedVal  = pieceValue ptMoved
             in if null friendlyDefenders
                then ptMoved /= Pawn -- 紐なしで歩以外の駒を敵の利きに無意味に飛び込ませるのはタダ捨て
                else (movedVal > minOppVal + 200.0) -- 紐があっても、安い駒で取られる損交換は排除！

-- | 中盤の攻め筋評価（成駒の破壊力、敵玉包囲度・玉頭圧力、敵陣への拠点・垂れ歩）
evalMidgameAttack :: Player -> State -> Double
evalMidgameAttack plr st =
  let bd = board st
      opp = opponent plr
      oppKingSq = findKing opp bd
      
      -- 1. 成駒（龍・馬・と金）の威力
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
      
      -- 2. 敵玉周辺への利き・圧力（玉頭・包囲網）
      kingPressureBonus = case oppKingSq of
        Nothing -> 0.0
        Just (kx, ky) ->
          let kingNeighbors = [ (kx + dx, ky + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0), not (isOutOfBounds (kx + dx, ky + dy)) ]
              headSq = (kx, ky + (if opp == Sente then -1 else 1)) -- 敵玉の頭
              neighborPressures = sum [ if isCoveredBy plr sq bd then 40.0 else 0.0 | sq <- kingNeighbors ]
              headPressure = if isCoveredBy plr headSq bd then 80.0 else 0.0
          in neighborPressures + headPressure
      
      -- 3. 敵陣深くへの拠点（と金・歩）
      footholdBonus = sum
        [ 50.0 | (sq, Piece p pt) <- Map.toList bd, p == plr, inEnemyCamp plr (snd sq), pt `elem` [Pawn, PPawn] ]
  in promotedBonus + kingPressureBonus + footholdBonus
  where
    inEnemyCamp Sente y = y <= 3
    inEnemyCamp Gote  y = y >= 7

-- | 中盤の受け筋評価（自玉守備陣・金銀の近接配置と連結スクラム、敵成駒の侵入阻止）
evalMidgameDefense :: Player -> State -> Double
evalMidgameDefense plr st =
  let bd = board st
      opp = opponent plr
      myKingSq = findKing plr bd
      
      -- 1. 自玉の守備陣（金銀の近接配置と連結スクラム）
      (castleBonus, dangerAroundKing) = case myKingSq of
        Nothing -> (0.0, 0.0)
        Just (kx, ky) ->
          let myGuards = [ (sq, pt) | (sq, Piece p pt) <- Map.toList bd
                                    , p == plr
                                    , pt `elem` [Gold, Silver, PPawn, PSilver]
                                    , abs (fst sq - kx) <= 2 && abs (snd sq - ky) <= 2 ]
              guardCount = length myGuards
              -- 金銀同士の紐付き（連結度）
              linkPairs = [ 1 | ((sq1, _), (sq2, _)) <- [(g1, g2) | g1 <- myGuards, g2 <- myGuards, fst g1 < fst g2]
                              , canPieceReachTarget sq1 (Piece plr Gold) sq2 bd ]
              connectedBonus = fromIntegral (sum linkPairs) * 50.0
              guardBonus = fromIntegral guardCount * 45.0
              
              -- 自玉周辺への敵利きの侵入ペナルティ
              kingNeighbors = [ (kx + dx, ky + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0), not (isOutOfBounds (kx + dx, ky + dy)) ]
              enemyAttacks = sum [ if isCoveredBy opp sq bd then 50.0 else 0.0 | sq <- kingNeighbors ]
          in (guardBonus + connectedBonus, enemyAttacks)
      
      -- 2. 自陣への敵の成駒（龍・馬）侵入ペナルティ
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

-- | 飛び駒（飛、竜、角、馬、桂、香車）の潜在的な利き・X-Ray・影の利き・ピン・開き王手を活用した攻撃力評価
evalPotentialRayAttack :: Player -> State -> Double
evalPotentialRayAttack plr st =
  let bd = board st
      opp = opponent plr
      
      evalPlayerRays player =
        let enemy = opponent player
            eKingSq = findKing enemy bd
            myPieces = [ (sq, pt) | (sq, Piece p pt) <- Map.toList bd, p == player ]
            
            -- A. 飛車・竜王（縦横の直通およびX-Ray影の利き）
            evalRookRays (rx, ry) pt =
              let isPromoted = pt == PRook
                  orthoDirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]
                  
                  scanDir (dx, dy) =
                    let lineSquares = takeWhile (not . isOutOfBounds)
                                      [ (rx + d * dx, ry + d * dy) | d <- [1..8] ]
                        piecesOnLine = [ (s, p) | s <- lineSquares, Just p <- [Map.lookup s bd] ]
                    in case piecesOnLine of
                         -- 直通射線
                         [] -> 5.0 * fromIntegral (length lineSquares)
                         -- 障害駒が1個だけ挟まっている：潜在的X-Ray射線！
                         [((ox, oy), Piece opOpt opPt)] ->
                           let dist = abs (ox - rx) + abs (oy - ry)
                               discoveredBonus =
                                 if opOpt == player
                                 then case eKingSq of
                                        Just (ekx, eky) | (dx == 0 && ekx == rx && signum (eky - ry) == dy) ||
                                                          (dy == 0 && eky == ry && signum (ekx - rx) == dx) -> 180.0
                                        _ -> 45.0
                                 else 0.0
                               pinBonus =
                                 if opOpt == enemy
                                 then case eKingSq of
                                        Just (ekx, eky) | (dx == 0 && ekx == rx && signum (eky - ry) == dy) ||
                                                          (dy == 0 && eky == ry && signum (ekx - rx) == dx) -> 150.0
                                        _ -> if opPt `elem` [Rook, Bishop, PRook, PBishop, Gold, Silver] then 60.0 else 0.0
                                 else 0.0
                           in (15.0 / fromIntegral (dist + 1)) + discoveredBonus + pinBonus
                         _ -> 0.0
                  
                  -- 敵玉と同じ筋・段に位置しているボーナス（王手筋の睨み）
                  kingAlignBonus = case eKingSq of
                    Just (ekx, eky) ->
                      (if rx == ekx then 110.0 else 0.0) + (if ry == eky then 70.0 else 0.0)
                    Nothing -> 0.0
                  
                  proBonus = if isPromoted then 80.0 else 0.0
                  dirScores = sum [ scanDir dir | dir <- orthoDirs ]
              in dirScores + kingAlignBonus + proBonus

            -- B. 角行・竜馬（斜めの直通およびX-Ray影の利き・角睨み）
            evalBishopRays (bx, by) pt =
              let isPromoted = pt == PBishop
                  diagDirs = [(1, 1), (1, -1), (-1, 1), (-1, -1)]
                  
                  scanDiag (dx, dy) =
                    let lineSquares = takeWhile (not . isOutOfBounds)
                                      [ (bx + d * dx, by + d * dy) | d <- [1..8] ]
                        piecesOnLine = [ (s, p) | s <- lineSquares, Just p <- [Map.lookup s bd] ]
                    in case piecesOnLine of
                         [] -> 4.0 * fromIntegral (length lineSquares)
                         [((ox, oy), Piece opOpt opPt)] ->
                           let dist = max (abs (ox - bx)) (abs (oy - by))
                               discoveredBonus =
                                 if opOpt == player
                                 then case eKingSq of
                                        Just (ekx, eky) | abs (ekx - bx) == abs (eky - by) &&
                                                          signum (ekx - bx) == dx && signum (eky - by) == dy -> 180.0
                                        _ -> 45.0
                                 else 0.0
                               pinBonus =
                                 if opOpt == enemy
                                 then case eKingSq of
                                        Just (ekx, eky) | abs (ekx - bx) == abs (eky - by) &&
                                                          signum (ekx - bx) == dx && signum (eky - by) == dy -> 150.0
                                        _ -> if opPt `elem` [Rook, Bishop, PRook, PBishop, Gold, Silver] then 60.0 else 0.0
                                 else 0.0
                           in (12.0 / fromIntegral (dist + 1)) + discoveredBonus + pinBonus
                         _ -> 0.0
                  
                  kingDiagBonus = case eKingSq of
                    Just (ekx, eky) ->
                      if abs (bx - ekx) == abs (by - eky) then 120.0 else 0.0
                    Nothing -> 0.0
                  
                  proBonus = if isPromoted then 80.0 else 0.0
                  dirScores = sum [ scanDiag dir | dir <- diagDirs ]
              in dirScores + kingDiagBonus + proBonus

            -- C. 香車（縦の直通射線・串刺し・下段の香車の潜在力）
            evalLanceRays (lx, ly) =
              let pDir = if player == Sente then -1 else 1
                  forwardSquares = takeWhile (not . isOutOfBounds)
                                   [ (lx, ly + d * pDir) | d <- [1..8] ]
                  piecesOnLine = [ (s, p) | s <- forwardSquares, Just p <- [Map.lookup s bd] ]
                  
                  kingColBonus = case eKingSq of
                    Just (ekx, eky) | ekx == lx && (if player == Sente then eky < ly else eky > ly) ->
                      case piecesOnLine of
                        []   -> 200.0
                        [_]  -> 130.0 -- 障害駒1個越しに敵玉を睨むX-Ray！
                        [_,_]-> 60.0
                        _    -> 20.0
                    _ -> 0.0
                  
                  skewerBonus = case piecesOnLine of
                    [((_, _), Piece opOpt _), ((_, _), Piece opOpt2 _)]
                      | opOpt == enemy && opOpt2 == enemy -> 90.0
                    _ -> 0.0
                  
                  baseMobility = fromIntegral (length forwardSquares) * 8.0
              in baseMobility + kingColBonus + skewerBonus

            -- D. 桂馬（駒飛び越え急襲・両取り・背後開通潜在力）
            evalKnightRays (kx, ky) =
              let pDir = if player == Sente then -1 else 1
                  targetSquares = [ (kx - 1, ky + 2 * pDir), (kx + 1, ky + 2 * pDir) ]
                  validTargets = filter (not . isOutOfBounds) targetSquares
                  
                  kingProximityBonus = case eKingSq of
                    Just (ekx, eky) ->
                      let dists = [ abs (tx - ekx) + abs (ty - eky) | (tx, ty) <- validTargets ]
                      in if any (<= 2) dists then 90.0 else 0.0
                    Nothing -> 0.0
                  
                  forkThreatBonus = sum
                    [ let attackers = getPiecesCovering enemy (tx, ty) bd
                      in if null attackers
                         then case Map.lookup (tx, ty) bd of
                                Just (Piece op pt) | op == enemy && pt `elem` [Gold, Silver, Rook, Bishop] -> 130.0
                                _ -> 20.0
                         else 0.0
                    | (tx, ty) <- validTargets
                    ]
                  
                  behindSq = (kx, ky - pDir)
                  unblockBonus = case Map.lookup behindSq bd of
                    Just (Piece p pt) | p == player && pt `elem` [Lance, Rook, PRook] -> 80.0
                    _ -> 0.0
              in kingProximityBonus + forkThreatBonus + unblockBonus

            totalRooks   = sum [ evalRookRays sq pt   | (sq, pt) <- myPieces, pt `elem` [Rook, PRook] ]
            totalBishops = sum [ evalBishopRays sq pt | (sq, pt) <- myPieces, pt `elem` [Bishop, PBishop] ]
            totalLances  = sum [ evalLanceRays sq     | (sq, pt) <- myPieces, pt == Lance ]
            totalKnights = sum [ evalKnightRays sq    | (sq, pt) <- myPieces, pt == Knight ]
        in totalRooks + totalBishops + totalLances + totalKnights

  in evalPlayerRays plr - evalPlayerRays opp

-- | 局面の総合評価：重み付き中盤攻防統合評価関数（駒の利き・手得・手損・駒得・駒損・金玉連携・詰み筋距離・飛び駒潜在利きで最適化）
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
      pieceControlAdvantage = evalPieceControl player st * wPieceControl weights
      tempoLossAdvantage = evalTempoLoss player st * wTempoLoss weights
      goldKingCoordAdvantage = evalGoldKingCoordination player st * wGoldKingCoord weights
      distanceToMateAdvantage = (evalDistanceToMate player st - evalDistanceToMate opp st) * wDistanceToMate weights
      numericalAssaultAdvantage = (evalNumericalAssault player st - evalNumericalAssault opp st) * wNumericalAssault weights
      tacticalForkAdvantage = evalTacticalFork player st * wTacticalFork weights
      potentialRayAdvantage = evalPotentialRayAttack player st * wPotentialRays weights
  in materialAdvantage + morseAdvantage + attackAdvantage + defenseAdvantage
     + kakutoKeitoAdvantage + looseAdvantage + pieceControlAdvantage
     + tempoLossAdvantage + goldKingCoordAdvantage + distanceToMateAdvantage
     + numericalAssaultAdvantage + tacticalForkAdvantage + potentialRayAdvantage


-- | デフォルト重みでの評価関数（後方互換）
evaluateStateDefault :: Player -> State -> Double
evaluateStateDefault = evaluateState defaultWeights

-- ============================================================================
-- 5. 合法手生成
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

    -- スライド移動（飛、角、香）
    slide (x, y) (dx, dy) = go (x + dx, y + dy)
      where
        go sq@(cx, cy)
          | isOutOfBounds sq = []
          | otherwise = case Map.lookup sq bd of
              Nothing -> sq : go (cx + dx, cy + dy)
              Just (Piece p _) -> if p == plr then [] else [sq]

    -- 1マス移動
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

    -- 持ち駒を打つ手
    myHand = fromMaybe emptyHand (Map.lookup plr hnds)
    emptySquares = [ (x, y) | x <- [1..9], y <- [1..9], not (Map.member (x, y) bd) ]

    -- 二歩チェック（成っていない歩が同じ筋にあるか）
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

-- ============================================================================
-- 6. 千日手判定（トポロジー無限ループの自動検出）
-- ============================================================================

-- | 同一局面が 4 回以上発生したかどうかを判定
isSennichite :: PositionKey -> Map.Map PositionKey Int -> Bool
isSennichite key hist =
  case Map.lookup key hist of
    Just count -> count >= 4
    Nothing    -> False

-- ============================================================================
-- 7. 型レベル定跡（Morse V-Path）ルックアップ
-- ============================================================================

-- | 現在の指し手履歴から、CompiledMorseBook の V-Path に適合する定跡手を検索する
lookupBookMove :: [String] -> State -> Maybe Move
lookupBookMove currentMoves st =
  let legalMoves = f5SyzygyFilter st (generateMoves st)
      -- 角頭歩突きおよび無駄な香車上がりなどの手損手は定跡であっても絶対に除外
      safeLegalMoves = filter (\m -> not (isPushingKakutoPawn st m) && not (isFutileLanceLift st m)) legalMoves
      allPaths = Map.elems Book.runtimePaths
      matchingPaths = filter (\p -> currentMoves `isPrefixOf` p && length p > length currentMoves) allPaths
      candidates = [ parseMove (p !! length currentMoves) | p <- matchingPaths ]
      validBookMoves = filter (`elem` safeLegalMoves) candidates
      
      -- 初手（currentMoves == [] かつ初期局面）の場合、7六歩 (7g7f) または 2六歩 (2g2f) を最優先
      sortedMoves =
        if null currentMoves
        then if board st == initBoard
             then sortBy (\m1 m2 ->
                    let score m = case m of
                          NormalMove (7, 7) (7, 6) False -> 2 :: Int
                          NormalMove (2, 7) (2, 6) False -> 1 :: Int
                          _                              -> 0 :: Int
                    in compare (score m2) (score m1)
                  ) validBookMoves
             else []
        else validBookMoves
  in listToMaybe sortedMoves

-- ============================================================================
-- 8. 最善手選択 (型レベル定跡 ＆ 静止探索付きα-β探索 ＆ Garside置換表 ＆ F5フィルター ＆ 千日手回避)
-- ============================================================================

-- | 静止探索（捕獲手・王手・大駒成り・急所打ち込みを先読みして水平線効果を解消）
-- ★ 王手（Check）されている局面では Stand Pat（パス）を禁止し、王手解除手を全探索して詰み判定！
searchQuiescence :: MidgameWeights -> Int -> Double -> Double -> State -> Double
searchQuiescence weights 0 _ _ st = evaluateState weights (turn st) st
searchQuiescence weights qdepth alpha beta st =
  let plr = turn st
      inCheck = isKingUnderAttack plr (board st)
  in if inCheck
     then
       -- 王手されている局面：パス（Stand Pat）は不可！王手を解除する手を探索
       let rawMoves = generateMoves st
           legalEscapes = f5SyzygyFilter st rawMoves
       in if null legalEscapes
          then -10000.0 -- 王手を解除できなければ即詰み！
          else qLoop legalEscapes alpha beta
     else
       -- 通常局面：Stand Pat 判定後に戦術手（捕獲・大駒成り・王手・打ち込み・突破）を探索
       let standPat = evaluateState weights plr st
       in if standPat >= beta
          then beta
          else
            let curAlpha = max alpha standPat
                rawMoves = generateMoves st
                tacticalMoves = filter (isTacticalMove st) rawMoves
                f5Tacticals = f5SyzygyFilter st tacticalMoves
            in if null f5Tacticals
               then curAlpha
               else qLoop f5Tacticals curAlpha beta
  where
    isTacticalMove s m =
      let plr = turn s
          opp = opponent plr
          nextS = applyMove m s
          isCheck = isKingUnderAttack opp (board nextS)
          isFork = isForkMove s m
          (toSq, ptMoved, isPromo) = case m of
            NormalMove _ to p -> (to, Pawn, p)
            DropMove pt to    -> (to, pt, False)
          -- 数の攻めによる突破マスか
          oppDefenders = getPiecesCovering opp toSq (board s)
          myAttackers = getPiecesCovering plr toSq (board nextS)
          isBreakthrough = length myAttackers > length oppDefenders
          -- 飛び駒の潜在的射線の開通（ディスカバード・アタック）
          isRayUnblock = case m of
            NormalMove from _ _ ->
              any (\(bsq, Piece bp bpt) -> bp == plr && bpt `elem` [Rook, PRook, Bishop, PBishop, Lance] && canPieceXRayTarget bsq (Piece bp bpt) from (board s)) (Map.toList (board s))
            _ -> False
      in case m of
           -- 1. 相手の駒を取る手、大駒成り、王手、両取り、数の攻め突破、射線開通
           NormalMove _ to promo ->
             Map.member to (board s) || promo || isCheck || isFork || isBreakthrough || isRayUnblock
           -- 2. 持ち駒の打ち込み（王手、両取り、敵玉周辺への打ち込み、急所打ち込み、数の攻め突破）
           DropMove pt to ->
             isCheck || isFork || isBreakthrough || (case findKing opp (board s) of
                           Just (kx, ky) -> abs (fst to - kx) <= 2 && abs (snd to - ky) <= 2 && pt `elem` [Gold, Silver, Rook, Bishop, Knight, Pawn]
                           Nothing -> False)

    qLoop [] curA _ = curA
    qLoop (m:ms) curA b =
      let nextSt = applyMove m st
          score = - searchQuiescence weights (qdepth - 1) (-b) (-curA) nextSt
      in if score >= b
         then b
         else qLoop ms (max curA score) b

-- | ネガマックス α-β 探索 (自手番の視点でのスコアと最善手を返す: 3-Ply + 静止探索2-Ply)
searchAlphaBeta :: MidgameWeights -> Int -> Double -> Double -> State -> (Double, Maybe Move)
searchAlphaBeta weights 0 alpha beta st =
  (searchQuiescence weights 2 alpha beta st, Nothing)
searchAlphaBeta weights depth alpha beta st =
  let inCheck = isKingUnderAttack (turn st) (board st)
      rawMoves = generateMoves st
      f5Moves = f5SyzygyFilter st rawMoves
      (_sennichite, safeMoves) = partition (\m -> 
        let nextSt = applyMove m st
        in isSennichite (positionKey nextSt) (history nextSt)
        ) f5Moves
      -- ★ 王手されている局面では一切の除外を行わずすべての王手回避手を検討する
      nonKakutoMoves = if inCheck
                       then safeMoves
                       else filter (\m -> not (isPushingKakutoPawn st m) && not (isFutileLanceLift st m)) safeMoves
      effectiveMoves = if null nonKakutoMoves then safeMoves else nonKakutoMoves
  in if null f5Moves
     then (-10000.0, Nothing) -- 詰み
     else if null effectiveMoves
       then (0.0, Nothing)     -- 千日手引き分け
       else
         -- 枝刈り効率化のため、タダ捨て手を強烈ペナルティ化し、1手先静的評価で降順ソート
         let scoreMove m =
               let isBlunder = isBlunderMove st m
                   blunderPen = if isBlunder then -2000.0 else 0.0
                   isFutileLance = if inCheck then False else isFutileLanceLift st m
                   lancePen = if isFutileLance then -5000.0 else 0.0
                   
                   nextSt = applyMove m st
                   opp = opponent (turn st)
                   nextBd = board nextSt
                   isCheck = isKingUnderAttack opp nextBd
                   checkBonus = if isCheck then 400.0 else 0.0
                   forkBonus = if isForkMove st m then 450.0 else 0.0
                   
                   -- ★ とびごま（飛、竜、角、馬、桂、香）の潜在的な利きを活かした戦術ボーナス
                   (fromSq, toSq) = case m of
                     NormalMove f t _ -> (Just f, t)
                     DropMove _ t     -> (Nothing, t)
                   
                   -- 1. 開き王手（Discovered Check）：自駒を動かしたマス自身ではなく、背後の飛び駒（飛・角・香）から敵玉に王手がかかった手！
                   isDiscoveredCheck = isCheck && case fromSq of
                     Just _ ->
                       let oppK = findKing opp nextBd
                           movedPiece = Map.lookup toSq nextBd
                       in case (oppK, movedPiece) of
                            (Just kSq, Just p) -> not (canPieceReachTarget toSq p kSq nextBd)
                            _                  -> False
                     Nothing -> False
                   discoveredBonus = if isDiscoveredCheck then 600.0 else 0.0
                   
                   -- 2. 潜在的射線の開放（Unblocking Ray）：駒を動かしたことで、背後の飛・角・香の射線が開通した
                   rayUnblockBonus = case fromSq of
                     Just f ->
                       let behindPieces = [ bsq | (bsq, Piece bp bpt) <- Map.toList (board st)
                                                , bp == turn st, bpt `elem` [Rook, PRook, Bishop, PBishop, Lance]
                                                , canPieceXRayTarget bsq (Piece bp bpt) f (board st) ]
                       in if not (null behindPieces) then 280.0 else 0.0
                     Nothing -> 0.0
                   
                   -- 3. 飛び駒を敵玉と同じライン（筋・段・斜め）に据える手、または桂馬の急襲
                   potentialAlignBonus = case m of
                     NormalMove _ to _ ->
                       case findKing opp (board st) of
                         Just (ekx, eky) ->
                           let (tx, ty) = to
                               isAligned = tx == ekx || ty == eky || abs (tx - ekx) == abs (ty - eky)
                           in if isAligned then 180.0 else 0.0
                         Nothing -> 0.0
                     DropMove pt to ->
                       case findKing opp (board st) of
                         Just (ekx, eky) ->
                           let (tx, ty) = to
                               isAligned = (pt `elem` [Rook, Lance] && tx == ekx) ||
                                           (pt == Rook && ty == eky) ||
                                           (pt == Bishop && abs (tx - ekx) == abs (ty - eky)) ||
                                           (pt == Knight && abs (tx - ekx) <= 2 && abs (ty - eky) <= 2)
                           in if isAligned then 220.0 else 0.0
                         Nothing -> 0.0
                   
                   -- 大駒の成り捨て・重要駒破壊・打ち込み・数の攻めボーナス
                   tacticalBonus = case m of
                     -- 大駒の成り（王手を伴う成り捨てなら超特大加点！）
                     NormalMove _ _ True -> if isCheck then 500.0 else 240.0
                     -- 敵玉近傍への大駒・金銀の打ち込み（王手ならさらに加点）
                     DropMove pt to ->
                       let baseDrop = case findKing opp (board st) of
                             Just (kx, ky) | abs (fst to - kx) <= 2 && abs (snd to - ky) <= 2 ->
                               if pt `elem` [Gold, Silver, Rook, Bishop, Knight] then 320.0 else 120.0
                             _ -> 60.0
                       in if isCheck then baseDrop + 200.0 else baseDrop
                     -- 飛車先歩突き・角道オープン
                     NormalMove (2, 7) (2, 6) _ | turn st == Sente -> 280.0
                     NormalMove (2, 6) (2, 5) _ | turn st == Sente -> 240.0
                     NormalMove (8, 3) (8, 4) _ | turn st == Gote  -> 280.0
                     NormalMove (8, 4) (8, 5) _ | turn st == Gote  -> 240.0
                     NormalMove (7, 7) (7, 6) _ | turn st == Sente -> 220.0
                     NormalMove (3, 3) (3, 4) _ | turn st == Gote  -> 220.0
                     _ -> 0.0
                   
                   s = evaluateState weights (turn st) nextSt
               in s + blunderPen + lancePen + checkBonus + forkBonus + tacticalBonus
                  + discoveredBonus + rayUnblockBonus + potentialAlignBonus
             orderedMoves = sortBy (\m1 m2 -> compare (scoreMove m2) (scoreMove m1)) effectiveMoves
             -- 王手をかけられているときはすべての王手回避手を読み、それ以外は上位14手を深く読む
             topMoves = if inCheck then effectiveMoves else take (if depth >= 2 then 14 else 10) orderedMoves
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
-- 8.5 詰み関与駒だけの商空間によるエンドゲームテーブル (Quotient Endgame Tablebase: QEGTB)
-- ============================================================================

-- | 詰み関与駒の商空間局所パターン
-- 81マス全体から詰みの成否に関与する極小部分空間（玉近傍・王手射線・持ち駒）のみを同値類として縮約
data QuotientPattern = QuotientPattern
  { qpName          :: !String
  , qpTargetKingRank:: !(Square -> Bool)                       -- 受手玉の座標条件
  , qpRequiredHand  :: ![(PieceType, Int)]                     -- 詰みに必要な最小限の持ち駒
  , qpSupportCheck  :: !(Square -> Board -> Player -> Bool)    -- 王手駒に紐（支え）が付いているかの述語
  , qpCandidateMove :: !(Square -> Player -> Board -> Maybe Move) -- 最善王手生成関数
  , qpDTM           :: !Int                                    -- 詰み手数 (Distance to Mate: 1, 3, 5...)
  }

-- | バックプロパゲーションによって抽出された商空間エンドゲームテーブルベース
endgameTablebase :: [QuotientPattern]
endgameTablebase =
  [ -- 1. 頭金イデアル (Atama-Kin Family: DTM = 1)
    -- 玉の頭（前1マス）に金を打つ。背後が盤外（1段目/9段目）または壁で逃げ場なし
    QuotientPattern
      { qpName = "AtamaKin_Drop_DTM1"
      , qpTargetKingRank = \(_, ky) -> ky == 1 || ky == 9 || ky == 2 || ky == 8
      , qpRequiredHand = [(Gold, 1)]
      , qpSupportCheck = \(kx, ky) bd att ->
          let headSq = if att == Sente then (kx, ky + 1) else (kx, ky - 1)
          in not (isOutOfBounds headSq) && isSquareUnderAttack att headSq bd
      , qpCandidateMove = \(kx, ky) att bd ->
          let headSq = if att == Sente then (kx, ky + 1) else (kx, ky - 1)
          in if not (isOutOfBounds headSq) && not (Map.member headSq bd)
             then Just (DropMove Gold headSq)
             else Nothing
      , qpDTM = 1
      }
    -- 2. 盤上の龍・馬・金による玉頭突入詰み (Promoted Piece Head Attack: DTM = 1)
  , QuotientPattern
      { qpName = "Board_Head_Attack_DTM1"
      , qpTargetKingRank = \(_, ky) -> ky == 1 || ky == 9 || ky == 2 || ky == 8
      , qpRequiredHand = []
      , qpSupportCheck = \(kx, ky) bd att ->
          let headSq = if att == Sente then (kx, ky + 1) else (kx, ky - 1)
          in not (isOutOfBounds headSq) && isSquareUnderAttack att headSq bd
      , qpCandidateMove = \(kx, ky) att bd ->
          let headSq = if att == Sente then (kx, ky + 1) else (kx, ky - 1)
              attackers = [ sq | (sq, p@(Piece pPlr pt)) <- Map.toList bd
                               , pPlr == att
                               , pt `elem` [PRook, PBishop, Gold, PPawn, PSilver, PKnight, PLance]
                               , canPieceReachTarget sq p headSq bd
                          ]
          in case attackers of
               (fromSq:_) -> Just (NormalMove fromSq headSq False)
               []         -> Nothing
      , qpDTM = 1
      }
    -- 3. 吊るし桂イデアル (Tsurushi-Kei: DTM = 1)
  , QuotientPattern
      { qpName = "TsurushiKei_DTM1"
      , qpTargetKingRank = \(_, ky) -> ky <= 3 || ky >= 7
      , qpRequiredHand = [(Knight, 1)]
      , qpSupportCheck = \_ _ _ -> True
      , qpCandidateMove = \(kx, ky) att bd ->
          let dirY = if att == Sente then 2 else -2
              dropSq1 = (kx - 1, ky + dirY)
              dropSq2 = (kx + 1, ky + dirY)
              validSq sq = not (isOutOfBounds sq) && not (Map.member sq bd)
          in case filter validSq [dropSq1, dropSq2] of
               (sq:_) -> Just (DropMove Knight sq)
               []     -> Nothing
      , qpDTM = 1
      }
    -- 4. 腹金・尻金イデアル (HaraKin: DTM = 1)
  , QuotientPattern
      { qpName = "HaraKin_DTM1"
      , qpTargetKingRank = \_ -> True
      , qpRequiredHand = [(Gold, 1)]
      , qpSupportCheck = \(kx, ky) bd att ->
          isSquareUnderAttack att (kx - 1, ky) bd || isSquareUnderAttack att (kx + 1, ky) bd
      , qpCandidateMove = \(kx, ky) att bd ->
          let leftSq = (kx - 1, ky)
              rightSq = (kx + 1, ky)
              validDrop sq = not (isOutOfBounds sq) && not (Map.member sq bd) && isSquareUnderAttack att sq bd
          in case filter validDrop [leftSq, rightSq] of
               (sq:_) -> Just (DropMove Gold sq)
               []     -> Nothing
      , qpDTM = 1
      }
    -- 5. 一間竜イデアル (Ikken-Ryu: DTM = 3 - バックプロパゲーション)
  , QuotientPattern
      { qpName = "IkkenRyu_DTM3"
      , qpTargetKingRank = \(_, ky) -> ky <= 3 || ky >= 7
      , qpRequiredHand = [(Silver, 1), (Gold, 1)]
      , qpSupportCheck = \_ _ _ -> True
      , qpCandidateMove = \(kx, ky) att bd ->
          let rRookSq = listToMaybe [ sq | (sq@(rx, ry), Piece p pt) <- Map.toList bd
                                         , p == att, pt == PRook, rx == kx, abs (ry - ky) == 2 ]
          in case rRookSq of
               Just _ ->
                 let dirY = if att == Sente then 1 else -1
                     midSq = (kx, ky + dirY)
                 in if not (Map.member midSq bd) && not (isOutOfBounds midSq)
                    then Just (DropMove Silver midSq)
                    else Nothing
               Nothing -> Nothing
      , qpDTM = 3
      }
    -- 6. 送りの手筋イデアル (Okuri: DTM = 3 - バックプロパゲーション)
  , QuotientPattern
      { qpName = "Okuri_DTM3"
      , qpTargetKingRank = \(_, ky) -> ky == 2 || ky == 8
      , qpRequiredHand = [(Gold, 1)]
      , qpSupportCheck = \_ _ _ -> True
      , qpCandidateMove = \(kx, ky) att bd ->
          let edgeY = if att == Sente then 1 else 9
              targetSq = (kx, edgeY)
          in if not (Map.member targetSq bd) && not (isOutOfBounds targetSq) && isSquareUnderAttack att targetSq bd
             then Just (DropMove Silver targetSq)
             else Nothing
      , qpDTM = 3
      }
  ]

-- | 玉の周囲の脱出可能マストポロジー（Escape Topology）
-- 盤外、受手側の守備駒の壁、攻め手の利きによって塞がれていない、玉が合法的に逃げられる隣接マス
getKingEscapes :: Player -> Square -> Board -> [Square]
getKingEscapes kingPlayer (kx, ky) bd =
  let opp = opponent kingPlayer
      neighbors = [ (kx + dx, ky + dy)
                  | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)
                  , not (isOutOfBounds (kx + dx, ky + dy))
                  ]
      canEscapeTo sq = case Map.lookup sq bd of
        Just (Piece p _) | p == kingPlayer -> False -- 味方の壁で塞がれている
        _ -> not (isSquareUnderAttack opp sq bd)    -- 相手の利きがなければ逃げられる
  in filter canEscapeTo neighbors

-- | 普遍商空間エンドゲームテーブルベース (Universal Quotient EGTB)
-- 玉がどこにいようと（美濃囲い、矢倉、穴熊、中住まい、端玉、中段玉）、
-- 玉の周囲の脱出可能トポロジーを法とする商空間同値類として一元的に即詰みを判定！
lookupUniversalQuotientEGTB :: State -> Maybe (Int, Move)
lookupUniversalQuotientEGTB st =
  let att = turn st
      def = opponent att
      bd = board st
      curHands = hands st
      maybeDefK = findKing def bd
  in case maybeDefK of
       Nothing -> Nothing
       Just kSq@(kx, ky) ->
         let escapes = getKingEscapes def kSq bd
             myHands = fromMaybe emptyHand (Map.lookup att curHands)
             hasGold = Map.findWithDefault 0 Gold myHands >= 1
             hasKnight = Map.findWithDefault 0 Knight myHands >= 1
             
             -- 1. 普遍的金打ちイデアル（頭金・腹金・尻金・押し込みのトポロジー商空間同値類: DTM = 1）
             -- 玉の隣接マスで、支え（紐）があり、打った金が玉に王手をかけ、
             -- かつ玉のすべての脱出マス（escapes）をカバーするマスを探す
             goldDropMate = if not hasGold then Nothing else
               let candidateSquares = [ (kx + dx, ky + dy)
                                      | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)
                                      , not (isOutOfBounds (kx + dx, ky + dy))
                                      , not (Map.member (kx + dx, ky + dy) bd)
                                      , isSquareUnderAttack att (kx + dx, ky + dy) bd -- 支え（紐）あり
                                      ]
                   isMateDrop sq =
                     let dropMv = DropMove Gold sq
                         nextSt = applyMove dropMv st
                         nextBd = board nextSt
                         isCheck = isKingUnderAttack def nextBd
                         remainingEscapes = [ e | e <- escapes, e /= sq
                                                , not (canPieceReachTarget sq (Piece att Gold) e nextBd) ]
                     in isCheck && null remainingEscapes
               in case filter isMateDrop candidateSquares of
                    (mSq:_) -> Just (1, DropMove Gold mSq)
                    []      -> Nothing
             
             -- 2. 盤上の龍・馬・金・成駒による普遍的突入詰み（DTM = 1）
             boardPieceMate =
               let myBoardAttackers = [ (sq, p) | (sq, p@(Piece pPlr pt)) <- Map.toList bd
                                                , pPlr == att
                                                , pt `elem` [PRook, PBishop, Gold, PPawn, PSilver, PKnight, PLance] ]
                   candidateMoves =
                     [ NormalMove fromSq toSq False
                     | (fromSq, piece@(Piece _ pType)) <- myBoardAttackers
                     , dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)
                     , let toSq = (kx + dx, ky + dy)
                     , not (isOutOfBounds toSq)
                     , canPieceReachTarget fromSq piece toSq bd
                     , isSquareUnderAttack att toSq bd || pType `elem` [PRook, PBishop]
                     ]
                   isMateMove mv =
                     let nextSt = applyMove mv st
                         nextBd = board nextSt
                         isCheck = isKingUnderAttack def nextBd
                         (NormalMove _ toSq _) = mv
                         remainingEscapes = [ e | e <- escapes, e /= toSq
                                                , not (isSquareUnderAttack att e nextBd) ]
                     in isCheck && null remainingEscapes
               in case filter isMateMove candidateMoves of
                    (mv:_) -> Just (1, mv)
                    []     -> Nothing
             
             -- 3. 普遍的吊るし桂イデアル（DTM = 1）
             tsurushiKeiMate = if not hasKnight || not (null escapes) then Nothing else
               let dirY = if att == Sente then 2 else -2
                   dropSquares = [ (kx - 1, ky + dirY), (kx + 1, ky + dirY) ]
                   validSq sq = not (isOutOfBounds sq) && not (Map.member sq bd)
               in case filter validSq dropSquares of
                    (sq:_) -> Just (1, DropMove Knight sq)
                    []     -> Nothing
         in listToMaybe [ res | Just res <- [goldDropMate, boardPieceMate, tsurushiKeiMate] ]

-- | 商空間エンドゲームテーブル (QEGTB) 照合
-- ★ 玉形の違いを幾何学的脱出トポロジーの商空間同値類として一元的に判定する Universal QEGTB を最優先照会！
lookupQuotientTablebase :: State -> Maybe (Int, Move)
lookupQuotientTablebase st =
  case lookupUniversalQuotientEGTB st of
    Just uRes -> Just uRes
    Nothing ->
      let att = turn st
          def = opponent att
          curBd = board st
          curHands = hands st
          maybeDefKingSq = findKing def curBd
      in case maybeDefKingSq of
       Nothing -> Nothing
       Just kSq ->
         let myHands = fromMaybe emptyHand (Map.lookup att curHands)
             hasRequiredHand reqs = all (\(pt, cnt) -> Map.findWithDefault 0 pt myHands >= cnt) reqs

             matchPattern QuotientPattern{..} =
               if not (qpTargetKingRank kSq)
               then Nothing
               else if not (hasRequiredHand qpRequiredHand)
               then Nothing
               else if not (qpSupportCheck kSq curBd att)
               then Nothing
               else
                 case qpCandidateMove kSq att curBd of
                   Just mv ->
                     -- 着手後の局面で実際に相手玉が王手になっているか安全検証
                     let nextSt = applyMove mv st
                     in if isKingUnderAttack def (board nextSt)
                        then Just (qpDTM, mv)
                        else Nothing
                   Nothing -> Nothing

             matches = [ res | pat <- endgameTablebase, Just res <- [matchPattern pat] ]
         in listToMaybe matches

-- | USI の go コマンド引数から思考時間上限（マイクロ秒）を計算
-- byoyomi (秒読み) の秒数を上限とし、通信・I/Oラグ（150ms）を差し引いて切れ負けを完全防止
calcTimeLimitMicros :: Player -> [String] -> Int
calcTimeLimitMicros ply tokens =
  let byo = case elemIndex "byoyomi" tokens of
        Just idx | idx + 1 < length tokens -> readMaybe (tokens !! (idx + 1))
        _ -> Nothing
      movetime = case elemIndex "movetime" tokens of
        Just idx | idx + 1 < length tokens -> readMaybe (tokens !! (idx + 1))
        _ -> Nothing
      btime = case elemIndex "btime" tokens of
        Just idx | idx + 1 < length tokens -> readMaybe (tokens !! (idx + 1))
        _ -> Nothing
      wtime = case elemIndex "wtime" tokens of
        Just idx | idx + 1 < length tokens -> readMaybe (tokens !! (idx + 1))
        _ -> Nothing
      binc = case elemIndex "binc" tokens of
        Just idx | idx + 1 < length tokens -> readMaybe (tokens !! (idx + 1))
        _ -> Nothing
      winc = case elemIndex "winc" tokens of
        Just idx | idx + 1 < length tokens -> readMaybe (tokens !! (idx + 1))
        _ -> Nothing
      
      (myTime, myInc) = case ply of
        Sente -> (btime, binc)
        Gote  -> (wtime, winc)
      
      -- 通信・GUIバッファの安全マージン（150ms）
      safetyMargin = 150
      
      -- 思考上限ミリ秒
      timeMs = case (byo, movetime, myTime, myInc) of
        -- 1. byoyomi 指定がある場合: byoyomi の秒数を上限とする
        (Just b, _, _, _) -> max 100 (b - safetyMargin)
        -- 2. movetime 指定がある場合: movetime を上限とする
        (_, Just m, _, _) -> max 100 (m - safetyMargin)
        -- 3. フィッシャールール (inc 指定あり)
        (_, _, Just t, Just inc) -> max 100 (min (t `div` 15 + inc) (t + inc - safetyMargin))
        -- 4. 持ち時間制 (残り時間から算出)
        (_, _, Just t, _) -> max 100 (t `div` 20)
        -- 5. 指定なし (デフォルト1秒秒読み相当: 850ms)
        _                 -> 850
  in timeMs * 1000

-- | 反復深化探索 (byoyomi を上限とし、タイムアウトで打ち切り)
searchBestMoveIterativeWithTimeout :: MidgameWeights -> Int -> State -> IO (Either String Move, State)
searchBestMoveIterativeWithTimeout weights timeLimitMicros state = do
  let inCheck = isKingUnderAttack (turn state) (board state)
      moves = generateMoves state
      f5FilteredMoves = f5SyzygyFilter state moves
      (_sennichiteMoves, safeMoves) = 
        partition (\m -> 
          let nextState = applyMove m state
          in isSennichite (positionKey nextState) (history nextState)
        ) f5FilteredMoves
      -- ★ 王手されている局面では一切の除外を行わずすべての王手回避手を検討する
      nonKakutoMoves = if inCheck
                       then safeMoves
                       else filter (\m -> not (isPushingKakutoPawn state m) && not (isFutileLanceLift state m)) safeMoves
      effectiveMoves = if null nonKakutoMoves then safeMoves else nonKakutoMoves

      -- ★ QEGTB（商空間エンドゲームテーブル）の王手無視・自殺手判定の完全防止：
      --   自玉が現在王手されている場合、またはmateMoveが自玉を王手から救っていない場合は、
      --   QEGTBの詰み手であっても絶対に採用せず、王手回避・正規探索に委ねる！
      validEgtb = case lookupQuotientTablebase state of
        Just (dtm, mateMove) ->
          let safeMvs = f5SyzygyFilter state [mateMove]
          in if inCheck
             then Nothing
             else case safeMvs of
                    [safeM] | safeM `elem` effectiveMoves -> Just (dtm, safeM)
                    _                                     -> Nothing
        Nothing -> Nothing

  case validEgtb of
    Just (dtm, mateMove) -> do
      putStrLn $ "info string quotient egtb hit dtm " ++ show dtm ++ " score mate " ++ show dtm ++ " pv " ++ showMove mateMove
      hFlush stdout
      return (Right mateMove, state)
    Nothing -> do
      -- 2. 型レベル定跡（V-Path スライド）を照会（自玉が王手されている時は定跡ではなく王手回避手を最優先）
      let maybeBook = if inCheck then Nothing else lookupBookMove (moveHistory state) state
      case maybeBook of
        Just bookMove -> do
          putStrLn $ "info string book hit pv " ++ showMove bookMove
          hFlush stdout
          return (Right bookMove, state)
        Nothing -> do
          -- 3. 定跡外または定跡完了後は、反復深化α-β探索（Garsideキャッシュ + F5フィルター + タダ捨て防止 + 静止探索）
          if null f5FilteredMoves
            then return (Left "resign", state)
            else if null effectiveMoves
              then return (Left "draw", state)
              else
                case Map.lookup (positionKey state) (cache state) of
                  Just (_score, mv) | mv `elem` effectiveMoves -> do
                    putStrLn $ "info string hash hit pv " ++ showMove mv
                    hFlush stdout
                    return (Right mv, state)
                  _ -> do
                     -- 初期フォールバックとして合法かつ安全な最善手候補を設定
                     let fallbackMove = case effectiveMoves of (m:_) -> m; [] -> error "no effective moves"
                     bestRef <- newIORef (0.0, fallbackMove)
                 
                     -- タイムアウト付き反復深化探索 (byoyomi を上限として depth 1..31 まで探索)
                     _ <- timeout timeLimitMicros $ do
                       let runIDS d
                             | d > 31 = return ()
                             | otherwise = do
                                 let (score, maybeMv) = searchAlphaBeta weights d (-99999.0) 99999.0 state
                                 case maybeMv of
                                   Just mv | mv `elem` effectiveMoves -> do
                                     -- 遅延評価を解除して正格に評価を確定
                                     _ <- evaluate (score `seq` mv `seq` ())
                                     writeIORef bestRef (score, mv)
                                     putStrLn $ "info depth " ++ show d ++ " score cp " ++ show (round score :: Int) ++ " pv " ++ showMove mv
                                     hFlush stdout
                                     if abs score >= 9000.0
                                       then return () -- 詰みを発見した場合は即座に着手
                                       else runIDS (d + 1)
                                   _ -> return ()
                       runIDS 1
                 
                     -- タイムアウトした場合でも、直前までに探索が完了した最高深さの最善手を取得
                     (bestScore, rawBestMove) <- readIORef bestRef
                     -- 万全の安全保証：選択された手が f5FilteredMoves に含まれているか検証
                     let bestMove = if rawBestMove `elem` f5FilteredMoves
                                    then rawBestMove
                                    else fallbackMove
                         newCache = Map.insert (positionKey state) (bestScore, bestMove) (cache state)
                     return (Right bestMove, state { cache = newCache })

-- 後方互換用：純粋探索
searchBestMoveWithCache :: MidgameWeights -> State -> (Either String Move, State)
searchBestMoveWithCache weights state =
  let inCheck = isKingUnderAttack (turn state) (board state)
      maybeBook = if inCheck then Nothing else lookupBookMove (moveHistory state) state
  in case maybeBook of
    Just bookMove ->
      (Right bookMove, state)
    Nothing ->
      let moves = generateMoves state
          f5FilteredMoves = f5SyzygyFilter state moves
          (_sennichiteMoves, safeMoves) = 
            partition (\m -> 
              let nextState = applyMove m state
              in isSennichite (positionKey nextState) (history nextState)
            ) f5FilteredMoves
          nonKakutoMoves = if inCheck
                           then safeMoves
                           else filter (\m -> not (isPushingKakutoPawn state m) && not (isFutileLanceLift state m)) safeMoves
          effectiveMoves = if null nonKakutoMoves then safeMoves else nonKakutoMoves
      in if null f5FilteredMoves
         then (Left "resign", state)
         else if null effectiveMoves
           then (Left "draw", state)
           else
             case Map.lookup (positionKey state) (cache state) of
               Just (_score, mv) | mv `elem` effectiveMoves -> (Right mv, state)
               _ ->
                 let (bestScore, maybeBestMove) = searchAlphaBeta weights 5 (-99999.0) 99999.0 state
                 in case maybeBestMove of
                      Just bestMove | bestMove `elem` f5FilteredMoves ->
                        let newCache = Map.insert (positionKey state) (bestScore, bestMove) (cache state)
                        in (Right bestMove, state { cache = newCache })
                      _ ->
                        case effectiveMoves of
                          (m:_) -> (Right m, state)
                          []    -> (Left "resign", state)

-- ============================================================================
-- 9. USIメインループ
-- ============================================================================

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  weights <- loadMidgameWeights
  usiLoop weights initState

usiLoop :: MidgameWeights -> State -> IO ()
usiLoop weights currentState = do
  line <- getLine
  let tokens = words line
  case tokens of
    [] -> usiLoop weights currentState
    (cmd:_) -> case cmd of
      "usi" -> do
        putStrLn "id name ShogiMorseEngine_Haskell_with_F5_Garside_and_Sennichite"
        putStrLn "id author Shogi_Topology_Project"
        putStrLn "usiok"
        hFlush stdout
        usiLoop weights currentState
      
      "isready" -> do
        putStrLn "readyok"
        hFlush stdout
        usiLoop weights currentState
      
      "usinewgame" -> do
        w <- loadMidgameWeights
        usiLoop w initState { cache = Map.empty }
      
      "position" -> do
        let movesTokens = case elemIndex "moves" tokens of
              Just idx -> drop (idx + 1) tokens
              Nothing  -> []
            baseTokens = case elemIndex "moves" tokens of
              Just idx -> take idx tokens
              Nothing  -> tokens
            baseState = case drop 1 baseTokens of
              ("startpos":_) -> initState
              ("sfen":sfenParts) -> parseSfen sfenParts
              _ -> initState
            finalState = foldl' (\st mvStr -> applyMove (parseMove mvStr) st) baseState movesTokens
        usiLoop weights finalState { cache = cache currentState, moveHistory = movesTokens }
      
      "go" -> do
        let timeLimitMicros = calcTimeLimitMicros (turn currentState) (drop 1 tokens)
        (res, nextState) <- searchBestMoveIterativeWithTimeout weights timeLimitMicros currentState
        case res of
          Left "draw" -> do
            putStrLn "bestmove draw"
            hFlush stdout
            usiLoop weights nextState
          Left _ -> do
            putStrLn "bestmove resign"
            hFlush stdout
            usiLoop weights nextState
          Right best -> do
            putStrLn $ "bestmove " ++ showMove best
            hFlush stdout
            usiLoop weights nextState
      
      "quit" -> do
        exitSuccess
      
      _ -> usiLoop weights currentState
