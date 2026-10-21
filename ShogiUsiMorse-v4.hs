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
  { wMaterial       :: !Double -- 駒得重み (標準 1.0)
  , wMorseDanger    :: !Double -- モース玉ポテンシャル重み (標準 6.0)
  , wAttack         :: !Double -- 中盤攻め筋（成駒・玉頭圧力・拠点）重み (標準 1.3)
  , wDefense        :: !Double -- 中盤受け筋（金銀囲い・連結・敵成駒排除）重み (標準 1.3)
  , wKakuto         :: !Double -- 序盤角頭・桂頭防衛重み (標準 1.0)
  , wLoosePenalty   :: !Double -- 放れ駒ペナルティ重み (標準 1.0)
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

-- | 移動先マスが相手の利きに晒されており、かつ味方の紐が付いていない（または安い駒で取られる）大悪手・タダ捨て手か判定
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
      
      -- 移動先マスにおける相手の利き駒一覧
      oppAttackers = getPiecesCovering opp toSq bd
      -- 移動後にそのマスに味方の利き（紐）が付いているか
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
          then ptMoved /= Pawn -- 紐なしで歩以外の駒（銀、金、飛、角、桂、香）を相手の利きに飛び込ませるのは絶対タダ捨て！
          else movedVal > minOppVal + 250.0 -- 紐があっても、歩などで金銀飛角を取られる大損交換は悪手

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

-- | 局面の総合評価：重み付き中盤攻防統合評価関数
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
      -- 角頭歩突きは定跡であっても絶対に除外
      safeLegalMoves = filter (not . isPushingKakutoPawn st) legalMoves
      allPaths = Map.elems Book.runtimePaths
      matchingPaths = filter (\p -> currentMoves `isPrefixOf` p && length p > length currentMoves) allPaths
      candidates = [ parseMove (p !! length currentMoves) | p <- matchingPaths ]
      validBookMoves = filter (`elem` safeLegalMoves) candidates
      
      -- 初手（currentMoves == []）の場合、7六歩 (7g7f) または 2六歩 (2g2f) を最優先
      sortedMoves =
        if null currentMoves
        then sortBy (\m1 m2 ->
               let score m = case m of
                     NormalMove (7, 7) (7, 6) False -> 2 :: Int
                     NormalMove (2, 7) (2, 6) False -> 1 :: Int
                     _                              -> 0 :: Int
               in compare (score m2) (score m1)
             ) validBookMoves
        else validBookMoves
  in listToMaybe sortedMoves

-- ============================================================================
-- 8. 最善手選択 (型レベル定跡 ＆ 静止探索付きα-β探索 ＆ Garside置換表 ＆ F5フィルター ＆ 千日手回避)
-- ============================================================================

-- | 静止探索（駒の取り合いが落ち着くまで捕獲手を先読みする: 水平線効果の解消）
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

-- | ネガマックス α-β 探索 (自手番の視点でのスコアと最善手を返す: 3-Ply + 静止探索2-Ply)
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
      -- ★ 角頭歩突き (8g8f, 2c2d) は探索候補から完全除外
      nonKakutoMoves = filter (not . isPushingKakutoPawn st) safeMoves
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
searchBestMoveIterativeWithTimeout weights timeLimitMicros state =
  -- 1. まず型レベル定跡（V-Path スライド）を照会
  case lookupBookMove (moveHistory state) state of
    Just bookMove -> do
      putStrLn $ "info string book hit pv " ++ showMove bookMove
      hFlush stdout
      return (Right bookMove, state)
    Nothing -> do
      -- 2. 定跡外または定跡完了後は、反復深化α-β探索（Garsideキャッシュ + F5フィルター + タダ捨て防止 + 静止探索）
      let moves = generateMoves state
          f5FilteredMoves = f5SyzygyFilter state moves
          
          (_sennichiteMoves, safeMoves) = 
            partition (\m -> 
              let nextState = applyMove m state
              in isSennichite (positionKey nextState) (history nextState)
            ) f5FilteredMoves
          nonKakutoMoves = filter (not . isPushingKakutoPawn state) safeMoves
          effectiveMoves = if null nonKakutoMoves then safeMoves else nonKakutoMoves
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
                               Just mv -> do
                                 -- 遅延評価を解除して正格に評価を確定
                                 _ <- evaluate (score `seq` mv `seq` ())
                                 writeIORef bestRef (score, mv)
                                 putStrLn $ "info depth " ++ show d ++ " score cp " ++ show (round score :: Int) ++ " pv " ++ showMove mv
                                 hFlush stdout
                                 if abs score >= 9000.0
                                   then return () -- 詰みを発見した場合は即座に着手
                                   else runIDS (d + 1)
                               Nothing -> return ()
                   runIDS 1
                 
                 -- タイムアウトした場合でも、直前までに探索が完了した最高深さの最善手を取得
                 (bestScore, bestMove) <- readIORef bestRef
                 let newCache = Map.insert (positionKey state) (bestScore, bestMove) (cache state)
                 return (Right bestMove, state { cache = newCache })

-- 後方互換用：純粋探索
searchBestMoveWithCache :: MidgameWeights -> State -> (Either String Move, State)
searchBestMoveWithCache weights state =
  case lookupBookMove (moveHistory state) state of
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
          nonKakutoMoves = filter (not . isPushingKakutoPawn state) safeMoves
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
                      Just bestMove ->
                        let newCache = Map.insert (positionKey state) (bestScore, bestMove) (cache state)
                        in (Right bestMove, state { cache = newCache })
                      Nothing ->
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
