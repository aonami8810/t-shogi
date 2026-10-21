{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import System.Exit
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified CompiledMorseBook as Book

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

      -- 持ち駒（駒台）の更新
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

-- | 盤面の任意の位置 (targetSq) が、attackerPlayer から王手（攻撃）されているか判定する
isSquareUnderAttack :: Player -> Square -> Board -> Bool
isSquareUnderAttack attackerPlayer targetSq bd =
  any (\(sq, Piece _ pt) -> canPieceReach sq pt) attackerPieces
  where
    attackerPieces = [ (sq, p) | (sq, p@(Piece plr _)) <- Map.toList bd, plr == attackerPlayer ]
    
    canPieceReach :: Square -> PieceType -> Bool
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

-- | スライド駒の射線上に障害駒がないか確認（トポロジー的遮断判定）
isPathClear :: Square -> Square -> Board -> Bool
isPathClear (x1, y1) (x2, y2) bd =
  let dx = signum (x2 - x1)
      dy = signum (y2 - y1)
      path = takeWhile (/= (x2, y2)) $ drop 1 $ iterate (\(cx, cy) -> (cx + dx, cy + dy)) (x1, y1)
  in all (\sq -> not (Map.member sq bd)) path

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

pieceValue :: PieceType -> Double
pieceValue Pawn    = 100.0
pieceValue Lance   = 300.0
pieceValue Knight  = 400.0
pieceValue Silver  = 500.0
pieceValue Gold    = 600.0
pieceValue Bishop  = 800.0
pieceValue Rook    = 1000.0
pieceValue King    = 15000.0
pieceValue PPawn   = 600.0
pieceValue PLance  = 600.0
pieceValue PKnight = 600.0
pieceValue PSilver = 600.0
pieceValue PBishop = 1100.0
pieceValue PRook   = 1300.0

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
    findKing plr b = listToMaybe [sq | (sq, Piece p pt) <- Map.toList b, p == plr, pt == King]
    dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
    outOfBounds (x, y) = x < 1 || x > 9 || y < 1 || y > 9

-- 局面の総合評価：(敵玉のモースポテンシャル - 自玉のモースポテンシャル) + (駒得マテリアル差)
evaluateState :: Player -> State -> Double
evaluateState player st =
  let bd = board st
      myDanger = evalMorsePotential player bd
      enemyDanger = evalMorsePotential (opponent player) bd
      morseAdvantage = (enemyDanger - myDanger) * 10.0
      materialAdvantage = evalMaterial player st
  in morseAdvantage + materialAdvantage

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
      allPaths = Map.elems Book.runtimePaths
      matchingPaths = filter (\p -> currentMoves `isPrefixOf` p && length p > length currentMoves) allPaths
      candidates = [ parseMove (p !! length currentMoves) | p <- matchingPaths ]
      validBookMoves = filter (`elem` legalMoves) candidates
  in listToMaybe validBookMoves

-- ============================================================================
-- 8. 最善手選択 (型レベル定跡 ＆ Garside置換表 ＆ F5フィルター ＆ 千日手回避)
-- ============================================================================

searchBestMoveWithCache :: State -> (Either String Move, State)
searchBestMoveWithCache state =
  -- 1. まず型レベル定跡（V-Path スライド）を照会
  case lookupBookMove (moveHistory state) state of
    Just bookMove ->
      -- ★ 定跡ヒット：探索なしでノータイム指し
      (Right bookMove, state)
    Nothing ->
      -- 2. 定跡外または定跡完了後は、通常のモースポテンシャル探索（Garsideキャッシュ + F5フィルター）
      let moves = generateMoves state
          f5FilteredMoves = f5SyzygyFilter state moves
          
          (_sennichiteMoves, safeMoves) = 
            partition (\m -> 
              let nextState = applyMove m state
              in isSennichite (positionKey nextState) (history nextState)
            ) f5FilteredMoves
      in if null f5FilteredMoves
         then (Left "resign", state)
         else if null safeMoves
           then (Left "draw", state)
           else
             case Map.lookup (positionKey state) (cache state) of
               Just (_score, mv) | mv `elem` safeMoves -> (Right mv, state)
               _ ->
                 let scoredMoves = [ (evaluateState (turn state) (applyMove m state), m) | m <- safeMoves ]
                     sorted = sortBy (\(s1, _) (s2, _) -> compare s2 s1) scoredMoves
                 in case sorted of
                      ((bestScore, bestMove) : _) ->
                        let newCache = Map.insert (positionKey state) (bestScore, bestMove) (cache state)
                        in (Right bestMove, state { cache = newCache })
                      [] -> (Left "resign", state)

-- ============================================================================
-- 9. USIメインループ
-- ============================================================================

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  usiLoop initState

usiLoop :: State -> IO ()
usiLoop currentState = do
  line <- getLine
  let tokens = words line
  case tokens of
    [] -> usiLoop currentState
    (cmd:_) -> case cmd of
      "usi" -> do
        putStrLn "id name ShogiMorseEngine_Haskell_with_F5_Garside_and_Sennichite"
        putStrLn "id author Shogi_Topology_Project"
        putStrLn "usiok"
        usiLoop currentState
      
      "isready" -> do
        putStrLn "readyok"
        usiLoop currentState
      
      "usinewgame" -> do
        usiLoop initState { cache = Map.empty }
      
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
        usiLoop finalState { cache = cache currentState, moveHistory = movesTokens }
      
      "go" -> do
        let (res, nextState) = searchBestMoveWithCache currentState
        case res of
          Left "draw" -> do
            putStrLn "bestmove draw"
            usiLoop nextState
          Left _ -> do
            putStrLn "bestmove resign"
            usiLoop nextState
          Right best -> do
            putStrLn $ "bestmove " ++ showMove best
            usiLoop nextState
      
      "quit" -> do
        exitSuccess
      
      _ -> usiLoop currentState
