{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import System.Exit
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map

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

-- 状態（手番、盤面、持ち駒、Garside置換表キャッシュ、トポロジー歴史履歴）
data State = State
  { turn    :: Player
  , board   :: Board
  , hands   :: Map.Map Player Hand
  , cache   :: Map.Map Board (Double, Move) -- Garside標準化キャッシュ（置換表）
  , history :: Map.Map Board Int            -- ★ トポロジー歴史：千日手検出用の盤面出現頻度
  } deriving (Show)

-- ============================================================================
-- 2. USIプロトコルパーサ ＆ 文字列変換
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
initState = State
  { turn    = Sente
  , board   = initBoard
  , hands   = Map.fromList [(Sente, emptyHand), (Gote, emptyHand)]
  , cache   = Map.empty
  , history = Map.fromList [(initBoard, 1)] -- 初期局面の登録
  }

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
  deriving (Eq, Show)

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

-- USI指し手文字列のパース
parseMove :: String -> Move
parseMove str
  | '*' `elem` str =
      case break (== '*') str of
        (pStr, _:toStr) ->
          let pt = case pStr of
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

      -- ★ 盤面出現頻度のインクリメント（トポロジー履歴）
      newHistory = Map.insertWith (+) newBoard 1 history
  in State (opponent turn) newBoard newHands cache newHistory
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
      -- ★ 盤面出現頻度のインクリメント
      newHistory = Map.insertWith (+) newBoard 1 history
  in State (opponent turn) newBoard newHands cache newHistory

-- ============================================================================
-- 3. F5フィルター（シジジー事前予測による自殺手・王手放置の完全排除）
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
    Nothing  -> False

-- | F5シジジーフィルター
f5SyzygyFilter :: State -> [Move] -> [Move]
f5SyzygyFilter state moves =
  let turnPlr = turn state
      validMoves = filter (\m -> not (isKingUnderAttack turnPlr (board (applyMove m state)))) moves
  in validMoves

-- ============================================================================
-- 4. 簡略型「モースポテンシャル」評価関数
-- ============================================================================

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

-- 局面の総合評価：(自玉の安全性) - (敵玉の安全性)
-- 注意：evalMorsePotential は高いほど玉が危険（破局）であるため、
-- 手番プレイヤーにとっては (敵玉の危険度) - (自玉の危険度) が高いほど有利となる。
evaluateState :: Player -> State -> Double
evaluateState player st =
  let bd = board st
      myDanger = evalMorsePotential player bd
      enemyDanger = evalMorsePotential (opponent player) bd
  in enemyDanger - myDanger

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
isSennichite :: Board -> Map.Map Board Int -> Bool
isSennichite bd hist =
  case Map.lookup bd hist of
    Just count -> count >= 4
    Nothing    -> False

-- ============================================================================
-- 7. 最善手選択 (Garside標準化置換表 ＆ F5フィルター ＆ 千日手回避)
-- ============================================================================

-- | 千日手（トポロジー無限ループ）の発生を回避し、最善手を選択する
-- 回避不可能な千日手引き分けが確定した場合は Left "draw" を返す
-- 指す手がない（詰み等）の場合は Left "resign" を返す
searchBestMoveWithCache :: State -> (Either String Move, State)
searchBestMoveWithCache state =
  case Map.lookup (board state) (cache state) of
    Just (_score, mv) ->
      -- ★ Garsideキャッシュヒット：ただし、過去の最善手を指した結果が「千日手」を引き起こさないか検証
      let nextState = applyMove mv state
      in if isSennichite (board nextState) (history nextState)
         then evaluateAndFilterNoCache state -- 千日手ループに陥るため、キャッシュをスルーして安全な手を動的に再探索
         else (Right mv, state)
    Nothing ->
      evaluateAndFilterNoCache state

  where
    evaluateAndFilterNoCache :: State -> (Either String Move, State)
    evaluateAndFilterNoCache st =
      let moves = generateMoves st
          f5FilteredMoves = f5SyzygyFilter st moves
          
          -- ★指し手のトポロジー分水嶺：千日手（無限ループ）になる手と、安全な手を分類
          (_sennichiteMoves, safeMoves) = 
            partition (\m -> 
              let nextState = applyMove m st
              in isSennichite (board nextState) (history nextState)
            ) f5FilteredMoves
      in if null f5FilteredMoves
         then (Left "resign", st)
         else if null safeMoves
           then
             -- ★ 逃げ道がない（安全な手が一切ない）が、千日手に持ち込む手がある場合：
             -- トポロジー的な無限収縮（引き分け）を受け入れ、USIの "bestmove draw" を送出する
             (Left "draw", st)
           else
             -- 安全な手の選択肢の中から、モースポテンシャルが最も安定（最急降下）する手を選択
             let scoredMoves = [ (evaluateState (turn st) (applyMove m st), m) | m <- safeMoves ]
                 sorted = sortBy (\(s1, _) (s2, _) -> compare s2 s1) scoredMoves
             in case sorted of
                  ((bestScore, bestMove) : _) ->
                    let newCache = Map.insert (board st) (bestScore, bestMove) (cache st)
                    in (Right bestMove, st { cache = newCache })
                  [] -> (Left "resign", st)

-- ============================================================================
-- 8. USIメインループ
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
        -- 新規対局開始時にキャッシュおよび千日手用履歴（history）を初期化
        usiLoop initState { cache = Map.empty }
      
      "position" -> do
        -- 例: position startpos moves 7g7f 3g3f
        let movesTokens = case elemIndex "moves" tokens of
              Just idx -> drop (idx + 1) tokens
              Nothing  -> []
            baseState = initState
            -- 指し手を順に適用しながら、現在の対局の出現履歴（history）を同期構築
            finalState = foldl' (\st mvStr -> applyMove (parseMove mvStr) st) baseState movesTokens
        -- 置換表キャッシュ（これまでの全探索知能）は、引き継いで維持する
        usiLoop finalState { cache = cache currentState }
      
      "go" -> do
        -- Garsideキャッシュ、F5フィルター、トポロジー的千日手回避を組み合わせた最善手検索
        let (res, nextState) = searchBestMoveWithCache currentState
        case res of
          Left "draw" -> do
            -- ★ 千日手引き分けが確定：USIプロトコルに準拠して "draw" を送出
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
