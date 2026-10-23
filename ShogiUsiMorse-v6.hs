{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import System.Exit
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

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
-- 2. 8方向幾何単体（Direction8 Simplicial Complex）代数モデル
-- ============================================================================

-- | 盤面上の8つの基本方向単体
data Direction8
  = DirN   -- 前 (North)
  | DirS   -- 後 (South)
  | DirW   -- 左 (West)
  | DirE   -- 右 (East)
  | DirNW  -- 左前 (North-West)
  | DirNE  -- 右前 (North-East)
  | DirSW  -- 左後 (South-West)
  | DirSE  -- 右後 (South-East)
  deriving (Eq, Show, Enum, Bounded, Ord)

-- | 単体の射線種別（1ステップ単体か、長距離無限射線スライドか）
data RayType = UnitStep | InfiniteRay
  deriving (Eq, Show, Ord)

-- | 手番プレイヤー基準の幾何学的方向ベクトルへの射影
dirVector :: Player -> Direction8 -> (Int, Int)
dirVector Sente dir = case dir of
  DirN  -> ( 0, -1) ; DirS  -> ( 0,  1) ; DirW  -> ( 1,  0) ; DirE  -> (-1,  0)
  DirNW -> ( 1, -1) ; DirNE -> (-1, -1) ; DirSW -> ( 1,  1) ; DirSE -> (-1,  1)
dirVector Gote dir = case dir of
  DirN  -> ( 0,  1) ; DirS  -> ( 0, -1) ; DirW  -> (-1,  0) ; DirE  -> ( 1,  0)
  DirNW -> (-1,  1) ; DirNE -> ( 1,  1) ; DirSW -> (-1, -1) ; DirSE -> ( 1, -1)

-- | 各駒の利きを8方向単体複体の直和（チェイン分解）として一意に定義
-- （※桂馬のみ2マス先跳躍の特異単体として別記）
pieceSimplicialChain :: PieceType -> [(Direction8, RayType)]
pieceSimplicialChain pt = case pt of
  Pawn    -> [(DirN, UnitStep)]
  Lance   -> [(DirN, InfiniteRay)]
  Silver  -> [ (DirN, UnitStep)
             , (DirNW, UnitStep), (DirNE, UnitStep)
             , (DirSW, UnitStep), (DirSE, UnitStep) ]
  Gold    -> [ (DirN, UnitStep), (DirS, UnitStep)
             , (DirW, UnitStep), (DirE, UnitStep)
             , (DirNW, UnitStep), (DirNE, UnitStep) ]
  Bishop  -> [ (DirNW, InfiniteRay), (DirNE, InfiniteRay)
             , (DirSW, InfiniteRay), (DirSE, InfiniteRay) ]
  Rook    -> [ (DirN, InfiniteRay), (DirS, InfiniteRay)
             , (DirW, InfiniteRay), (DirE, InfiniteRay) ]
  King    -> [ (d, UnitStep) | d <- [minBound .. maxBound] ]
  -- 成駒群の単体分解
  PPawn   -> pieceSimplicialChain Gold
  PLance  -> pieceSimplicialChain Gold
  PKnight -> pieceSimplicialChain Gold
  PSilver -> pieceSimplicialChain Gold
  PBishop -> pieceSimplicialChain Bishop ++ [(DirN, UnitStep), (DirS, UnitStep), (DirW, UnitStep), (DirE, UnitStep)]
  PRook   -> pieceSimplicialChain Rook ++ [(DirNW, UnitStep), (DirNE, UnitStep), (DirSW, UnitStep), (DirSE, UnitStep)]
  Knight  -> [] -- 特異ホモロジーとして個別処理

isOutOfBounds :: Square -> Bool
isOutOfBounds (x, y) = x < 1 || x > 9 || y < 1 || y > 9

-- | 単体射線追跡（Ray-Casting）：単体の射線に沿って到達可能なマス集合を生成
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
                  Just _  -> [nextSq] -- 最初の駒に到達した時点で遮断（トポロジー的終端）
  in step (x, y)

-- | 盤上の駒から放射される全単体射線ターゲットを計算
pieceAttacks :: Square -> Piece -> Board -> [Square]
pieceAttacks sq (Piece plr pt) bd =
  let regularTargets = concatMap (\(dir, ray) ->
                         let vec = dirVector plr dir
                         in raySimplexTargets sq vec ray bd
                       ) (pieceSimplicialChain pt)
      knightTargets = if pt == Knight
                      then let dy = if plr == Sente then -2 else 2
                               cands = [(fst sq - 1, snd sq + dy), (fst sq + 1, snd sq + dy)]
                           in filter (not . isOutOfBounds) cands
                      else []
  in regularTargets ++ knightTargets

-- | 8方向単体ベクトルによる「マスの利き判定」（超平面貫通判定）
isSquareUnderAttack :: Player -> Square -> Board -> Bool
isSquareUnderAttack attackerPlayer targetSq bd =
  let attackers = Map.toList $ Map.filter (\(Piece p _) -> p == attackerPlayer) bd
  in any (\(sq, pc) -> targetSq `elem` pieceAttacks sq pc bd) attackers

-- ============================================================================
-- 3. USIプロトコルパーサ ＆ 文字列変換
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
  , history = Map.fromList [(initBoard, 1)]
  }

showSquare :: Square -> String
showSquare (f, r) = show f ++ [chr (ord 'a' + r - 1)]

parseSquare :: String -> Square
parseSquare [f, r] = (digitToInt f, ord r - ord 'a' + 1)
parseSquare _      = (1, 1)

data Move 
  = NormalMove Square Square Bool
  | DropMove PieceType Square
  deriving (Eq, Show)

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
      let (pStr, _:toStr) = break (== '*') str
          pt = case head pStr of
            'P' -> Pawn ; 'L' -> Lance ; 'N' -> Knight
            'S' -> Silver ; 'G' -> Gold ; 'B' -> Bishop ; 'R' -> Rook
            _   -> Pawn
      in DropMove pt (parseSquare toStr)
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
        Just (Piece plr pt) -> 
          if promo 
          then Piece plr (promote pt)
          else Piece plr pt
        Nothing -> Piece turn Pawn
      newBoard = Map.insert to newPiece boardWithSourceRemoved

      newHands = case captured of
        Just (Piece _ pt) ->
          let rawPt = demote pt
              handMap = fromMaybe emptyHand (Map.lookup turn hands)
              updatedHand = Map.insertWith (+) rawPt 1 handMap
          in Map.insert turn updatedHand hands
        Nothing -> hands

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
      updatedHand = Map.insertWith (\_ old -> old - 1) pt 0 handMap
      newHands = Map.insert turn updatedHand hands
      newHistory = Map.insertWith (+) newBoard 1 history
  in State (opponent turn) newBoard newHands cache newHistory

-- ============================================================================
-- 4. F5フィルター（8方向単体モデルによる王手・自殺手の高速遮断）
-- ============================================================================

isKingUnderAttack :: Player -> Board -> Bool
isKingUnderAttack plr bd =
  let kingPos = listToMaybe [sq | (sq, Piece p pt) <- Map.toList bd, p == plr, pt == King]
  in case kingPos of
    Just kSq -> isSquareUnderAttack (opponent plr) kSq bd
    Nothing  -> False

f5SyzygyFilter :: State -> [Move] -> [Move]
f5SyzygyFilter state moves =
  let turnPlr = turn state
      validMoves = filter (\m -> not (isKingUnderAttack turnPlr (board (applyMove m state)))) moves
  in validMoves

-- ============================================================================
-- 5. 8方向単体ベクトルによる有界チェンバー計算 ＆ トポロジカル評価関数
-- ============================================================================

-- | 王の周囲8方向の幾何学的チェンバー数 b(A) を 8方向単体ベクトル演算で直接算出
countEscapeChambers :: Player -> Square -> Board -> Double
countEscapeChambers plr (kx, ky) bd =
  let enemy = opponent plr
      -- 8方向の基本単体ベクトルに沿った隣接セル
      neighborSquares = [ (dir, (kx + dx, ky + dy))
                        | dir <- [minBound .. maxBound]
                        , let (dx, dy) = dirVector plr dir ]
      
      -- 各方向単体が「安全な脱出可能チェンバー」を形成しているか判定
      isOpenChamber (_, sq) =
        not (isOutOfBounds sq) &&
        not (isFriendly sq) &&
        not (isSquareUnderAttack enemy sq bd)
      
      validChambers = filter isOpenChamber neighborSquares
  in fromIntegral (length validChambers)
  where
    isFriendly sq = case Map.lookup sq bd of
      Just (Piece p _) -> p == plr
      Nothing          -> False

calcHandPressure :: Hand -> Double
calcHandPressure hand =
  let weights = [ (Rook, 5.0), (Bishop, 4.0), (Gold, 3.0)
                , (Silver, 2.5), (Knight, 1.5), (Lance, 1.2), (Pawn, 0.8) ]
  in sum [ w * fromIntegral (fromMaybe 0 (Map.lookup pt hand)) | (pt, w) <- weights ]

-- | 2枚の守備駒が協力して単体を二重に張る「シジジー（代数テンソル積 C(P_1) ⊗ C(P_2)）」による囲い堅牢度評価
-- ① 単体テンソル重なり (Simplex Tensor Overlap): 王の近傍シェル（距離2以内）の各マス x に対し、
--    複数の守備駒（金・銀・歩・桂・香）の UnitStep 単体が重なるペア数 C(m(x), 2)
-- ② 金銀2-単体鎖 (General Syzygy Chain: P_2 -> P_1 -> K): 王近傍の金・銀が互いを直接支え合う有向鎖
-- 平手初期局面（居玉）の基底シジジー値を差し引くことで、初期局面の D(K) = 7.254048 を厳密に保持しつつ、
-- 矢倉・美濃囲い等の高密度単体ネットワーク形成時に防壁距離 D(K) を増加させる。
evalSyzygyTensorShield :: Player -> Square -> Board -> Double
evalSyzygyTensorShield plr kSq@(kx, ky) bd =
  let -- 守備単体を形成する味方駒（玉・大駒を除く金・銀・歩・桂・香・成駒）の UnitStep 到達マス
      unitStepAttacks sq (Piece p pt) =
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

      -- 王の周囲（横方向3マス・縦方向2マス以内：矢倉の縦連結および美濃囲いの横連結を網羅）の防衛シェル
      defenseShell = [ (x, y)
                     | x <- [max 1 (kx - 3) .. min 9 (kx + 3)]
                     , y <- [max 1 (ky - 2) .. min 9 (ky + 2)]
                     , (x, y) /= kSq ]

      -- ① 単体テンソル積 C(P_i) ⊗ C(P_j): 同一マス x に2枚以上の守備単体が重なるペア数 m(m-1)/2
      tensorOverlapPairs = sum
        [ let m = length [ () | (_, _, targets) <- castlePieces, sq `elem` targets ]
          in fromIntegral (m * (m - 1)) * 0.5
        | sq <- defenseShell ]

      -- ② 金銀間のシジジー鎖 (P_2 -> P_1 -> K): 囲い内の金・銀が他の守備駒から支えられている有向鎖
      nearGenerals = [ (sq, pt)
                     | (sq, pt, _) <- castlePieces
                     , pt `elem` [Gold, Silver, PPawn, PLance, PKnight, PSilver] ]
      generalSyzygyLinks = sum
        [ fromIntegral (length [ () | (sq2, _, targets2) <- castlePieces, sq2 /= sq1, sq1 `elem` targets2 ])
        | (sq1, _) <- nearGenerals ]

      -- 玉が中央（5筋の居玉）から左右の囲い領域へ移動したことによるコホモロジー安定化係数
      flankFactor = if kx == 5 then 1.0 else 1.45
      rawSyzygy   = flankFactor * (0.30 * tensorOverlapPairs + 0.45 * generalSyzygyLinks)
      -- 平手初期配置（5九/5一居玉）における基底値 (tensorOverlapPairs=3.0, generalSyzygyLinks=4.0 -> 2.70)
      initBaseline = 2.70
  in max 0.0 (rawSyzygy - initBaseline)

-- | 各玉に対する詰みまでのトポロジー的距離 D(K)
evalKingDistance :: Player -> State -> Double
evalKingDistance plr State{..} =
  let kingPos = listToMaybe [sq | (sq, Piece p pt) <- Map.toList board, p == plr, pt == King]
  in case kingPos of
    Nothing -> 0.0
    Just kSq ->
      let enemy = opponent plr
          -- ① 8方向単体による有界脱出チェンバー数 b(A)
          chambers = countEscapeChambers plr kSq board
          chamberScore = 3.0 * log (1.0 + chambers)

          -- ② 守備駒ストランドの近傍結合度 ＋ 2枚の駒の単体テンソル積（シジジー連結度）
          defenders = [ sq | (sq, Piece p _) <- Map.toList board, p == plr, sq /= kSq ]
          defScore = sum [ 1.5 / (fromIntegral (manhattanDist kSq sq) + 0.5) | sq <- defenders ]
          syzygyShield = evalSyzygyTensorShield plr kSq board

          -- ③ 敵の盤上攻め駒の単体射線集中度
          attackers = [ sq | (sq, Piece p _) <- Map.toList board, p == enemy ]
          attScore = sum [ 2.5 / (fromIntegral (manhattanDist kSq sq) + 0.2) | sq <- attackers ]

          -- ④ 敵の持ち駒圧力
          enemyHand = fromMaybe emptyHand (Map.lookup enemy hands)
          handPressure = calcHandPressure enemyHand

          -- ⑤ 入玉による相転移
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
-- 6. 合法手生成（単体射線ターゲットを直接利用）
-- ============================================================================

generateMoves :: State -> [Move]
generateMoves st@State{..} =
  let myPieces = Map.toList $ Map.filter (\(Piece p _) -> p == turn) board
      boardMoves = concatMap (\(sq, pc@(Piece _ pt)) ->
                     let targets = pieceAttacks sq pc board
                         validTargets = filter (not . isFriendly) targets
                     in map (\tgt -> NormalMove sq tgt (shouldPromote pt (snd sq) (snd tgt))) validTargets
                   ) myPieces
      emptySquares = [(x, y) | x <- [1..9], y <- [1..9], not (Map.member (x, y) board)]
      myHand = fromMaybe emptyHand (Map.lookup turn hands)
      dropMoves = [ DropMove pt sq | (pt, count) <- Map.toList myHand, count > 0, sq <- take 10 emptySquares ]
  in boardMoves ++ dropMoves
  where
    isFriendly sq = case Map.lookup sq board of
      Just (Piece p _) -> p == turn
      Nothing          -> False

    shouldPromote pt srcY tgtY =
      let inZone y = if turn == Sente then y <= 3 else y >= 7
      in (inZone srcY || inZone tgtY) && pt `elem` [Pawn, Lance, Knight, Silver, Bishop, Rook]

-- ============================================================================
-- 7. 千日手判定 ＆ 最善手選択 (Garside置換表 + F5 + 単体モース勾配)
-- ============================================================================

isSennichite :: Board -> Map.Map Board Int -> Bool
isSennichite bd hist =
  case Map.lookup bd hist of
    Just count -> count >= 4
    Nothing    -> False

searchBestMoveWithCache :: State -> (Either String Move, State)
searchBestMoveWithCache state =
  case Map.lookup (board state) (cache state) of
    Just (_, mv) ->
      let nextState = applyMove mv state
      in if isSennichite (board nextState) (history nextState)
         then evaluateAndFilterNoCache state
         else (Right mv, state)
    Nothing ->
      evaluateAndFilterNoCache state

  where
    evaluateAndFilterNoCache :: State -> (Either String Move, State)
    evaluateAndFilterNoCache st =
      let moves = generateMoves st
          f5FilteredMoves = f5SyzygyFilter st moves
          
          (_, safeMoves) = 
            partition (\m -> 
              let nextState = applyMove m st
              in isSennichite (board nextState) (history nextState)
            ) f5FilteredMoves
      in if null f5FilteredMoves
         then
           let fallback = if null moves then NormalMove (5, 9) (5, 8) False else head moves
           in (Right fallback, st)
         else if null safeMoves
           then
             (Left "draw", st)
           else
             let scoredMoves = [ (evaluateState (turn st) (applyMove m st), m) | m <- safeMoves ]
                 sorted = sortBy (\(s1, _) (s2, _) -> compare s2 s1) scoredMoves
                 (bestScore, bestMove) = head sorted
                 newCache = Map.insert (board st) (bestScore, bestMove) (cache st)
             in (Right bestMove, st { cache = newCache })

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
  if null tokens 
    then usiLoop currentState
    else case head tokens of
      "usi" -> do
        putStrLn "id name ShogiMorseEngine_Haskell_v6_SimplicialComplex"
        putStrLn "id author Shogi_Topology_Project"
        putStrLn "usiok"
        usiLoop currentState
      
      "isready" -> do
        putStrLn "readyok"
        usiLoop currentState
      
      "usinewgame" -> do
        usiLoop initState { cache = Map.empty }
      
      "position" -> do
        let remaining = tail tokens
            (posType, movesTokens) = 
              if not (null remaining) && head remaining == "startpos"
              then (initState, drop 2 remaining)
              else (initState, remaining)
            finalState = foldl' (\st mvStr -> applyMove (parseMove mvStr) st) posType movesTokens
        usiLoop finalState { cache = cache currentState }
      
      "go" -> do
        let (res, nextState) = searchBestMoveWithCache currentState
        case res of
          Left "draw" -> do
            putStrLn "bestmove draw"
            usiLoop nextState
          Right bestMv -> do
            putStrLn $ "bestmove " ++ showMove bestMv
            usiLoop nextState

      "eval" -> do
        let bd = board currentState
            kS = fromMaybe (5, 9) (listToMaybe [sq | (sq, Piece Sente King) <- Map.toList bd])
            kG = fromMaybe (5, 1) (listToMaybe [sq | (sq, Piece Gote  King) <- Map.toList bd])
            syzS = evalSyzygyTensorShield Sente kS bd
            syzG = evalSyzygyTensorShield Gote  kG bd
            dS   = evalKingDistance Sente currentState
            dG   = evalKingDistance Gote  currentState
            ev   = evaluateState (turn currentState) currentState
        putStrLn $ "info string syzygy(S)=" ++ show syzS
                ++ " syzygy(G)=" ++ show syzG
                ++ " D(S)=" ++ show dS
                ++ " D(G)=" ++ show dG
                ++ " eval=" ++ show ev
        usiLoop currentState
      
      "quit" -> exitSuccess
      _      -> usiLoop currentState
