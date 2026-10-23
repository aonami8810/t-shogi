{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import System.IO
import System.Exit
import Data.Char
import Data.List
import Data.Maybe
import Data.Bits (Bits(..))
import Data.Word (Word8)
import Data.Ord (comparing)
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

-- 指し手表現（辞書式順序 Ord により Garside 左正規形の全順序を定義）
data Move 
  = NormalMove Square Square Bool -- 移動元, 移動先, 成るか
  | DropMove PieceType Square     -- 持ち駒の種類, 打つ場所
  deriving (Eq, Ord, Show)

-- 局面の完全一意キー（手番、盤面、両者の持ち駒）
type PositionKey = (Player, Board, Map.Map Player Hand)

-- 状態（手番、盤面、持ち駒、Garside置換表キャッシュ、トポロジー歴史履歴、直前の可換手記録）
data State = State
  { turn          :: Player
  , board         :: Board
  , hands         :: Map.Map Player Hand
  , cache         :: Map.Map PositionKey (Double, Move) -- Garside標準化キャッシュ（置換表）
  , history       :: Map.Map PositionKey Int            -- ★ トポロジー歴史：千日手検出用の局面出現頻度
  , lastCommMove  :: Map.Map Player (Maybe Move)        -- ★ Garside左正規形スライド判定用の直前可換手
  } deriving (Show)

positionKey :: State -> PositionKey
positionKey State{..} = (turn, board, hands)

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
initState =
  let initHands = Map.fromList [(Sente, emptyHand), (Gote, emptyHand)]
      initKey   = (Sente, initBoard, initHands)
  in State
       { turn         = Sente
       , board        = initBoard
       , hands        = initHands
       , cache        = Map.empty
       , history      = Map.singleton initKey 1
       , lastCommMove = Map.fromList [(Sente, Nothing), (Gote, Nothing)]
       }

-- 座標をUSI文字列に変換 (例: (7, 7) -> "7g")
showSquare :: Square -> String
showSquare (f, r) = show f ++ [chr (ord 'a' + r - 1)]

-- USI文字列を座標に変換 (例: "7g" -> (7, 7))
parseSquare :: String -> Square
parseSquare [f, r] = (digitToInt f, ord r - ord 'a' + 1)
parseSquare _      = (1, 1)

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
      let (pStr, restStr) = break (== '*') str
          toStr = drop 1 restStr
          pt = case listToMaybe pStr of
            Just 'P' -> Pawn   ; Just 'L' -> Lance  ; Just 'N' -> Knight
            Just 'S' -> Silver ; Just 'G' -> Gold   ; Just 'B' -> Bishop ; Just 'R' -> Rook
            _        -> Pawn
      in DropMove pt (parseSquare toStr)
  | otherwise =
      let fromStr = take 2 str
          toStr   = take 2 (drop 2 str)
          promo   = "+" `isSuffixOf` str
      in NormalMove (parseSquare fromStr) (parseSquare toStr) promo

-- 指し手の適用（トポロジー歴史履歴 history の自動蓄積 ＆ 不可逆手によるサイクル履歴の確定解放）
applyMove :: Move -> State -> State
applyMove mv@(NormalMove from to promo) st@State{..} =
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

      nextTurn = opponent turn
      nextKey  = (nextTurn, newBoard, newHands)

      -- ★ 不可逆手（歩・香・桂・銀の前進、成り、駒取り）の場合は過去の同値サイクル履歴を確定解放
      irreversible = isJust captured || promo || isIrreversibleAdvance turn board mv
      newHistory = if irreversible
                   then Map.singleton nextKey 1
                   else Map.insertWith (+) nextKey 1 history

      -- ★ Garside左正規形用：可換手なら記録し、非可換手ならバリア（Nothing）にリセット
      mClass = classifyMove st mv
      newLastComm = case mClass of
        CommutativeMove -> Map.insert turn (Just mv) lastCommMove
        _               -> Map.fromList [(Sente, Nothing), (Gote, Nothing)]
  in State nextTurn newBoard newHands cache newHistory newLastComm
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
      updatedHand = Map.insertWith (\_ old -> max 0 (old - 1)) pt 0 handMap
      newHands = Map.insert turn updatedHand hands
      nextTurn = opponent turn
      nextKey  = (nextTurn, newBoard, newHands)
      -- 駒打ちは特異非可換手なので直前可換手バリアをリセット
      newHistory = Map.insertWith (+) nextKey 1 history
      newLastComm = Map.fromList [(Sente, Nothing), (Gote, Nothing)]
  in State nextTurn newBoard newHands cache newHistory newLastComm

-- ============================================================================
-- 3. 8方向基本単体（Direction8）チェイン分解 ＆ F5シジジーフィルター
-- ============================================================================

-- | 1. 8方向の基底幾何単体（手番視点の局所座標系）
data Direction8
  = DirN   -- 前 (North:      ( 0, -1))
  | DirS   -- 後 (South:      ( 0, +1))
  | DirW   -- 左 (West:       (-1,  0))
  | DirE   -- 右 (East:       (+1,  0))
  | DirNW  -- 左前 (North-West: (-1, -1))
  | DirNE  -- 右前 (North-East: (+1, -1))
  | DirSW  -- 左後 (South-West: (-1, +1))
  | DirSE  -- 右後 (South-East: (+1, +1))
  deriving (Eq, Show, Enum, Bounded, Ord)

-- | 8方向単体のビットマスク表現（1ビット = 1基本単体 Δ_d、ビットORで単体合併 ⋃ Δ_d）
type SimplexMask8 = Word8

dirBit :: Direction8 -> SimplexMask8
dirBit d = bit (fromEnum d)

-- | 先手基準の方向ベクトルへの射影
dirVector :: Direction8 -> (Int, Int)
dirVector DirN  = ( 0, -1)
dirVector DirS  = ( 0,  1)
dirVector DirW  = (-1,  0)
dirVector DirE  = ( 1,  0)
dirVector DirNW = (-1, -1)
dirVector DirNE = ( 1, -1)
dirVector DirSW = (-1,  1)
dirVector DirSE = ( 1,  1)

-- | プレイヤー視点に応じた盤面上の実方向ベクトル (dx, dy)
playerDirVector :: Player -> Direction8 -> (Int, Int)
playerDirVector Sente d = dirVector d
playerDirVector Gote  d = let (dx, dy) = dirVector d in (-dx, -dy)

-- | 2. 単体の射線種別（1ステップ単体か、長距離スライド射線か）
data RayType = UnitStep | InfiniteRay
  deriving (Eq, Ord, Show)

-- | 3. 各駒が保持する8方向単体複体 C(P) = ⊕ c_d(P) Δ_d の記述
data SimplicialPiece = SimplicialPiece
  { pieceOwner      :: Player
  , activeSimplices :: [(Direction8, RayType)]
  , hasKnightHop    :: Bool -- 桂馬の特異ホモロジー跳躍 (2Δ_N ± Δ_W/E)
  } deriving (Eq, Show)

-- | 各駒の8方向単体複体マッピング C(P)
toSimplicialComplex :: Piece -> SimplicialPiece
toSimplicialComplex (Piece plr pt) =
  SimplicialPiece plr (getSimplices pt) (pt == Knight)
  where
    goldChain = [ (DirN, UnitStep), (DirS, UnitStep)
                , (DirW, UnitStep), (DirE, UnitStep)
                , (DirNW, UnitStep), (DirNE, UnitStep) ]
    getSimplices Pawn    = [(DirN, UnitStep)]
    getSimplices Lance   = [(DirN, InfiniteRay)]
    getSimplices Knight  = [] -- 跳躍単体は hasKnightHop で処理
    getSimplices Silver  = [ (DirN, UnitStep)
                           , (DirNW, UnitStep), (DirNE, UnitStep)
                           , (DirSW, UnitStep), (DirSE, UnitStep) ]
    getSimplices Gold    = goldChain
    getSimplices PPawn   = goldChain
    getSimplices PLance  = goldChain
    getSimplices PKnight = goldChain
    getSimplices PSilver = goldChain
    getSimplices Bishop  = [ (d, InfiniteRay) | d <- [DirNW, DirNE, DirSW, DirSE] ]
    getSimplices PBishop = [ (d, InfiniteRay) | d <- [DirNW, DirNE, DirSW, DirSE] ]
                        ++ [ (d, UnitStep)    | d <- [DirN, DirS, DirW, DirE] ]
    getSimplices Rook    = [ (d, InfiniteRay) | d <- [DirN, DirS, DirW, DirE] ]
    getSimplices PRook   = [ (d, InfiniteRay) | d <- [DirN, DirS, DirW, DirE] ]
                        ++ [ (d, UnitStep)    | d <- [DirNW, DirNE, DirSW, DirSE] ]
    getSimplices King    = [ (d, UnitStep)    | d <- [minBound .. maxBound] ]

-- | 駒種が持つ8方向単体ビットマスク（商空間での単体機能比較用）
pieceSimplexMask8 :: PieceType -> SimplexMask8
pieceSimplexMask8 pt =
  let SimplicialPiece _ chain _ = toSimplicialComplex (Piece Sente pt)
  in foldl' (\acc (d, _) -> acc .|. dirBit d) 0 chain

isOutOfBounds :: Square -> Bool
isOutOfBounds (x, y) = x < 1 || x > 9 || y < 1 || y > 9

-- | スライド駒の射線上に障害駒がないか確認（トポロジー的遮断判定）
isPathClear :: Square -> Square -> Board -> Bool
isPathClear (x1, y1) (x2, y2) bd =
  let dx = signum (x2 - x1)
      dy = signum (y2 - y1)
      path = takeWhile (/= (x2, y2)) $ drop 1 $ iterate (\(cx, cy) -> (cx + dx, cy + dy)) (x1, y1)
  in all (\sq -> not (Map.member sq bd)) path

-- | 指定マス targetSq に到達している attackerPlayer の8方向抽象単体の合併ビットマスク (⋃ Δ_d) を計算
-- 駒の名前（金・銀・角等）を剥がし、どの方向単体 Δ_d が targetSq を貫通しているかを8ビットで並列抽出
attackingSimplexMask8 :: Player -> Square -> Board -> (SimplexMask8, Bool)
attackingSimplexMask8 attackerPlayer targetSq bd =
  Map.foldlWithKey' accumulate (0, False) bd
  where
    accumulate (maskAcc, knightAcc) sq piece@(Piece p _)
      | p /= attackerPlayer || sq == targetSq = (maskAcc, knightAcc)
      | otherwise =
          let SimplicialPiece _ chain isKnight = toSimplicialComplex piece
              dx = fst targetSq - fst sq
              dy = snd targetSq - snd sq
              dirY = if attackerPlayer == Sente then -1 else 1
              knightHit = isKnight && abs dx == 1 && dy == 2 * dirY
              matchedMask = foldl' (\m (d, rayType) ->
                let (vx, vy) = playerDirVector attackerPlayer d
                    reaches = case rayType of
                      UnitStep    -> dx == vx && dy == vy
                      InfiniteRay ->
                        (if vx == 0 then dx == 0 else dx * vx > 0 && abs dx > 0) &&
                        (if vy == 0 then dy == 0 else dy * vy > 0 && abs dy > 0) &&
                        (vx == 0 || vy == 0 || abs dx == abs dy) &&
                        isPathClear sq targetSq bd
                in if reaches then m .|. dirBit d else m
                ) 0 chain
          in (maskAcc .|. matchedMask, knightAcc || knightHit)

-- | 盤面の任意の位置 (targetSq) が、attackerPlayer の単体射線または跳躍単体で貫通されているか判定
isSquareUnderAttack :: Player -> Square -> Board -> Bool
isSquareUnderAttack attackerPlayer targetSq bd =
  let (mask8, knightHit) = attackingSimplexMask8 attackerPlayer targetSq bd
  in mask8 /= 0 || knightHit

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
-- 4. トポロジカル評価関数（反対称性 ＆ 詰み手数差比例）
-- ============================================================================

-- | 王の周囲8方向単体 Δ_d に沿った有界チェンバー数 b(A)（トポロジー的自由度）の計算
-- 8方向基本単体 [DirN .. DirSE] それぞれについて、盤外でなく、自駒で塞がれておらず、
-- 敵の8方向単体マスク (attackingSimplexMask8) がゼロである安全チェンバーをカウント
countEscapeChambers :: Player -> Square -> Board -> Double
countEscapeChambers plr (kx, ky) bd =
  let enemy = opponent plr
      neighbors = [ (kx + dx, ky + dy)
                  | d <- [minBound .. maxBound :: Direction8]
                  , let (dx, dy) = playerDirVector plr d ]
      validChambers = filter (\sq -> 
          not (isOutOfBounds sq) && 
          not (isFriendly sq) && 
          not (isSquareUnderAttack enemy sq bd)
        ) neighbors
  in fromIntegral (length validChambers)
  where
    isFriendly sq = case Map.lookup sq bd of
      Just (Piece p _) -> p == plr
      Nothing          -> False

-- | 持ち駒が敵玉に及ぼすトポロジー圧力（王の周囲を直接封鎖・王手できる能力）
calcHandPressure :: Hand -> Double
calcHandPressure hand =
  let weights = [ (Rook, 5.0), (Bishop, 4.0), (Gold, 3.0)
                , (Silver, 2.5), (Knight, 1.5), (Lance, 1.2), (Pawn, 0.8) ]
  in sum [ w * fromIntegral (fromMaybe 0 (Map.lookup pt hand)) | (pt, w) <- weights ]

-- | 各プレイヤーの玉に対する詰みまでのトポロジー的距離 D(K)
-- 超平面配置による有界チェンバー数、守備駒の局所結合度、敵攻め駒のモース勾配集中度、持ち駒圧力から算出
evalKingDistance :: Player -> State -> Double
evalKingDistance plr State{..} =
  let kingPos = listToMaybe [sq | (sq, Piece p pt) <- Map.toList board, p == plr, pt == King]
  in case kingPos of
    Nothing -> 0.0 -- 王が消滅（詰み・捕獲状態）
    Just kSq ->
      let enemy = opponent plr
          -- ① 脱出可能チェンバー数による自由度 (b(A))
          chambers = countEscapeChambers plr kSq board
          chamberScore = 3.0 * log (1.0 + chambers)

          -- ② 守備駒ストランドの近傍結合度
          defenders = [ sq | (sq, Piece p _) <- Map.toList board, p == plr, sq /= kSq ]
          defScore = sum [ 1.5 / (fromIntegral (manhattanDist kSq sq) + 0.5) | sq <- defenders ]

          -- ③ 敵の盤上攻め駒のモース勾配集中度
          attackers = [ sq | (sq, Piece p _) <- Map.toList board, p == enemy ]
          attScore = sum [ 2.5 / (fromIntegral (manhattanDist kSq sq) + 0.2) | sq <- attackers ]

          -- ④ 敵の持ち駒圧力
          enemyHand = fromMaybe emptyHand (Map.lookup enemy hands)
          handPressure = calcHandPressure enemyHand

          -- ⑤ 入玉による相転移（敵陣1〜3段目に入ると有界領域が無限大へ開放）
          promotionZoneBonus = if (plr == Sente && snd kSq <= 3) || (plr == Gote && snd kSq >= 7)
                               then 25.0 else 0.0

          -- 有効詰み距離（正の値にクリップ）
          effectiveDistance = chamberScore + defScore + promotionZoneBonus - (attScore + handPressure)
      in max 0.0 effectiveDistance
  where
    manhattanDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- | 局面のトポロジカル総合評価関数
-- 先後同型や平手開始局面では厳密に 0.0
-- 詰み手数の差 (D(Sente) - D(Gote)) に正比例
evaluateState :: Player -> State -> Double
evaluateState viewPlayer st =
  let distSente = evalKingDistance Sente st
      distGote  = evalKingDistance Gote st
      -- 先手視点の評価差分 (手数の差)
      senteAdvantage = (distSente - distGote) * 150.0
  in if viewPlayer == Sente then senteAdvantage else -senteAdvantage

-- ============================================================================
-- 5. 合法手生成（8方向単体チェイン C(P) に基づくスライド射線・成・二歩禁止対応）
-- ============================================================================

generateMoves :: State -> [Move]
generateMoves State{..} =
  let myPieces = Map.toList $ Map.filter (\(Piece p _) -> p == turn) board
      boardMoves = concat [ getPieceMoves sq piece | (sq, piece) <- myPieces ]
      emptySquares = [(x, y) | x <- [1..9], y <- [1..9], not (Map.member (x, y) board)]
      myHand = fromMaybe emptyHand (Map.lookup turn hands)
      -- 玉近傍および前線の重要空白マスを優先した駒打ち生成（二歩・行き所のない駒を除外）
      validDropSquares pt = filter (isValidDrop pt) (prioritizeDropSquares emptySquares)
      dropMoves = [ DropMove pt sq
                  | (pt, count) <- Map.toList myHand
                  , count > 0
                  , sq <- take 18 (validDropSquares pt) ]
  in boardMoves ++ dropMoves
  where
    dirY = if turn == Sente then -1 else 1

    getPieceMoves :: Square -> Piece -> [Move]
    getPieceMoves from@(x, y) piece@(Piece _ pt) =
      let SimplicialPiece _ chain isKnight = toSimplicialComplex piece
          stepTargets = [ tgt
                        | (d, UnitStep) <- chain
                        , let (dx, dy) = playerDirVector turn d
                        , let tgt = (x + dx, y + dy)
                        , not (isOutOfBounds tgt), not (isFriendly tgt) ]
          rayTargets  = concat
                        [ traceRay from (playerDirVector turn d)
                        | (d, InfiniteRay) <- chain ]
          knightTargets = if isKnight
                          then [ tgt
                               | dx <- [1, -1]
                               , let tgt = (x + dx, y + 2 * dirY)
                               , not (isOutOfBounds tgt), not (isFriendly tgt) ]
                          else []
          allTargets  = stepTargets ++ rayTargets ++ knightTargets
      in concatMap (makeMovesForTarget from pt) allTargets

    traceRay :: Square -> (Int, Int) -> [Square]
    traceRay (x, y) (dx, dy) = go (x + dx, y + dy)
      where
        go sq
          | isOutOfBounds sq = []
          | isFriendly sq    = []
          | Map.member sq board = [sq] -- 敵駒を捕獲して射線終了
          | otherwise        = sq : go (fst sq + dx, snd sq + dy)

    makeMovesForTarget :: Square -> PieceType -> Square -> [Move]
    makeMovesForTarget from@(_, fy) pt to@(_, ty) =
      let canPromote = pt `elem` [Pawn, Lance, Knight, Silver, Bishop, Rook]
          inZone     = inPromoZone fy || inPromoZone ty
          mustPromo  = case pt of
            Pawn   -> ty == (if turn == Sente then 1 else 9)
            Lance  -> ty == (if turn == Sente then 1 else 9)
            Knight -> if turn == Sente then ty <= 2 else ty >= 8
            _      -> False
      in if canPromote && inZone
         then if mustPromo || pt `elem` [Pawn, Bishop, Rook]
              then [NormalMove from to True]
              else [NormalMove from to True, NormalMove from to False]
         else [NormalMove from to False]

    isValidDrop :: PieceType -> Square -> Bool
    isValidDrop pt (x, y) =
      let validRank = case pt of
            Pawn   -> if turn == Sente then y >= 2 else y <= 8
            Lance  -> if turn == Sente then y >= 2 else y <= 8
            Knight -> if turn == Sente then y >= 3 else y <= 7
            _      -> True
          noNifu = pt /= Pawn || not (any (\ry -> Map.lookup (x, ry) board == Just (Piece turn Pawn)) [1..9])
      in validRank && noNifu

    prioritizeDropSquares :: [Square] -> [Square]
    prioritizeDropSquares sqs =
      let enemyKing = listToMaybe [sq | (sq, Piece p pt) <- Map.toList board, p == opponent turn, pt == King]
      in case enemyKing of
           Just (kx, ky) -> sortBy (comparing (\(x, y) -> abs (x - kx) + abs (y - ky))) sqs
           Nothing       -> sqs

    isFriendly sq = case Map.lookup sq board of
      Just (Piece p _) -> p == turn
      Nothing          -> False

    inPromoZone r = if turn == Sente then r <= 3 else r >= 7

-- ============================================================================
-- 6. 指し手の可換・非可換トポロジー分類 ＆ 単体複体商空間（Simplicial Quotient）モデル
-- ============================================================================

-- | 駒の利きの幾何学的単体（Directed 1-Simplex 境界作用素）
data SimplexType
  = DiagFront     -- 斜め前方単体 [Δ_diag-front]（Δ_NW ⊕ Δ_NE：銀、角、馬、金・成金の斜め前、竜、玉）
  | StraightFront -- 直進前方単体 [Δ_straight-front]（Δ_N：歩、香、金、銀、飛、竜、成金、馬、玉）
  | Orthogonal    -- 十字方向単体 [Δ_orth]（Δ_W ⊕ Δ_E ⊕ Δ_S：金、飛、竜、成金、馬、玉）
  | DiagBack      -- 斜め後方単体 [Δ_diag-back]（Δ_SW ⊕ Δ_SE：銀、角、馬、竜、玉）
  | KnightJump    -- 桂馬跳躍単体 [Δ_knight]（2Δ_N ± Δ_W/E：桂馬）
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | 型レベル単体タグから値レベルへの射影
class KnownSimplex (s :: SimplexType) where
  simplexVal :: proxy s -> SimplexType

instance KnownSimplex 'DiagFront     where simplexVal _ = DiagFront
instance KnownSimplex 'StraightFront where simplexVal _ = StraightFront
instance KnownSimplex 'Orthogonal    where simplexVal _ = Orthogonal
instance KnownSimplex 'DiagBack      where simplexVal _ = DiagBack
instance KnownSimplex 'KnightJump    where simplexVal _ = KnightJump

-- | 駒がどの幾何学的単体を内包しているかを表す型クラス（多重複体対応）
class HasSimplex (s :: SimplexType) a where
  hasSimplexFace   :: proxy s -> a -> Bool
  getSimplexMoves  :: proxy s -> Player -> a -> Square -> [Square]

-- | 8方向基本単体 Direction8 から 5種商単体クラス SimplexType への標準全射
directionToSimplexType :: Direction8 -> SimplexType
directionToSimplexType DirN  = StraightFront
directionToSimplexType DirNW = DiagFront
directionToSimplexType DirNE = DiagFront
directionToSimplexType DirW  = Orthogonal
directionToSimplexType DirE  = Orthogonal
directionToSimplexType DirS  = Orthogonal
directionToSimplexType DirSW = DiagBack
directionToSimplexType DirSE = DiagBack

-- | 駒種 PieceType が張る1-単体（Directed 1-simplex）のリスト（8方向チェイン C(P) からの誘導）
pieceSimplexFaces :: PieceType -> [SimplexType]
pieceSimplexFaces pt =
  let SimplicialPiece _ chain isKnight = toSimplicialComplex (Piece Sente pt)
      dirFaces = nub [ directionToSimplexType d | (d, _) <- chain ]
  in if isKnight then KnightJump : dirFaces else dirFaces

-- | 指定された単体作用素 s に沿った局所1-単体ターゲット座標列
simplexTargetsFor :: SimplexType -> Player -> Square -> [Square]
simplexTargetsFor s plr (x, y) =
  let dy = if plr == Sente then -1 else 1
      dirs = [ d | d <- [minBound .. maxBound :: Direction8], directionToSimplexType d == s ]
      raw = case s of
        KnightJump -> [(x - 1, y + 2 * dy), (x + 1, y + 2 * dy)]
        _          -> [ (x + dx, y + dy') | d <- dirs, let (dx, dy') = playerDirVector plr d ]
  in filter (not . isOutOfBounds) raw

instance KnownSimplex s => HasSimplex s PieceType where
  hasSimplexFace p pt = simplexVal p `elem` pieceSimplexFaces pt
  getSimplexMoves p plr pt sq
    | hasSimplexFace p pt = simplexTargetsFor (simplexVal p) plr sq
    | otherwise           = []

-- | 単体同値類に基づく抽象指し手（GADT表現：商複体 K / ~_dir 上の射）
data AbstractMove where
  SimplexMove :: SimplexType -> Square -> Square -> AbstractMove
  SimplexDrop :: [SimplexType] -> Square -> AbstractMove
  Dir8Action  :: Direction8  -> Square -> Square -> AbstractMove

instance Show AbstractMove where
  show (SimplexMove st from to) =
    "SimplexMove[" ++ show st ++ "] " ++ showSquare from ++ "->" ++ showSquare to
  show (SimplexDrop sts to) =
    "Drop" ++ show sts ++ "*" ++ showSquare to
  show (Dir8Action d from to) =
    "Dir8[" ++ show d ++ "] " ++ showSquare from ++ "->" ++ showSquare to

-- | 駒のコスト序列（抽象V-Pathから具象手を動的インスタンス化する際の最小コスト選択用）
pieceInstantiationCost :: PieceType -> Int
pieceInstantiationCost pt = case pt of
  Pawn    -> 1 ; PPawn   -> 1
  Lance   -> 2 ; PLance  -> 2
  Knight  -> 3 ; PKnight -> 3
  Silver  -> 4 ; PSilver -> 4
  Gold    -> 5
  Bishop  -> 6 ; PBishop -> 6
  Rook    -> 7 ; PRook   -> 7
  King    -> 99

-- | 抽象単体打ち Drop([Δ])*sq から、手持ちの駒のうち同値類 [Δ] を満たす最小コストの具象DropMoveを生成
instantiateAbstractDrop :: Hand -> [SimplexType] -> Square -> Maybe Move
instantiateAbstractDrop hand requiredFaces targetSq =
  let available = [ pt
                  | (pt, cnt) <- Map.toList hand
                  , cnt > 0
                  , all (`elem` pieceSimplexFaces pt) requiredFaces ]
      sortedByCost = sortBy (comparing pieceInstantiationCost) available
  in case sortedByCost of
       (cheapestPt:_) -> Just (DropMove cheapestPt targetSq)
       []             -> Nothing

-- | 玉近傍の局所商空間シグネチャ（8方向単体ビットマスクとZ_2軌道和による正規化）
-- (有界チェンバー数 b(A), 前方直進 Δ_N, 前方斜め Δ_NW⊕Δ_NE, 十字 Δ_W⊕Δ_E, 後方 Δ_SW⊕Δ_SE⊕Δ_S)
type KingQuotientOrbit = (Int, Int, Int, Int, Int)

computeKingQuotientOrbit :: Player -> State -> KingQuotientOrbit
computeKingQuotientOrbit plr State{..} =
  let enemy = opponent plr
      kingPos = listToMaybe [sq | (sq, Piece p pt) <- Map.toList board, p == plr, pt == King]
  in case kingPos of
       Nothing -> (0, 0, 0, 0, 0)
       Just kSq@(kx, ky) ->
         let chambers = round (countEscapeChambers plr kSq board)
             neighborSq d = let (dx, dy) = playerDirVector plr d in (kx + dx, ky + dy)
             countAttacked ds = length
               [ sq
               | d <- ds
               , let sq = neighborSq d
               , not (isOutOfBounds sq)
               , isSquareUnderAttack enemy sq board ]
             -- 8方向基本単体のZ_2反転群軌道和
             frontStraight = countAttacked [DirN]
             diagFrontOrb  = countAttacked [DirNW, DirNE]
             sideOrthOrb   = countAttacked [DirW, DirE]
             backOrb       = countAttacked [DirSW, DirSE, DirS]
         in (chambers, frontStraight, diagFrontOrb, sideOrthOrb, backOrb)

-- | 着手位置 fromSq から対象マス targetSq（敵玉等）へ到達する8方向単体ビットマスク (⋃ Δ_d)
activeSimplexMaskToward :: Player -> PieceType -> Square -> Square -> Board -> SimplexMask8
activeSimplexMaskToward plr pt fromSq targetSq bd =
  let testBd = Map.insert fromSq (Piece plr pt) bd
      (mask8, _) = attackingSimplexMask8 plr targetSq (Map.filterWithKey (\sq _ -> sq == fromSq || sq /= targetSq) testBd)
  in mask8 .&. pieceSimplexMask8 pt

-- | 商複体 K / ~_dir による候補手の8方向単体同値類縮約
-- 同一マスへの駒打ちで、敵玉（およびその近傍チェンバー）に対して同一の8方向単体ビットマスク (⋃ Δ_d) として
-- 作用する複数の駒種がある場合、最小コストの代表元（歩 < 香 < 桂 < 銀 < 金 < 角 < 飛）に束ねて分岐爆発を圧縮する
compressBySimplexQuotient :: State -> [Move] -> [Move]
compressBySimplexQuotient st moves =
  let opp = opponent (turn st)
      oppKing = listToMaybe [sq | (sq, Piece p pt) <- Map.toList (board st), p == opp, pt == King]
  in case oppKing of
       Nothing -> moves
       Just kSq ->
         let (dropMoves, normalMoves) = partition isDrop moves
             sortedDrops = sortBy (comparing dropCostOrder) dropMoves
             compressedDrops = selectMinimalDropRepresentatives kSq sortedDrops []
         in normalMoves ++ compressedDrops
  where
    dropCostOrder (DropMove pt to) = (to, pieceInstantiationCost pt)
    dropCostOrder m                = ((0, 0), 99 + length (show m))

    selectMinimalDropRepresentatives _ [] acc = reverse acc
    selectMinimalDropRepresentatives kSq@(kx, ky) (m@(DropMove pt to@(tx, ty)):ms) acc =
      let plr = turn st
          opp = opponent plr
          chebyshev = max (abs (tx - kx)) (abs (ty - ky))
          -- 敵玉に対して張る8方向単体ビットマスク (⋃ Δ_d) とチェンバー封鎖数の同値類シグネチャ
          kingMask8 = activeSimplexMaskToward plr pt to kSq (board st)
          chamberCountAfter = countEscapeChambers opp kSq (board (applyMove m st))
          isRedundantInQuotient = any (\rep -> case rep of
            DropMove repPt repTo ->
              repTo == to &&
              pieceInstantiationCost repPt <= pieceInstantiationCost pt &&
              (chebyshev > 2 ||
               (activeSimplexMaskToward plr repPt repTo kSq (board st) == kingMask8 &&
                countEscapeChambers opp kSq (board (applyMove rep st)) == chamberCountAfter))
            _ -> False) acc
      in if isRedundantInQuotient
         then selectMinimalDropRepresentatives kSq ms acc
         else selectMinimalDropRepresentatives kSq ms (m : acc)
    selectMinimalDropRepresentatives kSq (_:ms) acc =
      selectMinimalDropRepresentatives kSq ms acc

-- | 指し手のトポロジカル4分類（ブレイド群・半群構造）
data TopologicalMoveClass
  = SingularNonCommutative     -- 特異非可換手（駒取り・駒打ち・王の有界チェンバー切断）
  | IrreversibleNonCommutative -- 不可逆な非可換手（歩・香・桂・銀の前進、成り：Positive Braid Monoid）
  | ReversibleNonCommutative   -- 可逆な非可換手（金・成金の斜め移動、飛・角・玉の交差移動）
  | CommutativeMove            -- 可換な手（互いに台 Support が交差しない独立平行ストランド）
  deriving (Eq, Ord, Show)

-- | 駒取り判定（次元の縮退）
isCapture :: Board -> Move -> Bool
isCapture bd (NormalMove _ to _) = Map.member to bd
isCapture _  (DropMove _ _)      = False

-- | 駒打ち判定（新たな超平面の挿入）
isDrop :: Move -> Bool
isDrop (DropMove _ _) = True
isDrop _              = False

-- | 歩・香・桂・銀の不可逆な前進および成りの判定（Positive Braid Monoid 元）
isIrreversibleAdvance :: Player -> Board -> Move -> Bool
isIrreversibleAdvance _   _  (NormalMove _ _ True) = True -- 成りは常に不可逆
isIrreversibleAdvance plr bd (NormalMove from (_, ty) False) =
  let dy = ty - snd from
      forward = if plr == Sente then dy < 0 else dy > 0
  in case Map.lookup from bd of
       Just (Piece _ pt) ->
         pt `elem` [Pawn, Lance, Knight] || (pt == Silver && forward)
       Nothing -> False
isIrreversibleAdvance _ _ (DropMove _ _) = True

-- | 敵玉または自玉の有界チェンバー数 b(A) を直接変化（切断・防衛）させるか判定
affectsKingChamber :: Player -> Board -> Move -> Bool
affectsKingChamber plr bd m =
  let enemy = opponent plr
      kings = [sq | (sq, Piece _ pt) <- Map.toList bd, pt == King]
      targetSq = case m of
        NormalMove _ to _ -> to
        DropMove _ to     -> to
      nearAnyKing = any (\(kx, ky) -> max (abs (fst targetSq - kx)) (abs (snd targetSq - ky)) <= 2) kings
      givesCheck  = isKingUnderAttack enemy (Map.insert targetSq (Piece plr Gold) bd)
  in nearAnyKing || givesCheck

-- | 指し手の台（Support：始点・終点および近傍干渉領域）の交差判定
-- 2つの指し手 m1, m2 が空間的に独立（遠乗法的 σ_i σ_j = σ_j σ_i）であるかを判定
areMovesCommutative :: Move -> Move -> Bool
areMovesCommutative m1 m2 =
  let sup1 = moveSupport m1
      sup2 = moveSupport m2
  in all (\(x1, y1) -> all (\(x2, y2) -> max (abs (x1 - x2)) (abs (y1 - y2)) >= 2) sup2) sup1
  where
    moveSupport (NormalMove from to _) = [from, to]
    moveSupport (DropMove _ to)        = [to]

-- | 指し手のトポロジカル分類関数
classifyMove :: State -> Move -> TopologicalMoveClass
classifyMove st m
  | isCapture (board st) m || isDrop m || affectsKingChamber (turn st) (board st) m =
      SingularNonCommutative
  | isIrreversibleAdvance (turn st) (board st) m =
      IrreversibleNonCommutative
  | isDiagonalOrCrossInterference (board st) m =
      ReversibleNonCommutative
  | otherwise =
      CommutativeMove
  where
    isDiagonalOrCrossInterference bd (NormalMove (fx, fy) (tx, ty) _) =
      (abs (tx - fx) == 1 && abs (ty - fy) == 1) ||
      case Map.lookup (fx, fy) bd of
        Just (Piece _ pt) -> pt `elem` [Rook, PRook, Bishop, PBishop]
        Nothing           -> False
    isDiagonalOrCrossInterference _ _ = False

-- | 干渉グラフ G = (V, E) の連結成分による可換手のトポロジカル直交分解
-- 互いに台が交差しない独立サブゲーム成分ごとに Garside 左正規形の代表元を抽出
canonicalizeCommutativeMoves :: State -> [Move] -> [Move]
canonicalizeCommutativeMoves st moves =
  let -- 1. 直前の自手番の可換手 m_prev がある場合、m_prev と可換かつ辞書順で逆転 (m < m_prev) する手順前後を排除
      prevComm = joinMaybe (Map.lookup (turn st) (lastCommMove st))
      garsideFiltered = case prevComm of
        Just mp -> filter (\m -> not (areMovesCommutative mp m && m < mp)) moves
        Nothing -> moves
      sortedLex = sort garsideFiltered
  in selectCanonicalSpine sortedLex []
  where
    joinMaybe (Just x) = x
    joinMaybe Nothing  = Nothing

    -- 互いに可換（遠乗法的: |x1 - x2| >= 2）かつ勾配変化が同等の独立ストランドをGarside代表元へ縮約
    selectCanonicalSpine [] acc = reverse acc
    selectCanonicalSpine (m:ms) acc =
      let nextSt = applyMove m st
          delta  = evaluateState (turn st) nextSt - evaluateState (turn st) st
          isRedundantCommutative =
            any (\rep -> areMovesCommutative m rep &&
                         abs (delta - (evaluateState (turn st) (applyMove rep st) - evaluateState (turn st) st)) < 15.0) acc
      in if isRedundantCommutative
         then selectCanonicalSpine ms acc
         else selectCanonicalSpine ms (m : acc)

-- | 敵玉の有界チェンバー減少量 Δb(A_King) の計測（超平面切断の曲率）
evalKingChamberDelta :: State -> Move -> Double
evalKingChamberDelta st m =
  let opp = opponent (turn st)
      curBd = board st
      nextBd = board (applyMove m st)
      oppKingCur  = listToMaybe [sq | (sq, Piece p pt) <- Map.toList curBd,  p == opp, pt == King]
      oppKingNext = listToMaybe [sq | (sq, Piece p pt) <- Map.toList nextBd, p == opp, pt == King]
      chamberReduction = case (oppKingCur, oppKingNext) of
        (Just k1, Just k2) -> countEscapeChambers opp k1 curBd - countEscapeChambers opp k2 nextBd
        _                  -> 0.0
      checkBonus = if isKingUnderAttack opp nextBd then 3.0 else 0.0
  in chamberReduction * 40.0 + checkBonus * 50.0

-- | トポロジカル可換性・非可換性 ＆ 単体複体商空間 (K / ~_dir) を利用した手の順序付けと枝刈り
pruneAndOrderMoves :: State -> [Move] -> [Move]
pruneAndOrderMoves st moves =
  let -- 0. 商複体 K / ~_dir による単体同値類縮約（同一単体作用を持つ駒打ち・アプローチを最小コスト代表元へ集約）
      quotientCompressed = compressBySimplexQuotient st moves

      -- 1. 合法手を「干渉クラスタ（非可換手）」と「独立手（可換手）」に分類
      (nonCommutativeMoves, commutativeMoves) = partition (isInterferingWithCriticalZone st) quotientCompressed

      -- 2. 非可換手（王への超平面切断、駒取り、急所前進）:
      --    モースポテンシャルの落差が大きい順（最急降下）にソート
      sortedCritical = sortBy (comparing (negate . evalMorseImpact st)) nonCommutativeMoves

      -- 3. 可換手（互いに独立な手）:
      --    Garside左正規形に従い、辞書式順序で代表元のみを抽出（手順前後の重複展開を排除）
      canonicalCommutative = canonicalizeCommutativeMoves st commutativeMoves
  in
      -- 非可換な勝負手を最優先で深く探索し、可換手は縮約して並行処理
      sortedCritical ++ canonicalCommutative
  where
    -- 敵玉近傍の超平面配置に干渉するか、または駒取り・駒打ち・主戦線不可逆前進であるかを判定
    isInterferingWithCriticalZone s m =
      isCapture (board s) m || isDrop m || affectsKingChamber (turn s) (board s) m || isCriticalIrreversible s m

    isCriticalIrreversible s mv@(NormalMove (fx, _) (_, ty) promo) =
      promo ||
      (isIrreversibleAdvance (turn s) (board s) mv &&
       (fx `elem` [2, 5, 7, 8] || (if turn s == Sente then ty <= 5 else ty >= 5)))
    isCriticalIrreversible _ _ = True

    -- モースポテンシャルの変化率（勾配の大きさ）＋超平面切断曲率 Δb(A)
    evalMorseImpact s m =
      let nextSt = applyMove m s
          baseGradient = evaluateState (turn s) nextSt - evaluateState (turn s) s
          chamberDelta = evalKingChamberDelta s m
          classBonus = case classifyMove s m of
            SingularNonCommutative     -> 35.0
            IrreversibleNonCommutative -> 15.0
            ReversibleNonCommutative   -> 5.0
            CommutativeMove            -> 0.0
      in baseGradient + chamberDelta + classBonus

-- ============================================================================
-- 7. 千日手判定（トポロジー無限ループの自動検出）
-- ============================================================================

-- | 同一局面が 4 回以上発生したかどうかを判定
isSennichite :: PositionKey -> Map.Map PositionKey Int -> Bool
isSennichite key hist =
  case Map.lookup key hist of
    Just count -> count >= 4
    Nothing    -> False

-- ============================================================================
-- 8. トポロジカル V-path α-β 探索 ＆ Garside標準化置換表
-- ============================================================================

-- | 非可換最急降下とGarside可換縮約を統合したマルチプライ探索
searchTopologicalAlphaBeta :: Int -> Double -> Double -> State -> (Double, [Move])
searchTopologicalAlphaBeta 0 _ _ st =
  (evaluateState (turn st) st, [])
searchTopologicalAlphaBeta depth alpha beta st =
  let rawMoves = generateMoves st
      f5Moves  = f5SyzygyFilter st rawMoves
      (_, safeMoves) = partition (\m ->
        let nextSt = applyMove m st
        in isSennichite (positionKey nextSt) (history nextSt)
        ) f5Moves
  in if null f5Moves
     then (-15000.0 - fromIntegral depth * 100.0, []) -- 詰み（最短手数の詰みを優先）
     else if null safeMoves
       then (0.0, []) -- 千日手引き分け
       else
         let ordered = pruneAndOrderMoves st safeMoves
             -- 深さに応じて非可換急所手＋Garside正規形代表元に探索幅を集中
             beamWidth = if depth >= 3 then 8 else 12
             candidates = take beamWidth ordered
         in loop candidates alpha []
  where
    loop [] curAlpha bestPv = (curAlpha, bestPv)
    loop (m:ms) curAlpha bestPv =
      let nextSt = applyMove m st
          (childScore, childPv) = searchTopologicalAlphaBeta (depth - 1) (-beta) (-curAlpha) nextSt
          score = -childScore
      in if score >= beta
         then (score, m : childPv) -- βカット（非可換急所手による早期枝刈り）
         else if score > curAlpha
              then loop ms score (m : childPv)
              else loop ms curAlpha (if null bestPv then (m : childPv) else bestPv)

searchBestMoveWithCache :: State -> (Either String (Move, Double, [Move]), State)
searchBestMoveWithCache state =
  let key = positionKey state
  in case Map.lookup key (cache state) of
    Just (score, mv) ->
      -- ★ Garsideキャッシュヒット：ただし、過去の最善手を指した結果が「千日手」を引き起こさないか検証
      let nextState = applyMove mv state
      in if isSennichite (positionKey nextState) (history nextState)
         then evaluateAndFilterNoCache state
         else (Right (mv, score, [mv]), state)
    Nothing ->
      evaluateAndFilterNoCache state

  where
    evaluateAndFilterNoCache :: State -> (Either String (Move, Double, [Move]), State)
    evaluateAndFilterNoCache st =
      let moves = generateMoves st
          f5FilteredMoves = f5SyzygyFilter st moves
          (_, safeMoves) = 
            partition (\m -> 
              let nextState = applyMove m st
              in isSennichite (positionKey nextState) (history nextState)
            ) f5FilteredMoves
      in if null f5FilteredMoves
         then (Left "resign", st)
         else if null safeMoves
           then (Left "draw", st)
           else
             let searchDepth = 3
                 (bestScore, pv) = searchTopologicalAlphaBeta searchDepth (-99999.0) 99999.0 st
                 fallbackMove = case pruneAndOrderMoves st safeMoves of
                   (m:_) -> m
                   []    -> NormalMove (5, 9) (5, 8) False
                 bestMove = case pv of
                   (m:_) | m `elem` safeMoves -> m
                   _                          -> fallbackMove
                 newCache = Map.insert (positionKey st) (bestScore, bestMove) (cache st)
             in (Right (bestMove, bestScore, if null pv then [bestMove] else pv), st { cache = newCache })

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
  eof <- isEOF
  if eof
    then exitSuccess
    else do
      line <- getLine
      let tokens = words line
      case tokens of
        [] -> usiLoop currentState
        (cmd:rest) -> case cmd of
          "usi" -> do
            putStrLn "id name ShogiMorseEngine_Haskell_v5_TopologicalDistance"
            putStrLn "id author Shogi_Topology_Project"
            putStrLn "usiok"
            usiLoop currentState
          
          "isready" -> do
            putStrLn "readyok"
            usiLoop currentState
          
          "usinewgame" -> do
            usiLoop initState { cache = Map.empty }
          
          "position" -> do
            let (posType, movesTokens) = 
                  case rest of
                    ("startpos":"moves":mvs) -> (initState, mvs)
                    ("startpos":mvs)         -> (initState, mvs)
                    _                        -> (initState, rest)
                finalState = foldl' (\st mvStr -> applyMove (parseMove mvStr) st) posType movesTokens
            usiLoop finalState { cache = cache currentState }
          
          "go" -> do
            let (res, nextState) = searchBestMoveWithCache currentState
            case res of
              Left "draw" -> do
                putStrLn "bestmove draw"
                usiLoop nextState
              Left _ -> do
                putStrLn "bestmove resign"
                usiLoop nextState
              Right (best, score, pv) -> do
                putStrLn $ "info depth 3 score cp " ++ show (round score :: Int) ++ " pv " ++ unwords (map showMove pv)
                putStrLn $ "bestmove " ++ showMove best
                usiLoop nextState

          "eval" -> do
            let dS = evalKingDistance Sente currentState
                dG = evalKingDistance Gote  currentState
                ev = evaluateState (turn currentState) currentState
                orbS = computeKingQuotientOrbit Sente currentState
                orbG = computeKingQuotientOrbit Gote  currentState
                rawMvs = f5SyzygyFilter currentState (generateMoves currentState)
                quotMvs = compressBySimplexQuotient currentState rawMvs
                prunedMvs = pruneAndOrderMoves currentState rawMvs
            putStrLn $ "info string D(Sente)=" ++ show dS
                    ++ " D(Gote)=" ++ show dG
                    ++ " eval=" ++ show ev
                    ++ " orbitS=" ++ show orbS
                    ++ " orbitG=" ++ show orbG
                    ++ " rawMoves=" ++ show (length rawMvs)
                    ++ " quotientMoves=" ++ show (length quotMvs)
                    ++ " garsidePrunedMoves=" ++ show (length prunedMvs)
            usiLoop currentState
          
          "quit" -> do
            exitSuccess
          
          _ -> usiLoop currentState
