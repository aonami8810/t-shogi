{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import Data.List
import Data.Maybe
import qualified Data.Map as Map

-- ============================================================================
-- 1. 基本データ構造の定義
-- ============================================================================

data Player = Sente | Gote
  deriving (Eq, Show, Enum, Bounded, Ord)

opponent :: Player -> Player
opponent Sente = Gote
opponent Gote  = Sente

type Square = (Int, Int)
type Board = Map.Map Square Piece

data PieceType 
  = Pawn | Lance | Knight | Silver | Gold | Bishop | Rook | King
  | PPawn | PLance | PKnight | PSilver | PBishop | PRook
  deriving (Eq, Show, Enum, Bounded, Ord)

data Piece = Piece Player PieceType
  deriving (Eq, Ord, Show)

-- 指し手表現
data Move 
  = NormalMove Square Square Bool
  | DropMove PieceType Square
  deriving (Eq, Show, Ord)

-- ============================================================================
-- 2. モースポテンシャルと「曲率（Curvature / Hessian近似）」の定義
-- ============================================================================

-- | 局面の簡易的なモースポテンシャル (王の危険度評価)
-- 0.0 (絶対安全) 〜 100.0 (破局的危険)
evalMorsePotential :: Player -> Board -> Double
evalMorsePotential player bd =
  let kingPos = findKing player bd
  in case kingPos of
    Nothing -> 100.0 -- 王がいない (死)
    Just (kx, ky) ->
      let enemyPieces = Map.toList $ Map.filter (\(Piece p _) -> p /= player) bd
          -- 敵駒からの幾何学的近接ポテンシャル
          influenceSum = sum [ 10.0 / (fromIntegral (dist (kx, ky) sq) + 0.1) | (sq, _) <- enemyPieces ]
          -- 自玉周囲の閉塞度 (有界領域不変量)
          surroundings = [(kx + dx, ky + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]
          wallPenalties = sum [ if outOfBounds sq || Map.member sq bd then 0.0 else 1.5 | sq <- surroundings ]
      in min 100.0 (influenceSum + wallPenalties)
  where
    findKing plr b = listToMaybe [sq | (sq, Piece p pt) <- Map.toList b, p == plr, pt == King]
    dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
    outOfBounds (x, y) = x < 1 || x > 9 || y < 1 || y > 9

-- | 局面の総合ポテンシャル（形勢）： (自玉の危険度) - (敵玉の危険度)
-- 符号が0から離れるほど、どちらかに形勢が偏っていることを示す
statePotential :: Player -> Board -> Double
statePotential player bd =
  let myDanger = evalMorsePotential player bd
      enemyDanger = evalMorsePotential (opponent player) bd
  in myDanger - enemyDanger

-- | 局所的「モース曲率」（Morse Curvature / Hessian二階差分近似）
-- 現在の局面から1手遷移可能なすべての局面（1-Morphisms）に対する、ポテンシャル変化の「標準偏差（あるいは最大階差）」
-- これが極大となる場所こそが、サドルノード分岐（形勢の崩壊）が潜む「サドル点周辺」である。
calculateCurvature :: Player -> Board -> [Board] -> Double
calculateCurvature player currentBoard nextBoards
  | null nextBoards = 0.0
  | otherwise =
      let currentV = statePotential player currentBoard
          nextVs    = map (statePotential player) nextBoards
          -- 各遷移への一階差分 (Gradient)
          gradients = map (\v -> v - currentV) nextVs
          -- 差分の平均値
          avgGrad   = sum gradients / fromIntegral (length gradients)
          -- 差分のばらつき（＝二階微分の絶対値、曲率の近似）
          variance  = sum [ (g - avgGrad) ^ 2 | g <- gradients ] / fromIntegral (length gradients)
      in sqrt variance -- 局所曲率（標準偏差）

-- ============================================================================
-- 3. モース勾配適応型アクティブ・ラーニング (Morse Adaptive Sampling)
-- ============================================================================

-- | アクティブ・ラーニングの探索統計量
data SearchNodeStats = SearchNodeStats
  { visitCount :: Int      -- 訪問回数 N(s)
  , curvature  :: Double   -- 局所曲率 κ(s) = ||∇² V||
  , lastScore  :: Double   -- 直近の評価スコア
  } deriving (Show)

-- 定跡学習データベース： 盤面(Garside正規形) -> 探索統計
type ActiveLearningBook = Map.Map Board SearchNodeStats

-- | 局面 s における適応的サンプリング重要度重み W(s) の算出
-- W(s) = f(κ(s)) * (1 / sqrt(N(s) + 1))
-- 曲率（ポテンシャルの傾きの変化の激しさ）が大きい勝負所（サドル点周辺）ほど重点的にサンプリングし、
-- すでに多く探索された（N(s) が大きい）局面はウェイトを下げる（UCBとは異なり、曲率主導の重点探索を行う）。
computeSamplingWeight :: Double -> SearchNodeStats -> Double
computeSamplingWeight baseBeta SearchNodeStats{..} =
  let explorationBonus = 1.0 / (sqrt (fromIntegral visitCount + 1.0))
      -- 曲率 κ(s) が高いエリアほどサンプリング確率を指数関数的に高める（感度調整パラメータ beta）
      curvatureFactor  = exp (baseBeta * curvature)
  in curvatureFactor * explorationBonus

-- | 候補遷移群（指し手と適用後の盤面）から、適応型サンプリング確率分布に基づいて1手を選択する（重点サンプリング）
-- 擬似乱数 (シード値) を用いた重み付きサンプリング（純粋関数実装）
selectMoveAdaptively :: Double -> ActiveLearningBook -> Board -> [(Move, Board)] -> Int -> (Move, Board)
selectMoveAdaptively beta learningBook currentBoard candidates seed
  | null candidates = error "No legal moves to sample."
  | otherwise =
      let -- 各候補盤面の統計情報を参照し、重みを計算
          weights = map (\(_, b) -> 
                          let stats = Map.findWithDefault (SearchNodeStats 0 0.5 0.0) b learningBook
                          in computeSamplingWeight beta stats
                        ) candidates
          totalWeight = sum weights
          -- 乱数シードから [0, totalWeight) のランダムな閾値を生成
          randVal = (fromIntegral (hash seed) / fromIntegral (maxBound :: Int)) * totalWeight
          -- 累積重みから選択
          selectedIdx = pickWeighted randVal 0 (zip candidates weights)
      in candidates !! selectedIdx
  where
    pickWeighted :: Double -> Double -> [((Move, Board), Double)] -> Int
    pickWeighted rv acc [] = 0
    pickWeighted rv acc [_] = 0
    pickWeighted rv acc (((_, _), w):xs)
      | rv < acc + w = 0
      | otherwise    = 1 + pickWeighted rv (acc + w) xs

    -- 簡易ハッシュ関数
    hash :: Int -> Int
    hash x = let x' = (x `pyXor` (x `pyShiftL` 13)) `pyAnd` 0x0FFFFFFF
                 x'' = (x' `pyXor` (x' `pyShiftR` 17)) `pyAnd` 0x0FFFFFFF
             in (x'' `pyXor` (x'' `pyShiftL` 5)) `pyAnd` 0x0FFFFFFF
      where
        pyXor a b = a `import_xor` b
        pyShiftL a b = a * (2^b)
        pyShiftR a b = a `div` (2^b)
        pyAnd a b = a `import_and` b -- 簡易ビット演算のエミュレート（正の整数のみ）
        import_xor a b = abs (a - b) -- 簡易シード変化
        import_and a b = a `mod` b

-- ============================================================================
-- 4. 学習ループ・シミュレーション
-- ============================================================================

-- | 自己対局を通じてアクティブラーニングデータベースを1ステップ更新する
updateLearningBook :: Player -> ActiveLearningBook -> Board -> [Board] -> ActiveLearningBook
updateLearningBook player book currentBoard nextBoards =
  let -- 1. 曲率の算出
      curv = calculateCurvature player currentBoard nextBoards
      -- 2. 現在盤面の既存統計の取得
      stats = Map.findWithDefault (SearchNodeStats 0 0.0 0.0) currentBoard book
      -- 3. 統計の更新 (訪問回数のインクリメント、算出した曲率の保存)
      newStats = stats
        { visitCount = visitCount stats + 1
        , curvature  = curv
        , lastScore  = statePotential player currentBoard
        }
  in Map.insert currentBoard newStats book

-- ============================================================================
-- 5. モデリング・検証用のメインエントリー
-- ============================================================================

main :: IO ()
main = do
  putStrLn "======================================================================="
  putStrLn " Morse Adaptive Sampling (Active Learning Model in Shogi) Simulation"
  putStrLn "======================================================================="
  
  -- 1. テスト用の擬似盤面の構築（平手初期状態を模したもの）
  let testBoard = Map.empty -- 簡易空盤面
      -- 1手先の探索候補盤面（3つの分岐：一方は安全な谷、他方は危険なサドル点近傍への道）
      -- 仮想的に、王の位置を動かして異なるポテンシャル変化を模倣
      boardA = Map.fromList [((5,9), Piece Sente King), ((8,2), Piece Gote Rook)] -- 平穏な遷移
      boardB = Map.fromList [((5,9), Piece Sente King), ((5,8), Piece Gote Rook)] -- 王手急襲（曲率が極めて高くなる分岐）
      boardC = Map.fromList [((6,9), Piece Sente King), ((8,2), Piece Gote Rook)] -- 局所回避

      candidates = [ (NormalMove (5,9) (5,9) False, boardA)
                   , (NormalMove (5,9) (5,8) False, boardB)
                   , (NormalMove (5,9) (6,9) False, boardC)
                   ]
      nextBoards = map snd candidates

  -- 2. 初期状態のアクティブ・ラーニング・定跡本の作成
  let initialBook = Map.empty
      -- 3つの候補局面に対する局所曲率 (Hessian) を計算し、定跡本を更新
      updatedBook = updateLearningBook Sente initialBook testBoard nextBoards

  -- 3. 計算された曲率とサンプリング重みの表示
  let statsSelf = Map.findWithDefault (SearchNodeStats 0 0.0 0.0) testBoard updatedBook
  putStrLn $ "Calculated Local Morse Curvature (||∇² V||): " ++ show (curvature statsSelf)
  
  putStrLn "\n--- Simulating sampling weights for next board choices ---"
  let statsA = SearchNodeStats { visitCount = 10, curvature = 0.1, lastScore = -2.0 }  -- 平穏な局面
      statsB = SearchNodeStats { visitCount = 2,  curvature = 4.8, lastScore = 15.5 }  -- 曲率が激しいサドル点周辺
      statsC = SearchNodeStats { visitCount = 1,  curvature = 1.2, lastScore = 1.0 }   -- まだ探索が浅い

      weightA = computeSamplingWeight 0.5 statsA
      weightB = computeSamplingWeight 0.5 statsB
      weightC = computeSamplingWeight 0.5 statsC

  putStrLn $ "Board A (Stable Basin, visited 10 times): Curvature = 0.1, Weight = " ++ show weightA
  putStrLn $ "Board B (Saddle Point, visited  2 times): Curvature = 4.8, Weight = " ++ show weightB ++ "  <-- [TARGETED]"
  putStrLn $ "Board C (Unexplored,   visited  1 times): Curvature = 1.2, Weight = " ++ show weightC

  putStrLn "\n--- Running Importance Sampling Selector ---"
  -- シード値を変えて重要度サンプリングを実行
  let mockBook = Map.fromList [(boardA, statsA), (boardB, statsB), (boardC, statsC)]
      selection1 = selectMoveAdaptively 0.5 mockBook testBoard candidates 12345
      selection2 = selectMoveAdaptively 0.5 mockBook testBoard candidates 67890
      selection3 = selectMoveAdaptively 0.5 mockBook testBoard candidates 54321
  
  putStrLn $ "Selection with seed 12345: " ++ show (fst selection1)
  putStrLn $ "Selection with seed 67890: " ++ show (fst selection2)
  putStrLn $ "Selection with seed 54321: " ++ show (fst selection3)
  putStrLn "======================================================================="
