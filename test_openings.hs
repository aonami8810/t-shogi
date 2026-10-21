{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import System.Exit
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map

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

data State = State
  { turn  :: Player
  , board :: Board
  , hands :: Map.Map Player Hand
  } deriving (Show)

data Move
  = NormalMove Square Square Bool
  | DropMove PieceType Square
  deriving (Eq, Show)

emptyHand :: Hand
emptyHand = Map.fromList
  [ (Pawn, 0), (Lance, 0), (Knight, 0), (Silver, 0)
  , (Gold, 0), (Bishop, 0), (Rook, 0)
  ]

initState :: State
initState =
  let initialBoard = Map.fromList
        [ ((1, 1), Piece Gote Lance),  ((2, 1), Piece Gote Knight), ((3, 1), Piece Gote Silver), ((4, 1), Piece Gote Gold)
        , ((5, 1), Piece Gote King),   ((6, 1), Piece Gote Gold),   ((7, 1), Piece Gote Silver), ((8, 1), Piece Gote Knight), ((9, 1), Piece Gote Lance)
        , ((8, 2), Piece Gote Rook),   ((2, 2), Piece Gote Bishop)
        , ((1, 3), Piece Gote Pawn),   ((2, 3), Piece Gote Pawn),   ((3, 3), Piece Gote Pawn),   ((4, 3), Piece Gote Pawn)
        , ((5, 3), Piece Gote Pawn),   ((6, 3), Piece Gote Pawn),   ((7, 3), Piece Gote Pawn),   ((8, 3), Piece Gote Pawn),   ((9, 3), Piece Gote Pawn)
        , ((1, 7), Piece Sente Pawn),  ((2, 7), Piece Sente Pawn),  ((3, 7), Piece Sente Pawn),  ((4, 7), Piece Sente Pawn)
        , ((5, 7), Piece Sente Pawn),  ((6, 7), Piece Sente Pawn),  ((7, 7), Piece Sente Pawn),  ((8, 7), Piece Sente Pawn),  ((9, 7), Piece Sente Pawn)
        , ((2, 8), Piece Sente Rook),  ((8, 8), Piece Sente Bishop)
        , ((1, 9), Piece Sente Lance), ((2, 9), Piece Sente Knight),((3, 9), Piece Sente Silver),((4, 9), Piece Sente Gold)
        , ((5, 9), Piece Sente King),  ((6, 9), Piece Sente Gold),  ((7, 9), Piece Sente Silver),((8, 9), Piece Sente Knight),((9, 9), Piece Sente Lance)
        ]
      initialHands = Map.fromList [(Sente, emptyHand), (Gote, emptyHand)]
  in State Sente initialBoard initialHands

parseSquare :: String -> Square
parseSquare [c, r] = (digitToInt c, ord r - ord 'a' + 1)
parseSquare _      = (1, 1)

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
  in State (opponent turn) newBoard newHands
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
  in State (opponent turn) newBoard newHands

testSequence :: String -> [String] -> IO ()
testSequence name moves = do
  putStrLn $ "Testing sequence: " ++ name ++ " (" ++ show (length moves) ++ " moves)"
  let step (st, idx) mvStr =
        let mv = parseMove mvStr
            nxt = applyMove mv st
        in (nxt, idx + 1)
      (finalSt, total) = foldl' step (initState, 0) moves
  putStrLn $ "  Success! Final turn: " ++ show (turn finalSt) ++ ", moves: " ++ show total

main :: IO ()
main = do
  putStrLn "Checking Sequences..."
  testSequence "Aigakari 1"
    ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3i3h", "7a7b", "5i6h", "5a4b", "4i5h", "4b3c", "6h7h", "7b6c", "3h2g", "8b8d", "2h2f", "8d7d", "2f2e", "7d8d", "2e2f", "8d7d"]

  testSequence "Kakugawari 1"
    ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "8h2b+", "3a2b", "7i8h", "7a6b", "3i4h", "6b7b", "4h3g", "4a3b", "6i7h", "5a4b", "5i6h", "6c6d", "3g4f", "7c7d", "4f3e", "3c3d", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h"]

  testSequence "Yagura 1"
    ["7g7f", "8c8d", "6g6f", "3c3d", "7i6h", "6c6d", "6h7g", "7a6b", "5g5f", "5a4b", "4i5h", "3a3b", "5i6h", "4b3c", "6h7h", "5c5d", "7h8h", "4c4d", "8h7h", "8d8e", "7h8h", "6b5c", "2g2f", "7c7d", "2f2e", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "4g4f", "3b4c", "3i4h", "4c3b", "4h4g"]

  testSequence "Shikenbisha 1"
    ["7g7f", "3c3d", "6g6f", "8c8d", "2h6h", "8d8e", "8h7g", "6b6c", "4i4h", "7a6b", "3i3h", "4a3b", "5i4h", "5a4b", "4h3h", "3a2b", "3h2h", "7b7a", "2h1h", "7a8b", "6h7h", "8b7b", "1h2h", "1c1d", "1g1f", "5c5d", "2g2f", "4b3a", "2f2e", "2b3c", "3g3f"]

  testSequence "Nakabisha 1"
    ["5g5f", "3c3d", "5h5e", "8c8d", "5e5f", "8d8e", "7g7f", "6b6c", "7i6h", "4a3b", "4i5h", "5a4b", "3i4h", "3a2b", "5i4h", "4b3a", "4h3h", "7a6b", "3h2h", "6b7c", "2g2f", "7c8d", "2f2e", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3g3f", "8d7c"]

  testSequence "Yokofudori 1"
    ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d", "2h2d", "8e8f", "8g8f", "8b8f", "2d3d", "8f8h+", "7i8h", "B*4e", "3d3f", "4e2g+", "3f3d", "+2g1g", "3d3f", "1g1f", "3f3e", "1f2f", "B*3c", "2f2e"]

