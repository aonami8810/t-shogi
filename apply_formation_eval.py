with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

def find_line(pat, start=0):
    for i in range(start, len(lines)):
        if pat in lines[i]:
            return i
    return -1

# 1. Insert evalSilverAdvanceAndIshidaFormation before evalSilverAdvanceAndIshidaFlow
idx_flow = find_line('evalSilverAdvanceAndIshidaFlow :: State -> Move -> Double')
print(f"evalSilverAdvanceAndIshidaFlow at line {idx_flow + 1}")

formation_func = """-- | 銀の前線進出（棒銀・腰掛け銀・早繰り銀）および石田流本組の局面評価（モース勾配流）
-- 局面の静的評価に直接反映することで、飛車のうろうろ手損を完全排除し、
-- 銀の前線繰り出しと石田流本組の空中要塞構築を確実に選択させる
evalSilverAdvanceAndIshidaFormation :: Player -> State -> Double
evalSilverAdvanceAndIshidaFormation p st =
  let bd = board st
      -- 1. 銀の前進・前線配置（棒銀・腰掛け銀・早繰り銀・中央厚み）
      silvers = [ sq | (sq, Piece owner pt) <- Map.toList bd, owner == p, pt `elem` [Silver, PSilver] ]
      silverScore = sum
        [ let (x, y) = sq
              adv = if p == Sente then 9 - y else y - 1
              -- 棒銀 (Climbing Silver: 先手2筋・1筋, 後手8筋・9筋)
              isClimbing = if p == Sente
                           then (x == 2 || x == 1) && y <= 6
                           else (x == 8 || x == 9) && y >= 4
              -- 腰掛け銀・中央厚み (Reclining / Central: 4,5,6筋の中段)
              isCentral = if p == Sente
                          then sq `elem` [(5, 6), (4, 6), (6, 6), (5, 5), (4, 5)]
                          else sq `elem` [(5, 4), (6, 4), (4, 4), (5, 5), (6, 5)]
              -- 前線進出 (Frontline: 5段目以上)
              isFront = if p == Sente then y <= 5 else y >= 5
          in (if isClimbing then 600.0 else 0.0) +
             (if isCentral  then 500.0 else 0.0) +
             (if isFront    then 350.0 else (if adv >= 2 then fromIntegral adv * 80.0 else 0.0))
        | sq <- silvers
        ]
      
      -- 2. 石田流本組・三間飛車のモース勾配流（浮き飛車・7五歩・7七桂・9七角）
      ishidaScore =
        if p == Sente
        then
          let hasSente75P = Map.lookup (7, 5) bd == Just (Piece Sente Pawn)
              hasSente76R = Map.lookup (7, 6) bd == Just (Piece Sente Rook)
              hasSente78R = Map.lookup (7, 8) bd == Just (Piece Sente Rook)
              hasSente77N = Map.lookup (7, 7) bd == Just (Piece Sente Knight)
              hasSente97B = Map.lookup (9, 7) bd == Just (Piece Sente Bishop)
              
              sankanBonus   = if hasSente78R then 400.0 else 0.0
              pawnBonus     = if hasSente75P then 500.0 else 0.0
              floatingBonus = if hasSente76R && hasSente75P then 900.0 else 0.0
              knightBonus   = if hasSente77N && hasSente76R then 550.0 else 0.0
              bishopBonus   = if hasSente97B && hasSente77N then 450.0 else 0.0
          in sankanBonus + pawnBonus + floatingBonus + knightBonus + bishopBonus
        else
          let hasGote35P = Map.lookup (3, 5) bd == Just (Piece Gote Pawn)
              hasGote34R = Map.lookup (3, 4) bd == Just (Piece Gote Rook)
              hasGote32R = Map.lookup (3, 2) bd == Just (Piece Gote Rook)
              hasGote33N = Map.lookup (3, 3) bd == Just (Piece Gote Knight)
              hasGote13B = Map.lookup (1, 3) bd == Just (Piece Gote Bishop)
              
              sankanBonus   = if hasGote32R then 400.0 else 0.0
              pawnBonus     = if hasGote35P then 500.0 else 0.0
              floatingBonus = if hasGote34R && hasGote35P then 900.0 else 0.0
              knightBonus   = if hasGote33N && hasGote34R then 550.0 else 0.0
              bishopBonus   = if hasGote13B && hasGote33N then 450.0 else 0.0
          in sankanBonus + pawnBonus + floatingBonus + knightBonus + bishopBonus

      -- 3. 飛車のうろうろ手損（自陣での無駄な筋変更・横移動・目的のない浮き）への厳罰
      rooks = [ sq | (sq, Piece owner pt) <- Map.toList bd, owner == p, pt == Rook ]
      wanderingRookPenalty = sum
        [ let (rx, ry) = sq
              isIshidaLegit = if p == Sente
                              then (sq == (7, 8) || (sq == (7, 6) && Map.lookup (7, 5) bd == Just (Piece Sente Pawn)))
                              else (sq == (3, 2) || (sq == (3, 4) && Map.lookup (3, 5) bd == Just (Piece Gote Pawn)))
              isOriginalPos = if p == Sente then sq == (2, 8) else sq == (8, 2)
              isLegitFuribisha = if p == Sente then rx `elem` [5, 6, 7, 8] && ry == 8
                                 else rx `elem` [2, 3, 4, 5] && ry == 2
              -- 飛車先が歩で塞がれているのに浮いている手損 (2七飛、2六飛等)
              pawnAhead = if p == Sente then Map.lookup (rx, 7) bd == Just (Piece p Pawn)
                          else Map.lookup (rx, 3) bd == Just (Piece p Pawn)
              isAimlessLift = if p == Sente
                              then (ry == 7 || ry == 6) && pawnAhead && not isIshidaLegit
                              else (ry == 3 || ry == 4) && pawnAhead && not isIshidaLegit
              -- 無意味な筋変更 (3筋や4筋など歩も突いていないのに自陣に横移動)
              isAimlessFile = if p == Sente
                              then not isOriginalPos && not isLegitFuribisha && not isIshidaLegit && ry >= 7
                              else not isOriginalPos && not isLegitFuribisha && not isIshidaLegit && ry <= 3
          in (if isAimlessLift then -2500.0 else 0.0) +
             (if isAimlessFile then -3000.0 else 0.0)
        | sq <- rooks
        ]
  in silverScore + ishidaScore + wanderingRookPenalty
"""

lines.insert(idx_flow, formation_func + '\n')
print("1. Inserted evalSilverAdvanceAndIshidaFormation.")

# 2. Add silverAdvanceAndIshidaAdvantage to evaluateState
idx_eval = find_line('evaluateState :: MidgameWeights -> Player -> State -> Double')
idx_drop_def = find_line('bishopDropDefenseAdvantage = evalBishopDropVulnerability player st - evalBishopDropVulnerability opp st', idx_eval)
print(f"bishopDropDefenseAdvantage at line {idx_drop_def + 1}")

lines.insert(idx_drop_def + 1, '      silverAdvanceAndIshidaAdvantage = evalSilverAdvanceAndIshidaFormation player st - evalSilverAdvanceAndIshidaFormation opp st\n')

idx_eval_ret = find_line('+ potentialRayAdvantage + bishopDropDefenseAdvantage', idx_drop_def)
lines[idx_eval_ret] = lines[idx_eval_ret].rstrip() + ' + silverAdvanceAndIshidaAdvantage\n'
print("2. Added silverAdvanceAndIshidaAdvantage to evaluateState.")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)

print("Saved all updates successfully!")
