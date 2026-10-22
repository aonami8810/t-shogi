with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

def find_line(pat, start=0):
    for i in range(start, len(lines)):
        if pat in lines[i]:
            return i
    return -1

# 1. Update evalSilverAdvanceAndIshidaFormation with decisive gradients
idx_formation = find_line('evalSilverAdvanceAndIshidaFormation :: Player -> State -> Double')
idx_flow = find_line('evalSilverAdvanceAndIshidaFlow :: State -> Move -> Double', idx_formation)

new_formation = """evalSilverAdvanceAndIshidaFormation :: Player -> State -> Double
evalSilverAdvanceAndIshidaFormation p st =
  let bd = board st
      -- 1. 銀の前進・前線配置（棒銀・腰掛け銀・早繰り銀・中央厚み）
      silvers = [ sq | (sq, Piece owner pt) <- Map.toList bd, owner == p, pt `elem` [Silver, PSilver] ]
      silverScore = sum
        [ let (x, y) = sq
              adv = if p == Sente then 9 - y else y - 1
              -- 棒銀 (Climbing Silver: 先手2筋・1筋, 後手8筋・9筋)
              isClimbing = if p == Sente
                           then (x == 2 || x == 1 || (x == 3 && y == 7)) && y <= 7
                           else (x == 8 || x == 9 || (x == 7 && y == 3)) && y >= 3
              -- 腰掛け銀・中央厚み (Reclining / Central: 4,5,6筋の中段)
              isCentral = if p == Sente
                          then sq `elem` [(5, 6), (4, 6), (6, 6), (5, 5), (4, 5)]
                          else sq `elem` [(5, 4), (6, 4), (4, 4), (5, 5), (6, 5)]
              -- 前線進出 (Frontline: 5段目以上)
              isFront = if p == Sente then y <= 5 else y >= 5
          in (if isClimbing then 1800.0 else 0.0) +
             (if isCentral  then 1600.0 else 0.0) +
             (if isFront    then 1200.0 else (if adv >= 2 then fromIntegral adv * 250.0 else 0.0))
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
              
              sankanBonus   = if hasSente78R then (if hasSente75P then 2200.0 else 900.0) else 0.0
              pawnBonus     = if hasSente75P then 800.0 else 0.0
              floatingBonus = if hasSente76R && hasSente75P then 2600.0 else 0.0
              knightBonus   = if hasSente77N && hasSente76R then 1200.0 else 0.0
              bishopBonus   = if hasSente97B && hasSente77N then 900.0 else 0.0
          in sankanBonus + pawnBonus + floatingBonus + knightBonus + bishopBonus
        else
          let hasGote35P = Map.lookup (3, 5) bd == Just (Piece Gote Pawn)
              hasGote34R = Map.lookup (3, 4) bd == Just (Piece Gote Rook)
              hasGote32R = Map.lookup (3, 2) bd == Just (Piece Gote Rook)
              hasGote33N = Map.lookup (3, 3) bd == Just (Piece Gote Knight)
              hasGote13B = Map.lookup (1, 3) bd == Just (Piece Gote Bishop)
              
              sankanBonus   = if hasGote32R then (if hasGote35P then 2200.0 else 900.0) else 0.0
              pawnBonus     = if hasGote35P then 800.0 else 0.0
              floatingBonus = if hasGote34R && hasGote35P then 2600.0 else 0.0
              knightBonus   = if hasGote33N && hasGote34R then 1200.0 else 0.0
              bishopBonus   = if hasGote13B && hasGote33N then 900.0 else 0.0
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
          in (if isAimlessLift then -3500.0 else 0.0) +
             (if isAimlessFile then -4000.0 else 0.0)
        | sq <- rooks
        ]
  in silverScore + ishidaScore + wanderingRookPenalty
"""

lines[idx_formation:idx_flow] = [l + '\n' for l in new_formation.strip().split('\n')] + ['\n\n']

# 2. Update evalSilverAdvanceAndIshidaFlow
idx_flow_new = find_line('evalSilverAdvanceAndIshidaFlow :: State -> Move -> Double')
idx_shuff = find_line('evalPieceShuffling :: State -> Move -> Double', idx_flow_new)

new_flow = """evalSilverAdvanceAndIshidaFlow :: State -> Move -> Double
evalSilverAdvanceAndIshidaFlow st m =
  let plr = turn st
      opp = opponent plr
      curBd = board st
      nextSt = applyMove m st
      nextBd = board nextSt
  in case m of
       NormalMove from to _ ->
         case Map.lookup from curBd of
           -- A. 銀の前進・前線進出（棒銀・腰掛け銀・早繰り銀）
           Just (Piece p Silver) | p == plr ->
             let (fx, fy) = from
                 (tx, ty) = to
                 isAdvancing = if plr == Sente then ty < fy else ty > fy
                 -- 1. 棒銀（Climbing Silver: 先手2筋・後手8筋を登る）
                 isClimbingSilver =
                   if plr == Sente
                   then (fx `elem` [1, 2, 3] && tx `elem` [1, 2, 3] && ty <= 7)
                   else (fx `elem` [7, 8, 9] && tx `elem` [7, 8, 9] && ty >= 3)
                 -- 2. 腰掛け銀・中央進出（Reclining / Central Silver: 5六・4六・6六）
                 isCentralSilver =
                   if plr == Sente
                   then (to == (5, 6) || to == (4, 6) || to == (6, 6) || to == (5, 5) || to == (4, 5))
                   else (to == (5, 4) || to == (6, 4) || to == (4, 4) || to == (5, 5) || to == (6, 5))
                 -- 3. 前線（敵陣近傍）進出
                 isFrontline = if plr == Sente then ty <= 5 else ty >= 5
             in if not isAdvancing then 0.0
                else if isClimbingSilver then 1800.0 -- 棒銀の力強い進出！
                else if isCentralSilver  then 1600.0 -- 腰掛け銀・中央厚み進出！
                else if isFrontline      then 1200.0 -- 前線進出！
                else 600.0
           
           -- B. 石田流本組・三間飛車のモース勾配流
           -- 1. 飛車の動き（三間飛車 7八飛 / 3二飛、浮き飛車 7六飛 / 3四飛）
           Just (Piece p Rook) | p == plr ->
             let (fx, fy) = from
                 (tx, ty) = to
                 -- 先手石田流
                 isSenteSankanRook = plr == Sente && to == (7, 8)
                 isSenteFloatingRook = plr == Sente && from == (7, 8) && to == (7, 6) &&
                                       Map.lookup (7, 5) curBd == Just (Piece Sente Pawn)
                 -- 後手石田流
                 isGoteSankanRook = plr == Gote && to == (3, 2)
                 isGoteFloatingRook = plr == Gote && from == (3, 2) && to == (3, 4) &&
                                      Map.lookup (3, 5) curBd == Just (Piece Gote Pawn)
             in if isSenteFloatingRook || isGoteFloatingRook
                then 2600.0 -- ★ 石田流浮き飛車（最強の空中戦要塞！）
                else if isSenteSankanRook || isGoteSankanRook
                then (if Map.lookup (if plr == Sente then (7, 5) else (3, 5)) curBd == Just (Piece plr Pawn) then 2200.0 else 1000.0)
                else 0.0
           
           -- 2. 歩の動き（石田流の命：7五歩 / 3五歩突き）
           Just (Piece p Pawn) | p == plr ->
             let isSenteIshidaPawn = plr == Sente && from == (7, 6) && to == (7, 5)
                 isGoteIshidaPawn  = plr == Gote && from == (3, 4) && to == (3, 5)
             in if isSenteIshidaPawn || isGoteIshidaPawn
                then 1200.0 -- ★ 石田流の急所歩突き！
                else 0.0
                
           -- 3. 桂馬の動き（石田流本組の桂跳ね：7七桂 / 3三桂）
           Just (Piece p Knight) | p == plr ->
             let isSenteIshidaKnight = plr == Sente && to == (7, 7) &&
                                       Map.lookup (7, 6) curBd == Just (Piece Sente Rook)
                 isGoteIshidaKnight  = plr == Gote && to == (3, 3) &&
                                       Map.lookup (3, 4) curBd == Just (Piece Gote Rook)
             in if isSenteIshidaKnight || isGoteIshidaKnight
                then 1200.0 -- ★ 石田流本組の桂跳ね！
                else 0.0
                
           -- 4. 角の動き（石田流の角転回：9七角 / 1三角）
           Just (Piece p Bishop) | p == plr ->
             let isSenteCornerBishop = plr == Sente && to == (9, 7)
                 isGoteCornerBishop  = plr == Gote && to == (1, 3)
             in if isSenteCornerBishop || isGoteCornerBishop
                then 900.0
                else 0.0
           _ -> 0.0
       _ -> 0.0
"""

lines[idx_flow_new:idx_shuff] = [l + '\n' for l in new_flow.strip().split('\n')] + ['\n\n']

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)

print("Saved updated high gradient formations!")
