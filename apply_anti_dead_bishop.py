with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

def find_line(pat, start=0):
    for i in range(start, len(lines)):
        if pat in lines[i]:
            return i
    return -1

# 1. Update evaluateState to include bishopDropDefenseAdvantage
idx_mat = find_line('materialAdvantage = evalMaterial')
idx_potRay = find_line('potentialRayAdvantage = evalPotentialRayAttack', idx_mat)
idx_eval_in = find_line('in materialAdvantage +', idx_potRay)
print(f"evaluateState potentialRayAdvantage at {idx_potRay + 1}, return at {idx_eval_in + 1}")

lines.insert(idx_potRay + 1, '      bishopDropDefenseAdvantage = evalBishopDropVulnerability player st - evalBishopDropVulnerability opp st\n')
# Now update return sum
idx_eval_in_new = find_line('in materialAdvantage +', idx_potRay)
idx_last_term = find_line('+ numericalAssaultAdvantage + tacticalForkAdvantage + potentialRayAdvantage', idx_eval_in_new)
lines[idx_last_term] = lines[idx_last_term].rstrip() + ' + bishopDropDefenseAdvantage\n'
print("1. Added bishopDropDefenseAdvantage to evaluateState.")

# 2. Fix isAligned in scoreMove (require direct ray for Bishop)
idx_aligned = find_line('(pt == Bishop && abs (tx - ekx) == abs (ty - eky))')
print(f"isAligned at line {idx_aligned + 1}")
lines[idx_aligned] = '                                            (pt == Bishop && abs (tx - ekx) == abs (ty - eky) && canPieceReachTarget to (Piece (turn st) Bishop) (ekx, eky) nextBd) ||\n'
print("2. Fixed isAligned to require canPieceReachTarget for Bishop.")

# 3. Add bishopDropQualityBonus to scoreMove
idx_trapBonus = find_line('trappedDesperadoBonus = evalTrappedPieceDesperado st m')
print(f"trappedDesperadoBonus at line {idx_trapBonus + 1}")
lines.insert(idx_trapBonus + 1, '                   bishopDropQualityBonus = evalBishopDropQuality st m\n')
idx_sm_return = find_line('in s + captureBonus + blunderPen', idx_trapBonus)
idx_sm_term = find_line('+ antiQuotientSafetyBonus + shufflingPen', idx_sm_return)
lines[idx_sm_term - 1] = lines[idx_sm_term - 1].rstrip() + ' + bishopDropQualityBonus\n'
print("3. Added bishopDropQualityBonus to scoreMove.")

# 4. Insert evalBishopDropQuality and evalBishopDropVulnerability before evalPieceShuffling
idx_shuff = find_line('evalPieceShuffling :: State -> Move -> Double')
print(f"evalPieceShuffling at line {idx_shuff + 1}")

bishop_funcs = """-- | 角打ちの手（DropMove Bishop to）の品質・角道の開通度（Mobility）評価
-- 98角のように角道が自駒で塞がれている無意味な死に角打ちを完全排除
evalBishopDropQuality :: State -> Move -> Double
evalBishopDropQuality st m =
  let plr = turn st
      opp = opponent plr
      curBd = board st
      nextSt = applyMove m st
      nextBd = board nextSt
  in case m of
       DropMove Bishop to ->
         let dirs = [(-1, -1), (-1, 1), (1, -1), (1, 1)]
             countRay (dx, dy) =
               let raySquares = takeWhile (\\(x, y) -> x >= 1 && x <= 9 && y >= 1 && y <= 9)
                                          [ (fst to + i * dx, snd to + i * dy) | i <- [1..8] ]
                   validSquares [] = 0
                   validSquares (sq:sqs) =
                     case Map.lookup sq curBd of
                       Nothing -> 1 + validSquares sqs
                       Just _  -> 1
               in validSquares raySquares
             
             totalMobility = sum (map countRay dirs)
             
             oppKingSq = findKing opp nextBd
             hasDirectRayToKing = case oppKingSq of
               Just kSq -> canPieceReachTarget to (Piece plr Bishop) kSq nextBd
               Nothing  -> False
               
             inEnemyZone y = if plr == Sente then y <= 3 else y >= 7
             attacksEnemyCamp = any (\\(dx, dy) ->
               let targetSquares = [ (fst to + i * dx, snd to + i * dy) | i <- [1..8] ]
               in any (\\sq -> not (isOutOfBounds sq) && inEnemyZone (snd sq)) targetSquares
               ) dirs
         in if totalMobility <= 2
            then -10000.0 -- 角道が0〜2マスしか通っていない！98角などの完全な死角打ちは厳禁！
            else if totalMobility <= 4 && not hasDirectRayToKing
            then -4000.0  -- 角道が狭すぎる無意味な角打ち
            else if hasDirectRayToKing
            then 800.0    -- 敵玉に直通する急所角打ち！
            else if attacksEnemyCamp
            then 400.0    -- 相手陣を睨む有効な角打ち
            else 100.0
       _ -> 0.0

-- | 相手の角打ち（Bishop Drop Threat）に対する脆弱性・無防備さの評価
-- 相手が手持ちに角を持っているとき、自陣・中段への危険な角打ち（両取り・馬作り・王手）をスキャンして厳罰化
evalBishopDropVulnerability :: Player -> State -> Double
evalBishopDropVulnerability plr st =
  let opp = opponent plr
      oppHand = Map.findWithDefault Map.empty opp (hands st)
      hasOppBishop = Map.findWithDefault 0 Bishop oppHand > 0
  in if not hasOppBishop
     then 0.0
     else
       let bd = board st
           targetY = if plr == Sente then [4..9] else [1..6]
           candidateDropSquares = [ (x, y) | x <- [1..9], y <- targetY, not (Map.member (x, y) bd) ]
           
           isDangerousBishopDrop sq =
             let myDefenders = getPiecesCovering plr sq bd
                 isProtected = not (null myDefenders)
                 
                 testBd = Map.insert sq (Piece opp Bishop) bd
                 threatenedPieces = [ tsq | (tsq, Piece p pt) <- Map.toList testBd
                                          , p == plr
                                          , pt `elem` [King, Rook, PRook, Gold, Silver]
                                          , canPieceReachTarget sq (Piece opp Bishop) tsq testBd ]
                 
                 isForkOrCheck = length threatenedPieces >= 2 ||
                                 any (\\tsq -> Map.lookup tsq bd == Just (Piece plr King)) threatenedPieces
                 
                 canPromoteNext = if opp == Sente then snd sq >= 7 else snd sq <= 3
             in not isProtected && (isForkOrCheck || canPromoteNext)
             
           dangerousCount = length (filter isDangerousBishopDrop candidateDropSquares)
       in if dangerousCount > 0
          then - (fromIntegral dangerousCount * 1200.0) -- 危険な角打ちの隙1箇所につき -1200点！
          else 600.0 -- 隙がゼロなら完璧な鉄壁防御ボーナス！
"""

lines.insert(idx_shuff, bishop_funcs + '\n')
print("4. Inserted evalBishopDropQuality and evalBishopDropVulnerability.")

# 5. Update isBlunderMove to detect dead bishop drops
idx_wandering = find_line('isWanderingKingBlunder =')
idx_blunder_res = find_line('in if isMajorSuicideCheck || isMajorCheapTradeCheck ||', idx_wandering)
print(f"isBlunderMove result at line {idx_blunder_res + 1}")

dead_bishop_blunder = """          -- 9. 角道が通っていない死に角打ち（98角など）の完全排除
          isDeadBishopDrop = case m of
            DropMove Bishop to ->
              let dirs = [(-1, -1), (-1, 1), (1, -1), (1, 1)]
                  countRay (dx, dy) =
                    let raySquares = takeWhile (\\(x, y) -> x >= 1 && x <= 9 && y >= 1 && y <= 9)
                                               [ (fst to + i * dx, snd to + i * dy) | i <- [1..8] ]
                        validSquares [] = 0
                        validSquares (sq:sqs) =
                          case Map.lookup sq bd of
                            Nothing -> 1 + validSquares sqs
                            Just _  -> 1
                    in validSquares raySquares
                  totalMobility = sum (map countRay dirs)
              in totalMobility <= 2
            _ -> False
"""
lines.insert(idx_blunder_res, dead_bishop_blunder)
idx_blunder_res_new = find_line('in if isMajorSuicideCheck || isMajorCheapTradeCheck ||', idx_blunder_res)
lines[idx_blunder_res_new] = lines[idx_blunder_res_new].replace(
    'in if isMajorSuicideCheck || isMajorCheapTradeCheck ||',
    'in if isMajorSuicideCheck || isMajorCheapTradeCheck || isDeadBishopDrop ||'
)
print("5. Added isDeadBishopDrop to isBlunderMove.")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)

print("Saved all changes to ShogiUsiMorse-v4.hs successfully!")
