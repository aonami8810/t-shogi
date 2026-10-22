with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    content = f.read()

def find_line(pat, start=0):
    lines = content.split('\n')
    for i in range(start, len(lines)):
        if pat in lines[i]:
            return i
    return -1

# New functions to insert before evalPieceShuffling
advancement_funcs = """-- ============================================================================
-- 序盤の包括的モース勾配流：駒を前進させる手の網羅（Comprehensive Forward Flow）
-- 歩・銀・桂・角・飛の力強い前進、および戦型コミットメント（攻め筋の一貫性）
-- ============================================================================

-- | 1. 序盤の駒前進勾配流（着手スコア）：駒を前進させる手を網羅的に強力加点
evalComprehensiveAdvancementFlow :: State -> Move -> Double
evalComprehensiveAdvancementFlow st m =
  let plr = turn st
      opp = opponent plr
      curBd = board st
      nextSt = applyMove m st
      nextBd = board nextSt
  in case m of
       NormalMove from to _ ->
         let (fx, fy) = from
             (tx, ty) = to
             isAdvancing = if plr == Sente then ty < fy else ty > fy
         in case Map.lookup from curBd of
              -- A. 歩の前進勾配流（飛車先突き・中央突き・石田流急所・拠点構築）
              Just (Piece p Pawn) | p == plr ->
                let isRookPawnAdvance =
                      if plr == Sente
                      then (from == (2, 7) && to == (2, 6)) || (from == (2, 6) && to == (2, 5)) || (from == (2, 5) && to == (2, 4))
                      else (from == (8, 3) && to == (8, 4)) || (from == (8, 4) && to == (8, 5)) || (from == (8, 5) && to == (8, 6))
                    isCentralPawnAdvance =
                      if plr == Sente
                      then (from == (5, 7) && to == (5, 6)) || (from == (5, 6) && to == (5, 5))
                      else (from == (5, 3) && to == (5, 4)) || (from == (5, 4) && to == (5, 5))
                    isIshidaPawnAdvance =
                      if plr == Sente
                      then (from == (7, 7) && to == (7, 6)) || (from == (7, 6) && to == (7, 5))
                      else (from == (3, 3) && to == (3, 4)) || (from == (3, 4) && to == (3, 5))
                    isSidePawnAdvance =
                      if plr == Sente
                      then (from `elem` [(4, 7), (6, 7), (3, 7)] && isAdvancing)
                      else (from `elem` [(6, 3), (4, 3), (7, 3)] && isAdvancing)
                in if isRookPawnAdvance
                   then (if to == (2, 4) || to == (8, 6) then 1500.0 else (if to == (2, 5) || to == (8, 5) then 1200.0 else 1000.0))
                   else if isCentralPawnAdvance
                   then (if to == (5, 5) then 1300.0 else 1000.0)
                   else if isIshidaPawnAdvance
                   then (if to == (7, 5) || to == (3, 5) then 1300.0 else 1000.0)
                   else if isSidePawnAdvance
                   then 600.0
                   else if isAdvancing
                   then 350.0
                   else 0.0

              -- B. 銀の前進勾配流（棒銀・腰掛け銀・早繰り銀・中央進出）
              Just (Piece p Silver) | p == plr ->
                let isClimbingSilver =
                      if plr == Sente
                      then (fx `elem` [1, 2, 3] && tx `elem` [1, 2, 3] && ty <= 7)
                      else (fx `elem` [7, 8, 9] && tx `elem` [7, 8, 9] && ty >= 3)
                    isCentralSilver =
                      if plr == Sente
                      then to `elem` [(5, 6), (4, 6), (6, 6), (5, 5), (4, 5)]
                      else to `elem` [(5, 4), (6, 4), (4, 4), (5, 5), (6, 5)]
                    isFrontline = if plr == Sente then ty <= 5 else ty >= 5
                in if not isAdvancing then 0.0
                   else if isClimbingSilver then 1800.0 -- 棒銀の力強い進出！
                   else if isCentralSilver  then 1800.0 -- 腰掛け銀・中央厚み進出！
                   else if isFrontline      then 1200.0 -- 前線進出！
                   else 800.0

              -- C. 桂馬の前進勾配流（跳躍・石田流本組）
              Just (Piece p Knight) | p == plr ->
                let isIshidaKnight = (plr == Sente && to == (7, 7)) || (plr == Gote && to == (3, 3))
                    isAttackKnight = if plr == Sente
                                     then to `elem` [(3, 7), (4, 5), (7, 5), (2, 5)]
                                     else to `elem` [(7, 3), (6, 5), (3, 5), (8, 5)]
                in if isIshidaKnight then 1200.0
                   else if isAttackKnight then 1200.0
                   else if isAdvancing then 600.0
                   else 0.0

              -- D. 角の活用・前進勾配流
              Just (Piece p Bishop) | p == plr ->
                let isCornerBishop = (plr == Sente && to == (9, 7)) || (plr == Gote && to == (1, 3))
                    isCenterBishop = if plr == Sente
                                     then to `elem` [(7, 7), (6, 6), (5, 7), (7, 9)]
                                     else to `elem` [(3, 3), (4, 4), (5, 3), (3, 1)]
                in if isCornerBishop then 900.0
                   else if isCenterBishop then 800.0
                   else 300.0

              -- E. 飛車の前進勾配流（浮き飛車・前線突破）
              Just (Piece p Rook) | p == plr ->
                let isFloatingRook =
                      (plr == Sente && (to == (7, 6) || to == (2, 6) || to == (5, 6))) ||
                      (plr == Gote &&  (to == (3, 4) || to == (8, 4) || to == (5, 4)))
                    isVanguardRook = if plr == Sente then ty <= 5 else ty >= 5
                in if isFloatingRook then 3400.0
                   else if isVanguardRook then 1500.0
                   else 0.0
              _ -> 0.0
       _ -> 0.0

-- | 2. 攻め筋の一貫性（Strategic Commitment）：戦型フラフラ乗り換え・振り直しの厳罰化
evalStrategicCommitment :: State -> Move -> Double
evalStrategicCommitment st m =
  let plr = turn st
      opp = opponent plr
      curBd = board st
      hist = moveHistory st
      -- 先手の選択した主戦型の判定
      hasPlayedIshida = any (\mvStr -> mvStr == "2h7h" || mvStr == "7f7e") hist
      hasPlayedNakabisha = any (\mvStr -> mvStr == "2h5h" || mvStr == "5g5f") hist
      hasPlayedIbisha = any (\mvStr -> mvStr == "2g2f" || mvStr == "2f2e") hist
  in case m of
       NormalMove from to _ ->
         case Map.lookup from curBd of
           Just (Piece p Rook) | p == plr ->
             let (fx, _) = from
                 (tx, _) = to
                 -- 振り直し（筋の転換）検知
                 isSwitchingMode =
                   if plr == Sente
                   then (hasPlayedIshida && tx `notElem` [7]) ||
                        (hasPlayedNakabisha && tx `notElem` [5]) ||
                        (hasPlayedIbisha && tx `notElem` [2])
                   else False
                 isPromo = False
                 isCapturing = Map.member to curBd
             in if isSwitchingMode && not isCapturing
                then -5000.0 -- 一度決めた主戦型を放棄して別の筋へ振り直す日和見を完全厳禁！
                else 0.0
           _ -> 0.0
       _ -> 0.0

-- | 3. 局面評価：全駒の前進ポテンシャルエネルギー（Advancement Potential Energy）
evalComprehensiveAdvancementFormation :: Player -> State -> Double
evalComprehensiveAdvancementFormation p st =
  let bd = board st
      pieces = [ (sq, pt) | (sq, Piece owner pt) <- Map.toList bd, owner == p ]
      scorePiece ((x, y), pt) =
        let adv = if p == Sente then 9 - y else y - 1
        in case pt of
             Pawn ->
               let isAdvancedPawn = if p == Sente then y <= 6 else y >= 4
                   isDeepPawn     = if p == Sente then y <= 5 else y >= 5
               in fromIntegral adv * 30.0 + (if isDeepPawn then 250.0 else (if isAdvancedPawn then 120.0 else 0.0))
             Silver ->
               let isDeepSilver = if p == Sente then y <= 5 else y >= 5
               in fromIntegral adv * 50.0 + (if isDeepSilver then 400.0 else 0.0)
             Knight ->
               fromIntegral adv * 40.0
             Rook ->
               let isFloating = if p == Sente then y <= 6 else y >= 4
               in if isFloating then 300.0 else 0.0
             _ -> 0.0
  in sum (map scorePiece pieces)
"""

# Insert advancement_funcs before evalPieceShuffling
idx_shuff = content.find('evalPieceShuffling :: State -> Move -> Double')
if idx_shuff == -1:
    raise ValueError("evalPieceShuffling not found")

content = content[:idx_shuff] + advancement_funcs + '\n' + content[idx_shuff:]
print("Inserted advancement_funcs.")

# Now integrate into scoreMove
# In scoreMove:
# bishopDropQualityBonus = evalBishopDropQuality st m
# silverAndIshidaBonus = evalSilverAdvanceAndIshidaFlow st m
target_sm = """                   bishopDropQualityBonus = evalBishopDropQuality st m
                   silverAndIshidaBonus = evalSilverAdvanceAndIshidaFlow st m"""

replacement_sm = """                   bishopDropQualityBonus = evalBishopDropQuality st m
                   silverAndIshidaBonus = evalSilverAdvanceAndIshidaFlow st m
                   comprehensiveAdvanceBonus = evalComprehensiveAdvancementFlow st m
                   strategicCommitmentBonus = evalStrategicCommitment st m"""

if target_sm in content:
    content = content.replace(target_sm, replacement_sm)
    print("Added comprehensiveAdvanceBonus and strategicCommitmentBonus to scoreMove definitions.")
else:
    print("Warning: target_sm not found!")

# Update return line of scoreMove
# + silverAndIshidaBonus
target_sm_ret = "+ silverAndIshidaBonus"
replacement_sm_ret = "+ silverAndIshidaBonus + comprehensiveAdvanceBonus + strategicCommitmentBonus"
if target_sm_ret in content:
    content = content.replace(target_sm_ret, replacement_sm_ret)
    print("Added to scoreMove return expression.")
else:
    print("Warning: target_sm_ret not found!")

# Now integrate into evaluateState
# silverAdvanceAndIshidaAdvantage = evalSilverAdvanceAndIshidaFormation player st - evalSilverAdvanceAndIshidaFormation opp st
target_eval = """      silverAdvanceAndIshidaAdvantage = evalSilverAdvanceAndIshidaFormation player st - evalSilverAdvanceAndIshidaFormation opp st"""

replacement_eval = """      silverAdvanceAndIshidaAdvantage = evalSilverAdvanceAndIshidaFormation player st - evalSilverAdvanceAndIshidaFormation opp st
      comprehensiveAdvanceAdvantage = evalComprehensiveAdvancementFormation player st - evalComprehensiveAdvancementFormation opp st"""

if target_eval in content:
    content = content.replace(target_eval, replacement_eval)
    print("Added comprehensiveAdvanceAdvantage to evaluateState definitions.")
else:
    print("Warning: target_eval not found!")

target_eval_ret = "+ silverAdvanceAndIshidaAdvantage"
replacement_eval_ret = "+ silverAdvanceAndIshidaAdvantage + comprehensiveAdvanceAdvantage"
if target_eval_ret in content:
    content = content.replace(target_eval_ret, replacement_eval_ret)
    print("Added to evaluateState return expression.")
else:
    print("Warning: target_eval_ret not found!")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(content)

print("Saved all updates to ShogiUsiMorse-v4.hs successfully!")
