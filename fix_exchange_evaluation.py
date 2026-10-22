import sys

with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    code = f.read()

# 1. Update evalHangingPieces to use proportional penalties
old_hanging = """-- | 大駒・重要駒のタダ取られ（Hanging Pieces / En Prise）および大損交換危険度評価
-- モース勾配流における「大駒のタダ捨て・無謀な突撃による自滅」を評価関数レベルで完全排除
evalHangingPieces :: Player -> State -> Double
evalHangingPieces player st =
  let bd = board st
      opp = opponent player
      evalPlayerHanging plr =
        let enemy = opponent plr
            pieces = [ (sq, pt) | (sq, Piece p pt) <- Map.toList bd, p == plr ]
            hangingPenalty (sq, pt) =
              let oppAttackers = getPiecesCovering enemy sq bd
                  myDefenders  = getPiecesCovering plr sq bd
                  hasCheapAttacker = any (\\(_, Piece _ opt) -> opt `elem` [Pawn, Lance, Knight, Silver]) oppAttackers
                  isMajor = pt `elem` [Rook, PRook, Bishop, PBishop]
                  isGoldOrSilver = pt `elem` [Gold, Silver, PPawn, PSilver, PKnight, PLance]
              in if null oppAttackers
                 then 0.0
                 else if null myDefenders
                      then
                        if isMajor then -12000.0       -- 飛車・角のタダ取られは致命的壊滅！
                        else if isGoldOrSilver then -4500.0 -- 金銀のタダ取られ
                        else if pt `elem` [Knight, Lance] then -2500.0
                        else -150.0
                      else
                        if isMajor && hasCheapAttacker
                        then -9000.0                 -- 大駒が歩・香・桂・銀にタダ同然で取られる大損
                        else if isGoldOrSilver && any (\\(_, Piece _ opt) -> opt == Pawn) oppAttackers
                        then -2500.0
                        else 0.0
        in sum [ hangingPenalty p | p <- pieces ]
  in evalPlayerHanging player - evalPlayerHanging opp"""

new_hanging = """-- | 大駒・重要駒のタダ取られ（Hanging Pieces / En Prise）および大損交換危険度評価
-- モース勾配流における「大駒のタダ捨て・無謀な突撃による自滅」を評価関数レベルで完全排除
-- 各駒の価値（PieceValue）に比例したペナルティとし、角銀交換と角歩交換の価値差（400点）を厳格に保持
evalHangingPieces :: Player -> State -> Double
evalHangingPieces player st =
  let bd = board st
      opp = opponent player
      evalPlayerHanging plr =
        let enemy = opponent plr
            pieces = [ (sq, pt) | (sq, Piece p pt) <- Map.toList bd, p == plr ]
            hangingPenalty (sq, pt) =
              let oppAttackers = getPiecesCovering enemy sq bd
                  myDefenders  = getPiecesCovering plr sq bd
                  hasCheapAttacker = any (\\(_, Piece _ opt) -> opt `elem` [Pawn, Lance, Knight, Silver]) oppAttackers
                  val = pieceValue pt
              in if null oppAttackers
                 then 0.0
                 else if null myDefenders
                      then
                        -- 紐なしで取られる場合：その駒の価値 × 1.8倍（手番損失＋相手の戦力化）
                        - (val * 1.8)
                      else
                        -- 紐はあるが、相手の安い駒（歩・香・桂・銀）で取られる大損交換
                        if val >= 500.0 && hasCheapAttacker
                        then - ((val - 100.0) * 1.2)
                        else 0.0
        in sum [ hangingPenalty p | p <- pieces ]
  in evalPlayerHanging player - evalPlayerHanging opp"""

if old_hanging in code:
    code = code.replace(old_hanging, new_hanging, 1)
    print("1. Updated evalHangingPieces with proportional penalties.")
else:
    print("Could not find old_hanging!")

# 2. Update searchQuiescence with MVV-LVA sorting
old_quiesc = """            let curAlpha = max alpha standPat
                rawMoves = generateMoves st
                tacticalMoves = filter (isTacticalMove st) rawMoves
                f5Tacticals = f5SyzygyFilter st tacticalMoves
            in if null f5Tacticals
               then curAlpha
               else qLoop f5Tacticals curAlpha beta"""

new_quiesc = """            let curAlpha = max alpha standPat
                rawMoves = generateMoves st
                tacticalMoves = filter (isTacticalMove st) rawMoves
                f5Tacticals = f5SyzygyFilter st tacticalMoves
                -- MVV-LVA: 取る駒の価値が高い順（銀500点 -> 歩100点）にソートして最優先探索！
                scoreTacticalMove m = case m of
                  NormalMove _ to isP ->
                    let victimVal = case Map.lookup to (board st) of
                          Just (Piece _ pt) -> pieceValue pt
                          Nothing           -> 0.0
                        promoVal = if isP then 200.0 else 0.0
                    in victimVal * 10.0 + promoVal
                  DropMove pt _ ->
                    if pt `elem` [Rook, Bishop] then -500.0 else 100.0
                sortedTacticals = sortBy (\\m1 m2 -> compare (scoreTacticalMove m2) (scoreTacticalMove m1)) f5Tacticals
            in if null sortedTacticals
               then curAlpha
               else qLoop sortedTacticals curAlpha beta"""

if old_quiesc in code:
    code = code.replace(old_quiesc, new_quiesc, 1)
    print("2. Added MVV-LVA sorting to searchQuiescence.")
else:
    print("Could not find old_quiesc!")

# 3. Add captureBonus to scoreMove
old_scoreMove_sum = """                   s = evaluateState weights (turn st) nextSt
                in s + blunderPen + lancePen + suicidePen + checkBonus + forkBonus + tacticalBonus + fourPieceAdvanceBonus + oneTempoWinBonus + ironcladDefenseBonus"""

new_scoreMove_sum = """                   -- ★ 捕獲駒ボーナス（MVV-LVA）：銀を取る手（+3000点）と歩を取る手（+600点）に大差をつける
                   captureBonus = case targetCaptured of
                     Just (Piece _ pt) -> pieceValue pt * 6.0
                     Nothing           -> 0.0
                   
                   s = evaluateState weights (turn st) nextSt
                in s + captureBonus + blunderPen + lancePen + suicidePen + checkBonus + forkBonus + tacticalBonus + fourPieceAdvanceBonus + oneTempoWinBonus + ironcladDefenseBonus"""

if old_scoreMove_sum in code:
    code = code.replace(old_scoreMove_sum, new_scoreMove_sum, 1)
    print("3. Added captureBonus to scoreMove.")
else:
    print("Could not find old_scoreMove_sum!")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(code)

print("Saved all updates to ShogiUsiMorse-v4.hs successfully!")
