with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    content = f.read()

# 1. Fix isPushingKakutoPawn to ALLOW capturing the opponent's vanguard pawn (同歩)
old_kakuto_pawn = """-- | 角頭の歩突き（先手8七歩 8g8f、後手2三歩 2c2d）の絶対禁止判定
-- 飛車が8筋/2筋にいない状態で角頭の歩を突く悪手を完全に検知・排除
isPushingKakutoPawn :: State -> Move -> Bool
isPushingKakutoPawn st m =
  let plr = turn st
      bd = board st
  in case m of
    NormalMove (8, 7) (8, 6) _ | plr == Sente ->
      case Map.lookup (8, 8) bd of
        Just (Piece Sente Rook)  -> False -- 8八飛（向かい飛車）なら許容
        Just (Piece Sente PRook) -> False
        _                        -> True  -- 角が8八にいる/飛車がいないのに8七歩を突くのは絶対禁止
    NormalMove (2, 3) (2, 4) _ | plr == Gote ->
      case Map.lookup (2, 2) bd of
        Just (Piece Gote Rook)  -> False -- 2二飛なら許容
        Just (Piece Gote PRook) -> False
        _                       -> True  -- 角が2二にいる/飛車がいないのに2三歩を突くのは絶対禁止
    _ -> False"""

new_kakuto_pawn = """-- | 角頭の歩突き（先手8七歩 8g8f、後手2三歩 2c2d）の絶対禁止判定
-- ★ 超重要：相手の歩を取る「同歩（Take Vanguard Pawn）」は絶対に許可・大推奨！
-- 相手駒がおらず、飛車もいない状態で単に角頭の歩を突く悪手のみを排除
isPushingKakutoPawn :: State -> Move -> Bool
isPushingKakutoPawn st m =
  let plr = turn st
      bd = board st
  in case m of
    NormalMove (8, 7) (8, 6) _ | plr == Sente ->
      let isCapturing = Map.member (8, 6) bd -- 相手の8六歩を取る「同歩」は超重要守備手！
      in if isCapturing then False
         else case Map.lookup (8, 8) bd of
           Just (Piece Sente Rook)  -> False -- 8八飛（向かい飛車）なら許容
           Just (Piece Sente PRook) -> False
           _                        -> True  -- 単なる突き出しは禁止
    NormalMove (2, 3) (2, 4) _ | plr == Gote ->
      let isCapturing = Map.member (2, 4) bd -- 相手の2四歩を取る「同歩」は超重要守備手！
      in if isCapturing then False
         else case Map.lookup (2, 2) bd of
           Just (Piece Gote Rook)  -> False -- 2二飛なら許容
           Just (Piece Gote PRook) -> False
           _                       -> True  -- 単なる突き出しは禁止
    _ -> False"""

content = content.replace(old_kakuto_pawn, new_kakuto_pawn)
print("1. Fixed isPushingKakutoPawn capturing bug.")

# 2. Fix castleSquares in evalPieceShuffling to include Mino, Tsuno-Gin, and Furibisha king moves
old_castle_squares = """                      -- 囲いの定位置（美濃・矢倉・雁木・エルモ・穴熊）
                      castleSquares = if plr == Sente
                                      then [(7, 8), (8, 8), (7, 9), (6, 8), (8, 9)]
                                      else [(3, 2), (2, 2), (3, 1), (4, 2), (2, 1)]
                      isEnteringCastle = to `elem` castleSquares
                      castleBonus = if isEnteringCastle then 450.0 else 0.0"""

new_castle_squares = """                      -- 囲いの定位置（美濃囲い・つの銀・矢倉・雁木・エルモ・穴熊）
                      -- 先手美濃: 4八玉, 3八玉, 2八玉, 3九玉
                      -- 先手居飛車囲い: 6八玉, 7八玉, 8八玉, 7九玉, 8九玉
                      castleSquares = if plr == Sente
                                      then [(4, 8), (3, 8), (2, 8), (3, 9), (6, 8), (7, 8), (8, 8), (7, 9), (8, 9)]
                                      else [(6, 2), (7, 2), (8, 2), (7, 1), (4, 2), (3, 2), (2, 2), (3, 1), (2, 1)]
                      isEnteringCastle = to `elem` castleSquares
                      -- 美濃囲い・矢倉への入城・玉囲い前進には特大ボーナス
                      castleBonus = if isEnteringCastle
                                    then (if to `elem` [(3, 8), (2, 8), (7, 2), (8, 2), (8, 8), (2, 2)] then 2200.0 else 1800.0)
                                    else 0.0"""

content = content.replace(old_castle_squares, new_castle_squares)
print("2. Fixed castleSquares to fully support Mino castle and right-side king mobilization.")

# 3. Add Rook-File Defense & Pawn Drop & Tsuno-Gin & Anti-Sitting-King logic
new_modules = """-- ============================================================================
-- 居飛車の飛車先受け（Ironclad Rook-File Defense）＆ 居玉防止＆美濃・つの銀中飛車
-- ============================================================================

-- | 飛車先の受け（Rook-File Interception Defense）：
-- 1. 相手の飛車先突き（8六歩 / 2四歩）に対する「同歩（Capture Vanguard Pawn）」
-- 2. 飛車直射局面における「8七歩打ち（P*8g）」「2三歩打ち（P*2c）」の絶対的歩受け
evalRookFileDefenseFlow :: State -> Move -> Double
evalRookFileDefenseFlow st m =
  let plr = turn st
      opp = opponent plr
      curBd = board st
      nextSt = applyMove m st
      nextBd = board nextSt
  in case m of
       -- A. 歩打ち受け（P*8g 先手8七歩打 / P*2c 後手2三歩打）
       DropMove Pawn toSq ->
         let isSente87Drop = plr == Sente && toSq == (8, 7) &&
                             case Map.lookup (8, 6) curBd of
                               Just (Piece p Rook) | p == opp -> True
                               _ -> case Map.lookup (8, 5) curBd of
                                      Just (Piece p Rook) | p == opp -> True
                                      _ -> False
             isGote23Drop  = plr == Gote && toSq == (2, 3) &&
                             case Map.lookup (2, 4) curBd of
                               Just (Piece p Rook) | p == opp -> True
                               _ -> case Map.lookup (2, 5) curBd of
                                      Just (Piece p Rook) | p == opp -> True
                                      _ -> False
         in if isSente87Drop || isGote23Drop
            then 5000.0 -- ★ 飛車先の直射を止める絶対の歩受け手筋！竜作りを完全阻止！
            else 0.0

       -- B. 相手の飛車先歩に対する「同歩（Capture Vanguard Pawn）」
       NormalMove from toSq _ ->
         let isSenteCapture86 = plr == Sente && from == (8, 7) && toSq == (8, 6) &&
                                Map.lookup (8, 6) curBd == Just (Piece opp Pawn)
             isGoteCapture24  = plr == Gote && from == (2, 3) && toSq == (2, 4) &&
                                Map.lookup (2, 4) curBd == Just (Piece opp Pawn)
         in if isSenteCapture86 || isGoteCapture24
            then 4500.0 -- ★ 相手の飛車先突破を同歩で迎え撃つ絶対の迎撃手筋！
            else 0.0

-- | 中飛車における美濃囲い＆つの銀（Twin Silvers Central Rook）の形成評価
evalNakabishaCastlingFlow :: State -> Move -> Double
evalNakabishaCastlingFlow st m =
  let plr = turn st
      curBd = board st
      hist = moveHistory st
      isNakabishaMode = any (\\mvStr -> mvStr == "2h5h" || mvStr == "5g5f") hist ||
                        Map.lookup (if plr == Sente then (5, 8) else (5, 2)) curBd == Just (Piece plr Rook)
  in if not isNakabishaMode
     then 0.0
     else case m of
       -- 1. 玉の美濃囲い移動（5九 -> 4八 -> 3八 -> 2八）
       NormalMove from toSq _ ->
         case Map.lookup from curBd of
           Just (Piece p King) | p == plr ->
             if plr == Sente
             then if toSq == (4, 8) then 1800.0
                  else if toSq == (3, 8) then 2400.0
                  else if toSq == (2, 8) then 2600.0
                  else 0.0
             else if toSq == (6, 2) then 1800.0
                  else if toSq == (7, 2) then 2400.0
                  else if toSq == (8, 2) then 2600.0
                  else 0.0
           -- 2. つの銀中飛車（Twin Silvers: 4七銀・6七銀への展開）
           Just (Piece p Silver) | p == plr ->
             if plr == Sente
             then if toSq == (4, 7) || toSq == (6, 7) then 2200.0
                  else if toSq == (4, 8) || toSq == (6, 8) then 1600.0
                  else 0.0
             else if toSq == (6, 3) || toSq == (4, 3) then 2200.0
                  else if toSq == (6, 2) || toSq == (4, 2) then 1600.0
                  else 0.0
           _ -> 0.0
       _ -> 0.0

-- | 居玉（Sitting King / Central King Danger）の局面ペナルティ
-- 玉が初期位置（5九 / 5一）に居座っている危険局面を厳罰化
evalSittingKingPenalty :: Player -> State -> Double
evalSittingKingPenalty p st =
  let bd = board st
      opp = opponent p
      kingSq = findKing p bd
      oppMajorCount = length [ sq | (sq, Piece owner pt) <- Map.toList bd, owner == opp, pt `elem` [Rook, PRook, Bishop, PBishop] ]
  in case kingSq of
       Just (5, 9) | p == Sente ->
         -- 先手居玉：相手の大駒がいるのに5九居玉なら超厳罰！
         -2500.0 - fromIntegral oppMajorCount * 400.0
       Just (5, 1) | p == Gote ->
         -2500.0 - fromIntegral oppMajorCount * 400.0
       -- 4八や6八など囲いへの第一歩を踏み出していれば安全
       _ -> 0.0

-- | 美濃囲い・つの銀中飛車の局面完成度評価
evalMinoAndTsunoGinFormation :: Player -> State -> Double
evalMinoAndTsunoGinFormation p st =
  let bd = board st
      kingSq = findKing p bd
      isNakabisha = Map.lookup (if p == Sente then (5, 8) else (5, 2)) bd == Just (Piece p Rook) ||
                    Map.lookup (if p == Sente then (5, 6) else (5, 4)) bd == Just (Piece p Rook)
  in if not isNakabisha
     then 0.0
     else
       let -- 美濃囲い評価
           minoScore = case kingSq of
             Just (kx, ky) | p == Sente && kx `elem` [2, 3] && ky `elem` [8, 9] -> 2400.0
             Just (kx, ky) | p == Gote  && kx `elem` [7, 8] && ky `elem` [1, 2] -> 2400.0
             Just (4, 8)   | p == Sente -> 1500.0
             Just (6, 2)   | p == Gote  -> 1500.0
             _ -> 0.0
           -- つの銀（4七銀＆6七銀）評価
           tsunoScore =
             if p == Sente
             then (if Map.lookup (4, 7) bd == Just (Piece Sente Silver) then 1200.0 else 0.0) +
                  (if Map.lookup (6, 7) bd == Just (Piece Sente Silver) then 1200.0 else 0.0)
             else (if Map.lookup (6, 3) bd == Just (Piece Gote Silver) then 1200.0 else 0.0) +
                  (if Map.lookup (4, 3) bd == Just (Piece Gote Silver) then 1200.0 else 0.0)
       in minoScore + tsunoScore
"""

idx_shuff = content.find('evalPieceShuffling :: State -> Move -> Double')
if idx_shuff == -1:
    raise ValueError("evalPieceShuffling not found")

content = content[:idx_shuff] + new_modules + '\n' + content[idx_shuff:]
print("3. Inserted new defense and castling modules.")

# 4. Integrate into scoreMove
target_sm = "strategicCommitmentBonus = evalStrategicCommitment st m"
replacement_sm = """strategicCommitmentBonus = evalStrategicCommitment st m
                   rookFileDefenseBonus = evalRookFileDefenseFlow st m
                   nakabishaCastlingBonus = evalNakabishaCastlingFlow st m"""

content = content.replace(target_sm, replacement_sm)

target_sm_ret = "+ comprehensiveAdvanceBonus + strategicCommitmentBonus"
replacement_sm_ret = "+ comprehensiveAdvanceBonus + strategicCommitmentBonus + rookFileDefenseBonus + nakabishaCastlingBonus"
content = content.replace(target_sm_ret, replacement_sm_ret)
print("4. Integrated rookFileDefenseBonus and nakabishaCastlingBonus into scoreMove.")

# 5. Integrate into evaluateState
target_eval = "comprehensiveAdvanceAdvantage = evalComprehensiveAdvancementFormation player st - evalComprehensiveAdvancementFormation opp st"
replacement_eval = """comprehensiveAdvanceAdvantage = evalComprehensiveAdvancementFormation player st - evalComprehensiveAdvancementFormation opp st
      sittingKingAdvantage = evalSittingKingPenalty player st - evalSittingKingPenalty opp st
      nakabishaCastleAdvantage = evalMinoAndTsunoGinFormation player st - evalMinoAndTsunoGinFormation opp st"""

content = content.replace(target_eval, replacement_eval)

target_eval_ret = "+ comprehensiveAdvanceAdvantage"
replacement_eval_ret = "+ comprehensiveAdvanceAdvantage + sittingKingAdvantage + nakabishaCastleAdvantage"
content = content.replace(target_eval_ret, replacement_eval_ret)
print("5. Integrated sittingKingAdvantage and nakabishaCastleAdvantage into evaluateState.")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(content)

print("Saved all updates successfully!")
