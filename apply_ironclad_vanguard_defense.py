with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    content = f.read()

# 1. Update evalRookFileDefenseFlow with top-priority emergency bonuses
old_rook_def = """-- | 飛車先の受け（Rook-File Interception Defense）：
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
            else 0.0"""

new_rook_def = """-- | 飛車先の受け（Rook-File Interception Defense）：
-- 1. 相手の飛車先突き（8六歩 / 2四歩）に対する「同歩（Capture Vanguard Pawn）」
-- 2. 飛車直射局面における「8七歩打ち（P*8g）」「2三歩打ち（P*2c）」の絶対的歩受け
-- ★ 迎撃・歩受け放置は即敗北（竜作り被弾）に直結するため、玉囲いをも圧倒する最高優先度(+7000点 / -10000点)を付与
evalRookFileDefenseFlow :: State -> Move -> Double
evalRookFileDefenseFlow st m =
  let plr = turn st
      opp = opponent plr
      curBd = board st
      nextSt = applyMove m st
      nextBd = board nextSt
      
      -- 現在、相手の飛車先歩（8六歩 / 2四歩）が自陣に突き刺さっているか
      oppPawnBreaching =
        if plr == Sente
        then Map.lookup (8, 6) curBd == Just (Piece opp Pawn)
        else Map.lookup (2, 4) curBd == Just (Piece opp Pawn)
        
      -- 現在、相手の飛車（8六飛 / 2四飛）が直射しているか
      oppRookBreaching =
        if plr == Sente
        then (Map.lookup (8, 6) curBd == Just (Piece opp Rook) || Map.lookup (8, 5) curBd == Just (Piece opp Rook)) &&
             Map.lookup (8, 7) curBd == Nothing
        else (Map.lookup (2, 4) curBd == Just (Piece opp Rook) || Map.lookup (2, 5) curBd == Just (Piece opp Rook)) &&
             Map.lookup (2, 3) curBd == Nothing
  in case m of
       -- A. 相手の飛車先直射に対する「8七歩打ち（P*8g）」「2三歩打ち（P*2c）」
       DropMove Pawn toSq ->
         let isSente87Drop = plr == Sente && toSq == (8, 7) && oppRookBreaching
             isGote23Drop  = plr == Gote  && toSq == (2, 3) && oppRookBreaching
         in if isSente87Drop || isGote23Drop
            then 7500.0 -- ★ 飛車先の直射を止める絶対の歩受け手筋！竜作りを完全阻止！
            else 0.0

       -- B. 相手の飛車先歩に対する「同歩（Capture Vanguard Pawn）」
       NormalMove from toSq _ ->
         let isSenteCapture86 = plr == Sente && from == (8, 7) && toSq == (8, 6) && oppPawnBreaching
             isGoteCapture24  = plr == Gote  && from == (2, 3) && toSq == (2, 4) && oppPawnBreaching
         in if isSenteCapture86 || isGoteCapture24
            then 7000.0 -- ★ 相手の飛車先突破を同歩で迎え撃つ絶対の迎撃手筋！
            -- 飛車先が突き刺さっているのに手抜き（玉囲い等）をして放置した場合は超厳罰！
            else if oppPawnBreaching || oppRookBreaching
            then -10000.0 -- 飛車先突破を放置して竜を作られる即敗北手抜きを完全排除！
            else 0.0
       _ -> if oppRookBreaching then -10000.0 else 0.0"""

content = content.replace(old_rook_def, new_rook_def)
print("1. Replaced evalRookFileDefenseFlow with supreme emergency priority.")

# 2. Add evalRookVanguardBreachPenalty to evaluateState to reflect vanguard breach in position evaluation
vanguard_penalty_func = """-- | 局面評価：飛車先突破・直射被弾の危機ペナルティ
-- 相手の歩が8六/2四に突き出されているのに放置されている、あるいは相手飛車が直通している局面を厳罰化
evalRookVanguardBreachPenalty :: Player -> State -> Double
evalRookVanguardBreachPenalty p st =
  let bd = board st
      opp = opponent p
      isBreached =
        if p == Sente
        then (Map.lookup (8, 6) bd == Just (Piece opp Pawn) && Map.lookup (8, 7) bd == Just (Piece Sente Pawn)) ||
             ((Map.lookup (8, 6) bd == Just (Piece opp Rook) || Map.lookup (8, 5) bd == Just (Piece opp Rook)) && Map.lookup (8, 7) bd == Nothing)
        else (Map.lookup (2, 4) bd == Just (Piece opp Pawn) && Map.lookup (2, 3) bd == Just (Piece Gote Pawn)) ||
             ((Map.lookup (2, 4) bd == Just (Piece opp Rook) || Map.lookup (2, 5) bd == Just (Piece opp Rook)) && Map.lookup (2, 3) bd == Nothing)
  in if isBreached then -6000.0 else 0.0
"""

idx_shuff = content.find('evalPieceShuffling :: State -> Move -> Double')
content = content[:idx_shuff] + vanguard_penalty_func + '\n' + content[idx_shuff:]
print("2. Inserted evalRookVanguardBreachPenalty.")

# 3. Integrate evalRookVanguardBreachPenalty into evaluateState
target_eval = "nakabishaCastleAdvantage = evalMinoAndTsunoGinFormation player st - evalMinoAndTsunoGinFormation opp st"
replacement_eval = """nakabishaCastleAdvantage = evalMinoAndTsunoGinFormation player st - evalMinoAndTsunoGinFormation opp st
      vanguardBreachAdvantage = evalRookVanguardBreachPenalty player st - evalRookVanguardBreachPenalty opp st"""

content = content.replace(target_eval, replacement_eval)

target_eval_ret = "+ nakabishaCastleAdvantage"
replacement_eval_ret = "+ nakabishaCastleAdvantage + vanguardBreachAdvantage"
content = content.replace(target_eval_ret, replacement_eval_ret)
print("3. Integrated vanguardBreachAdvantage into evaluateState.")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(content)

print("Saved all updates successfully!")
