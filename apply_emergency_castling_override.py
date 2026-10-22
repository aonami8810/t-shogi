with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    content = f.read()

old_nakabisha_flow = """-- | 中飛車における美濃囲い＆つの銀（Twin Silvers Central Rook）の形成評価
evalNakabishaCastlingFlow :: State -> Move -> Double
evalNakabishaCastlingFlow st m =
  let plr = turn st
      curBd = board st
      hist = moveHistory st
      isNakabishaMode = any (\\mvStr -> mvStr == "2h5h" || mvStr == "5g5f") hist ||
                        Map.lookup (if plr == Sente then (5, 8) else (5, 2)) curBd == Just (Piece plr Rook)
  in if not isNakabishaMode
     then 0.0
     else case m of"""

new_nakabisha_flow = """-- | 中飛車における美濃囲い＆つの銀（Twin Silvers Central Rook）の形成評価
evalNakabishaCastlingFlow :: State -> Move -> Double
evalNakabishaCastlingFlow st m =
  let plr = turn st
      opp = opponent plr
      curBd = board st
      hist = moveHistory st
      isNakabishaMode = any (\\mvStr -> mvStr == "2h5h" || mvStr == "5g5f") hist ||
                        Map.lookup (if plr == Sente then (5, 8) else (5, 2)) curBd == Just (Piece plr Rook)
      -- 相手の飛車先が突き刺さっている緊急事態には玉囲いどころではない（迎撃最優先！）
      oppPawnBreaching =
        if plr == Sente then Map.lookup (8, 6) curBd == Just (Piece opp Pawn)
        else Map.lookup (2, 4) curBd == Just (Piece opp Pawn)
      oppRookBreaching =
        if plr == Sente then (Map.lookup (8, 6) curBd == Just (Piece opp Rook) || Map.lookup (8, 5) curBd == Just (Piece opp Rook)) && Map.lookup (8, 7) curBd == Nothing
        else (Map.lookup (2, 4) curBd == Just (Piece opp Rook) || Map.lookup (2, 5) curBd == Just (Piece opp Rook)) && Map.lookup (2, 3) curBd == Nothing
  in if not isNakabishaMode || oppPawnBreaching || oppRookBreaching
     then 0.0 -- 飛車先突破危機の間は玉囲いボーナスを停止し、迎撃を絶対優先！
     else case m of"""

content = content.replace(old_nakabisha_flow, new_nakabisha_flow)

# Also in evalRookFileDefenseFlow: make capture move bonus massive (+9000)
old_capture_bonus = """         if isSenteCapture86 || isGoteCapture24
            then 7000.0 -- ★ 相手の飛車先突破を同歩で迎え撃つ絶対の迎撃手筋！"""

new_capture_bonus = """         if isSenteCapture86 || isGoteCapture24
            then 9500.0 -- ★ 相手の飛車先突破を同歩で迎え撃つ絶対の迎撃手筋！"""

content = content.replace(old_capture_bonus, new_capture_bonus)

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(content)

print("Applied emergency castling override successfully!")
