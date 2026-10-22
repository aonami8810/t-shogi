with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    content = f.read()

old_func = """-- | 局面評価：飛車先突破・直射被弾の危機ペナルティ
-- 相手飛車が直通していて自陣の歩受け（8七/2三）がされていない局面は、
-- 次の手番で竜を作られて即敗北するため超特大ペナルティ(-10000点)
evalRookVanguardBreachPenalty :: Player -> State -> Double
evalRookVanguardBreachPenalty p st =
  let bd = board st
      opp = opponent p
      
      -- 相手の歩が突き刺さっているのに、同歩で取らずに手抜き放置した局面
      isPawnIgnored =
        if p == Sente
        then Map.lookup (8, 6) bd == Just (Piece opp Pawn) && Map.lookup (8, 7) bd == Just (Piece Sente Pawn)
        else Map.lookup (2, 4) bd == Just (Piece opp Pawn) && Map.lookup (2, 3) bd == Just (Piece Gote Pawn)
        
      -- 相手の飛車が直射しているのに、8七/2三に歩が打たれていない無防備局面
      isRookBreached =
        if p == Sente
        then (Map.lookup (8, 6) bd == Just (Piece opp Rook) || Map.lookup (8, 5) bd == Just (Piece opp Rook)) &&
             Map.lookup (8, 7) bd == Nothing
        else (Map.lookup (2, 4) bd == Just (Piece opp Rook) || Map.lookup (2, 5) bd == Just (Piece opp Rook)) &&
             Map.lookup (2, 3) bd == Nothing
  in if isRookBreached then -10000.0 -- 飛車先直射・歩受け放置は即死ペナルティ！
     else if isPawnIgnored then -5000.0
     else 0.0"""

new_func = """-- | 局面評価：飛車先突破・直射被弾の危機ペナルティ
-- 相手飛車が直通していて自陣の歩受け（8七/2三）がされていない局面は、
-- 次の手番で竜を作られて即敗北するため超特大ペナルティ(-10000点)
-- ★ 手番側が手持ちに歩を持っている場合は、この手番で受けられるためペナルティ免除
evalRookVanguardBreachPenalty :: Player -> State -> Double
evalRookVanguardBreachPenalty p st =
  let bd = board st
      opp = opponent p
      myHands = Map.findWithDefault Map.empty p (hands st)
      hasPawnInHand = Map.findWithDefault 0 Pawn myHands > 0
      
      -- 相手の歩が突き刺さっているのに、同歩で取らずに手抜き放置した局面
      isPawnIgnored =
        if p == Sente
        then Map.lookup (8, 6) bd == Just (Piece opp Pawn) && Map.lookup (8, 7) bd == Just (Piece Sente Pawn)
        else Map.lookup (2, 4) bd == Just (Piece opp Pawn) && Map.lookup (2, 3) bd == Just (Piece Gote Pawn)
        
      -- 相手の飛車が直射していて、8七/2三に歩がない無防備局面
      isRookBreached =
        if p == Sente
        then (Map.lookup (8, 6) bd == Just (Piece opp Rook) || Map.lookup (8, 5) bd == Just (Piece opp Rook)) &&
             Map.lookup (8, 7) bd == Nothing
        else (Map.lookup (2, 4) bd == Just (Piece opp Rook) || Map.lookup (2, 5) bd == Just (Piece opp Rook)) &&
             Map.lookup (2, 3) bd == Nothing
             
      -- 自分が手番(turn st == p)かつ手持ちに歩があるなら、この手番で歩打ち受けできるため無罰
      canDefendThisTurn = turn st == p && hasPawnInHand
  in if isPawnIgnored then -5000.0
     else if isRookBreached && not canDefendThisTurn then -10000.0
     else 0.0"""

content = content.replace(old_func, new_func)

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(content)

print("Applied turn-aware vanguard defense fix successfully!")
