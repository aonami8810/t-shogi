import re

with open(r'c:\VS\Workspace\t-shogi\ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    content = f.read()

# 1. searchQuiescence の改修（王手時のStand Pat禁止、王手回避手の全探索、詰み判定-10000.0）
old_qs = """-- | 静止探索（捕獲手・王手・大駒成り・急所打ち込みを先読みして水平線効果を解消）
searchQuiescence :: MidgameWeights -> Int -> Double -> Double -> State -> Double
searchQuiescence weights 0 _ _ st = evaluateState weights (turn st) st
searchQuiescence weights qdepth alpha beta st =
  let standPat = evaluateState weights (turn st) st
  in if standPat >= beta
     then beta
     else
       let curAlpha = max alpha standPat
           rawMoves = generateMoves st
           tacticalMoves = filter (isTacticalMove st) rawMoves
           f5Tacticals = f5SyzygyFilter st tacticalMoves
       in if null f5Tacticals
          then curAlpha
          else qLoop f5Tacticals curAlpha beta
  where
    isTacticalMove s m =
      let opp = opponent (turn s)
          nextS = applyMove m s
          isCheck = isKingUnderAttack opp (board nextS)
      in case m of
           -- 1. 相手の駒を取る手、大駒成り、王手
           NormalMove _ to promo ->
             Map.member to (board s) || promo || isCheck
           -- 2. 持ち駒の打ち込み（王手、または敵玉周辺への打ち込み）
           DropMove pt to ->
             isCheck || (case findKing opp (board s) of
                           Just (kx, ky) -> abs (fst to - kx) <= 2 && abs (snd to - ky) <= 2 && pt `elem` [Gold, Silver, Rook, Bishop, Knight]
                           Nothing -> False)

    qLoop [] curA _ = curA
    qLoop (m:ms) curA b =
      let nextSt = applyMove m st
          score = - searchQuiescence weights (qdepth - 1) (-b) (-curA) nextSt
      in if score >= b
         then b
         else qLoop ms (max curA score) b"""

new_qs = """-- | 静止探索（捕獲手・王手・大駒成り・急所打ち込みを先読みして水平線効果を解消）
-- ★ 王手（Check）されている局面では Stand Pat（パス）を禁止し、王手解除手を全探索して詰み判定！
searchQuiescence :: MidgameWeights -> Int -> Double -> Double -> State -> Double
searchQuiescence weights 0 _ _ st = evaluateState weights (turn st) st
searchQuiescence weights qdepth alpha beta st =
  let plr = turn st
      inCheck = isKingUnderAttack plr (board st)
  in if inCheck
     then
       -- 王手されている局面：パス（Stand Pat）は不可！王手を解除する手を探索
       let rawMoves = generateMoves st
           legalEscapes = f5SyzygyFilter st rawMoves
       in if null legalEscapes
          then -10000.0 -- 王手を解除できなければ即詰み！
          else qLoop legalEscapes alpha beta
     else
       -- 通常局面：Stand Pat 判定後に戦術手（捕獲・大駒成り・王手・打ち込み・突破）を探索
       let standPat = evaluateState weights plr st
       in if standPat >= beta
          then beta
          else
            let curAlpha = max alpha standPat
                rawMoves = generateMoves st
                tacticalMoves = filter (isTacticalMove st) rawMoves
                f5Tacticals = f5SyzygyFilter st tacticalMoves
            in if null f5Tacticals
               then curAlpha
               else qLoop f5Tacticals curAlpha beta
  where
    isTacticalMove s m =
      let plr = turn s
          opp = opponent plr
          nextS = applyMove m s
          isCheck = isKingUnderAttack opp (board nextS)
          (toSq, ptMoved, isPromo) = case m of
            NormalMove _ to p -> (to, Pawn, p)
            DropMove pt to    -> (to, pt, False)
          -- 数の攻めによる突破マスか
          oppDefenders = getPiecesCovering opp toSq (board s)
          myAttackers = getPiecesCovering plr toSq (board nextS)
          isBreakthrough = length myAttackers > length oppDefenders
      in case m of
           -- 1. 相手の駒を取る手、大駒成り、王手、数の攻め突破
           NormalMove _ to promo ->
             Map.member to (board s) || promo || isCheck || isBreakthrough
           -- 2. 持ち駒の打ち込み（王手、敵玉周辺への打ち込み、数の攻め突破）
           DropMove pt to ->
             isCheck || isBreakthrough || (case findKing opp (board s) of
                           Just (kx, ky) -> abs (fst to - kx) <= 2 && abs (snd to - ky) <= 2 && pt `elem` [Gold, Silver, Rook, Bishop, Knight, Pawn]
                           Nothing -> False)

    qLoop [] curA _ = curA
    qLoop (m:ms) curA b =
      let nextSt = applyMove m st
          score = - searchQuiescence weights (qdepth - 1) (-b) (-curA) nextSt
      in if score >= b
         then b
         else qLoop ms (max curA score) b"""

# 2. searchAlphaBeta で王手されているときの枝刈り解除 ＆ scoreMove の大駒成り捨て強化
old_sab = """              orderedMoves = sortBy (\\m1 m2 -> compare (scoreMove m2) (scoreMove m1)) effectiveMoves
              topMoves = take (if depth >= 2 then 10 else 8) orderedMoves
          in loop topMoves alpha Nothing"""

new_sab = """              orderedMoves = sortBy (\\m1 m2 -> compare (scoreMove m2) (scoreMove m1)) effectiveMoves
              -- 王手をかけられているときはすべての王手回避手を読み、それ以外は上位14手を深く読む
              inCheck = isKingUnderAttack (turn st) (board st)
              topMoves = if inCheck then effectiveMoves else take (if depth >= 2 then 14 else 10) orderedMoves
          in loop topMoves alpha Nothing"""

# 3. scoreMove で大駒成り捨て・サクリファイス・王手・数の攻めのスコアをさらに強化
old_scoremove = """                    opp = opponent (turn st)
                    isCheck = isKingUnderAttack opp (board nextSt)
                    checkBonus = if isCheck then 350.0 else 0.0
                    
                    -- 打ち込み・大駒の成り・飛車先歩突きボーナス
                    tacticalBonus = case m of
                      -- 大駒の成り
                      NormalMove _ _ True -> 180.0
                      -- 敵玉近傍への大駒・金銀の打ち込み
                      DropMove pt to ->
                        case findKing opp (board st) of
                          Just (kx, ky) | abs (fst to - kx) <= 2 && abs (snd to - ky) <= 2 ->
                            if pt `elem` [Gold, Silver, Rook, Bishop, Knight] then 260.0 else 90.0
                          _ -> 60.0
                      -- 飛車先歩突き・角道オープン
                      NormalMove (2, 7) (2, 6) _ | turn st == Sente -> 280.0
                      NormalMove (2, 6) (2, 5) _ | turn st == Sente -> 240.0
                      NormalMove (8, 3) (8, 4) _ | turn st == Gote  -> 280.0
                      NormalMove (8, 4) (8, 5) _ | turn st == Gote  -> 240.0
                      NormalMove (7, 7) (7, 6) _ | turn st == Sente -> 220.0
                      NormalMove (3, 3) (3, 4) _ | turn st == Gote  -> 220.0
                      _ -> 0.0"""

new_scoremove = """                    opp = opponent (turn st)
                    isCheck = isKingUnderAttack opp (board nextSt)
                    checkBonus = if isCheck then 400.0 else 0.0
                    
                    -- 大駒の成り捨て・重要駒破壊・打ち込み・数の攻めボーナス
                    tacticalBonus = case m of
                      -- 大駒の成り（王手を伴う成り捨てなら超特大加点！）
                      NormalMove _ _ True -> if isCheck then 500.0 else 240.0
                      -- 敵玉近傍への大駒・金銀の打ち込み（王手ならさらに加点）
                      DropMove pt to ->
                        let baseDrop = case findKing opp (board st) of
                              Just (kx, ky) | abs (fst to - kx) <= 2 && abs (snd to - ky) <= 2 ->
                                if pt `elem` [Gold, Silver, Rook, Bishop, Knight] then 320.0 else 120.0
                              _ -> 60.0
                        in if isCheck then baseDrop + 200.0 else baseDrop
                      -- 飛車先歩突き・角道オープン
                      NormalMove (2, 7) (2, 6) _ | turn st == Sente -> 280.0
                      NormalMove (2, 6) (2, 5) _ | turn st == Sente -> 240.0
                      NormalMove (8, 3) (8, 4) _ | turn st == Gote  -> 280.0
                      NormalMove (8, 4) (8, 5) _ | turn st == Gote  -> 240.0
                      NormalMove (7, 7) (7, 6) _ | turn st == Sente -> 220.0
                      NormalMove (3, 3) (3, 4) _ | turn st == Gote  -> 220.0
                      _ -> 0.0"""

norm_content = content.replace('\r\n', '\n')

def do_replace(src, tgt, rep, name):
    norm_tgt = tgt.replace('\r\n', '\n')
    norm_rep = rep.replace('\r\n', '\n')
    if norm_tgt in src:
        src = src.replace(norm_tgt, norm_rep)
        print(f"SUCCESS: {name}")
    else:
        print(f"FAILED: {name}")
    return src

norm_content = do_replace(norm_content, old_qs, new_qs, "searchQuiescence")
norm_content = do_replace(norm_content, old_sab, new_sab, "searchAlphaBeta inCheck topMoves")
norm_content = do_replace(norm_content, old_scoremove, new_scoremove, "scoreMove tactical bonuses")

with open(r'c:\VS\Workspace\t-shogi\ShogiUsiMorse-v4.hs', 'w', encoding='utf-8', newline='\n') as f:
    f.write(norm_content)

print("Check quiescence and tactical updates completed.")
