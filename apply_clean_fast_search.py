file_path = "c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs"
with open(file_path, "r", encoding="utf-8") as f:
    content = f.read()

# 1. Update scoreMove to fast-path when depth >= 4
old_score_head = """         let scoreMove m =
               let isBlunder = isBlunderMove st m"""

new_score_head = """         let scoreMove m =
               if depth >= 4
               then
                 let (toSq, isPromo) = case m of
                       NormalMove _ t p -> (t, p)
                       DropMove _ t    -> (t, False)
                     capVal = case Map.lookup toSq (board st) of
                       Just (Piece _ pt) -> pieceValue pt * 8.0
                       Nothing           -> 0.0
                     promoVal = if isPromo then 400.0 else 0.0
                     nextSt = applyMove m st
                     checkVal = if isKingUnderAttack (opponent (turn st)) (board nextSt) then 600.0 else 0.0
                 in capVal + promoVal + checkVal
               else
                 let isBlunder = isBlunderMove st m"""

content = content.replace(old_score_head, new_score_head)

# 2. Update loop to MC-VDR with inline QEGTB and dynamic topMoves pruning
old_loop_block = """              orderedMoves = sortBy (\\m1 m2 -> compare (scoreMove m2) (scoreMove m1)) effectiveMoves
              -- 王手をかけられているときはすべての王手回避手を読み、それ以外は上位14手を深く読む
              topMoves = if inCheck then effectiveMoves else take (if depth >= 2 then 14 else 10) orderedMoves
          in loop topMoves alpha Nothing
  where
    loop [] curAlpha bestMv = (curAlpha, bestMv)
    loop (m:ms) curAlpha bestMv =
      let nextSt = applyMove m st
          (childScore, _) = searchAlphaBeta weights (depth - 1) (-beta) (-curAlpha) nextSt
          score = -childScore
      in if score >= beta
         then (score, Just m)
         else if score > curAlpha
              then loop ms score (Just m)
              else loop ms curAlpha (if isJust bestMv then bestMv else Just m)"""

new_loop_block = """              orderedMoves = sortBy (\\m1 m2 -> compare (scoreMove m2) (scoreMove m1)) effectiveMoves
              -- ★ トポロジカル・モース動的枝刈り (Morse Topological Move Pruning)
              topMoves = if inCheck
                         then orderedMoves
                         else if depth >= 12
                         then take 3 orderedMoves
                         else if depth >= 6
                         then take 5 orderedMoves
                         else if depth >= 2
                         then take 8 orderedMoves
                         else take 12 orderedMoves
          in loop (zip topMoves [0..]) alpha Nothing
  where
    curInCheck = isKingUnderAttack (turn st) (board st)
    loop [] curAlpha bestMv = (curAlpha, bestMv)
    loop ((m, idx):ms) curAlpha bestMv =
      let nextSt = applyMove m st
          opp = opponent (turn st)
          nextBd = board nextSt
          isCheck = isKingUnderAttack opp nextBd
          isCapture = case m of
            NormalMove _ to _ -> Map.member to (board st)
            _                 -> False
          isPromo = case m of
            NormalMove _ _ True -> True
            _                   -> False
          
          -- ★ 1. 商空間エンドゲームテーブル（QEGTB）インライン短絡
          qegtbCheck = if curInCheck then Nothing else lookupQuotientTablebase nextSt
      in case qegtbCheck of
           Just (dtm, _) ->
             let mateScore = 9800.0 - fromIntegral dtm * 10.0
             in if mateScore >= beta
                then (mateScore, Just m)
                else if mateScore > curAlpha
                     then loop ms mateScore (Just m)
                     else loop ms curAlpha (if isJust bestMv then bestMv else Just m)
           Nothing ->
             -- ★ 2. モース臨界度に基づく可変深さ探索 (Morse Variable Depth Reduction: MC-VDR)
             let isCritical = idx == 0 || isCheck || isCapture || isPromo || curInCheck
                 reducedDepth = if isCritical
                                then depth - 1
                                else if idx <= 2
                                then max 0 (depth - 2)
                                else max 0 (depth - 3 - (idx `div` 3))
                 
                 (childScore, _) = searchAlphaBeta weights reducedDepth (-beta) (-curAlpha) nextSt
                 score = -childScore
             in if score >= beta
                then (score, Just m)
                else if score > curAlpha
                     then loop ms score (Just m)
                     else loop ms curAlpha (if isJust bestMv then bestMv else Just m)"""

content = content.replace(old_loop_block, new_loop_block)

# 3. Update IDS depth to 36
content = content.replace("d > 31 = return ()", "d > 36 = return ()")

with open(file_path, "w", encoding="utf-8", newline="\n") as f:
    f.write(content)

print("Applied clean fast search optimization successfully!")
