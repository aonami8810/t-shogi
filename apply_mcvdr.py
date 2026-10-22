import re

file_path = "c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs"
with open(file_path, "r", encoding="utf-8") as f:
    content = f.read()

target = """             orderedMoves = sortBy (\\m1 m2 -> compare (scoreMove m2) (scoreMove m1)) effectiveMoves
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

replacement = """             orderedMoves = sortBy (\\m1 m2 -> compare (scoreMove m2) (scoreMove m1)) effectiveMoves
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
          qegtbCheck = if inCheck then Nothing else lookupQuotientTablebase nextSt
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
             let isCritical = idx == 0 || isCheck || isCapture || isPromo || inCheck
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

# Also update runIDS depth to 36
target_depth = "d > 31 = return ()"
replacement_depth = "d > 36 = return ()"

normalized_content = content.replace("\r\n", "\n")
normalized_target = target.replace("\r\n", "\n")

if normalized_target in normalized_content:
    new_content = normalized_content.replace(normalized_target, replacement)
    new_content = new_content.replace(target_depth, replacement_depth)
    with open(file_path, "w", encoding="utf-8", newline="\n") as f:
        f.write(new_content)
    print("SUCCESS: Search engine upgraded with MC-VDR and Depth 36!")
else:
    print("ERROR: Target block not found in content!")
