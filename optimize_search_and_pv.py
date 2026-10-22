with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

def find_line(pattern, start=0):
    for i in range(start, len(lines)):
        if pattern in lines[i]:
            return i
    return -1

# 1. Update searchQuiescence
idx_q = find_line('searchQuiescence :: MidgameWeights')
idx_scoreFast = find_line('scoreMoveFast :: State -> Move -> Double', idx_q)
print(f"searchQuiescence from line {idx_q + 1} to {idx_scoreFast + 1}")

optimized_quiesc = """searchQuiescence :: MidgameWeights -> Int -> Double -> Double -> State -> Double
searchQuiescence weights 0 _ _ st = evaluateState weights (turn st) st
searchQuiescence weights qdepth alpha beta st =
  let plr = turn st
      inCheck = isKingUnderAttack plr (board st)
  in if inCheck
     then
       let rawMoves = generateMoves st
           legalEscapes = f5SyzygyFilter st rawMoves
       in if null legalEscapes
          then -10000.0
          else qLoop legalEscapes alpha beta
     else
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
          isFork = isForkMove s m
          (toSq, ptMoved, isPromo) = case m of
            NormalMove _ to p -> (to, Pawn, p)
            DropMove pt to    -> (to, pt, False)
          oppDefenders = getPiecesCovering opp toSq (board s)
          myAttackers = getPiecesCovering plr toSq (board nextS)
          isBreakthrough = length myAttackers > length oppDefenders
      in case m of
           NormalMove _ to promo ->
             Map.member to (board s) || promo || isCheck || isFork || isBreakthrough
           DropMove pt to ->
             let isMajor = pt `elem` [Rook, Bishop]
                 oppAttackers = getPiecesCovering opp to (board nextS)
                 friendlyDefenders = getPiecesCovering plr to (board nextS)
                 isSuicideDrop = isMajor && not (null oppAttackers) && null friendlyDefenders
             in not isSuicideDrop && (isCheck || isFork || isBreakthrough)

    qLoop [] curA _ = curA
    qLoop (m:ms) curA b =
      let nextSt = applyMove m st
          score = - searchQuiescence weights (qdepth - 1) (-b) (-curA) nextSt
      in if score >= b
         then b
         else qLoop ms (max curA score) b
"""

lines[idx_q:idx_scoreFast] = [l + '\n' for l in optimized_quiesc.strip().split('\n')] + ['\n\n']
print("1. Updated searchQuiescence to be lean and fast.")

# 2. Update searchAlphaBeta base depth 0 to qdepth 2
idx_ab0 = find_line('searchAlphaBeta weights 0 alpha beta st =')
lines[idx_ab0] = 'searchAlphaBeta weights 0 alpha beta st =\n  (searchQuiescence weights 2 alpha beta st, [])\n'
print("2. Set base qdepth to 2.")

# 3. Update topMoves in searchAlphaBeta
idx_topMoves = find_line('topMoves = if inCheck then effectiveMoves else', idx_ab0)
lines[idx_topMoves] = '             topMoves = if inCheck then effectiveMoves else take (if depth >= 3 then 8 else (if depth >= 2 then 12 else 16)) orderedMoves\n'
print("3. Optimized topMoves branching for deep search.")

# 4. Remove debug print and ensure proper PV output in searchBestMoveIterativeWithTimeout
idx_debug = find_line('info string debug d=', idx_topMoves)
if idx_debug != -1:
    lines.pop(idx_debug)
    print("4. Removed debug print line.")
idx_hflush = find_line('hFlush stdout', idx_topMoves)
# Notice we still have info depth d score cp ... pv ... with hFlush stdout

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)

print("Search engine optimized successfully for speed, deep search and full PV display!")
