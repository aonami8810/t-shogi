with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

def find_line(pattern, start=0):
    for i in range(start, len(lines)):
        if pattern in lines[i]:
            return i
    return -1

# 1. Insert evalHangingPieces after line 832
idx_loose = find_line('evalLoosePieces ::')
idx_loose_in = find_line('in (fromIntegral oppLoose', idx_loose)
print(f"Loose ends at line {idx_loose_in + 1}")

hanging_code = """
-- | 大駒・重要駒のタダ取られ（Hanging Pieces / En Prise）および大損交換危険度評価
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
  in evalPlayerHanging player - evalPlayerHanging opp
"""
hanging_lines = [l + '\n' for l in hanging_code.strip().split('\n')]
lines[idx_loose_in + 1:idx_loose_in + 1] = ['\n'] + hanging_lines + ['\n']
print("1. Added evalHangingPieces.")

# 2. Update evaluateState
idx_mat = find_line('materialAdvantage = evalMaterial')
idx_mat_in = find_line('in materialAdvantage +', idx_mat)
print(f"evaluateState from line {idx_mat + 1} to {idx_mat_in + 1}")

eval_block = """      materialAdvantage = evalMaterial player st * wMaterial weights
      hangingAdvantage = evalHangingPieces player st
      attackAdvantage = (evalMidgameAttack player st - evalMidgameAttack opp st) * wAttack weights
      defenseAdvantage = (evalMidgameDefense player st - evalMidgameDefense opp st) * wDefense weights
      kakutoKeitoAdvantage = evalKakutoKeitoDefense player st * wKakuto weights
      looseAdvantage = evalLoosePieces player st * wLoosePenalty weights
      pieceControlAdvantage = evalPieceControl player st * wPieceControl weights
      tempoLossAdvantage = evalTempoLoss player st * wTempoLoss weights
      goldKingCoordAdvantage = evalGoldKingCoordination player st * wGoldKingCoord weights
      distanceToMateAdvantage = (evalDistanceToMate player st - evalDistanceToMate opp st) * wDistanceToMate weights
      numericalAssaultAdvantage = (evalNumericalAssault player st - evalNumericalAssault opp st) * wNumericalAssault weights
      tacticalForkAdvantage = evalTacticalFork player st * wTacticalFork weights
      potentialRayAdvantage = evalPotentialRayAttack player st * wPotentialRays weights
  in materialAdvantage + hangingAdvantage + morseAdvantage + attackAdvantage + defenseAdvantage"""

lines[idx_mat:idx_mat_in + 1] = [l + '\n' for l in eval_block.split('\n')]
print("2. Updated evaluateState with hangingAdvantage.")

# 3. Update searchQuiescence
idx_q = find_line('searchQuiescence :: MidgameWeights')
idx_q_scoreMoveFast = find_line('scoreMoveFast :: State -> Move -> Double', idx_q)
print(f"searchQuiescence from line {idx_q + 1} to {idx_q_scoreMoveFast + 1}")

quiesc_block = """searchQuiescence :: MidgameWeights -> Int -> Double -> Double -> State -> Double
searchQuiescence weights 0 _ _ st = evaluateState weights (turn st) st
searchQuiescence weights qdepth alpha beta st =
  let plr = turn st
      opp = opponent plr
      curBd = board st
      inCheck = isKingUnderAttack plr curBd
      hasHangingMajor = any (\\(sq, Piece p pt) ->
        p == plr && pt `elem` [Rook, PRook, Bishop, PBishop] &&
        isSquareUnderAttack opp sq curBd && null (getPiecesCovering plr sq curBd)
        ) (Map.toList curBd)
  in if inCheck || hasHangingMajor
     then
       -- 王手または大駒がタダ取られの危機：Stand Pat を禁止し、すべての合法手・脱出手を探索！
       let rawMoves = generateMoves st
           legalMoves = f5SyzygyFilter st rawMoves
       in if null legalMoves
          then -10000.0
          else qLoop legalMoves alpha beta
     else
       -- 通常局面：Stand Pat と先手（捕獲・駒成・王手・大駒成り・突破）探索
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
          isRayUnblock = case m of
            NormalMove from _ _ ->
              any (\\(bsq, Piece bp bpt) -> bp == plr && bpt `elem` [Rook, PRook, Bishop, PBishop, Lance] && canPieceXRayTarget bsq (Piece bp bpt) from (board s)) (Map.toList (board s))
            _ -> False
      in case m of
           NormalMove _ to promo ->
             Map.member to (board s) || promo || isCheck || isFork || isBreakthrough || isRayUnblock
           DropMove pt to ->
             let isMajor = pt `elem` [Rook, Bishop]
                 oppAttackers = getPiecesCovering opp to (board nextS)
                 friendlyDefenders = getPiecesCovering plr to (board nextS)
                 isSuicideDrop = isMajor && not (null oppAttackers) && null friendlyDefenders
             in not isSuicideDrop && (isCheck || isFork || isBreakthrough || (case findKing opp (board s) of
                           Just (kx, ky) -> abs (fst to - kx) <= 2 && abs (snd to - ky) <= 2 && pt `elem` [Gold, Silver, Rook, Bishop, Knight, Pawn]
                           Nothing -> False))

    qLoop [] curA _ = curA
    qLoop (m:ms) curA b =
      let nextSt = applyMove m st
          score = - searchQuiescence weights (qdepth - 1) (-b) (-curA) nextSt
      in if score >= b
         then b
         else qLoop ms (max curA score) b
"""
lines[idx_q:idx_q_scoreMoveFast] = [l + '\n' for l in quiesc_block.strip().split('\n')] + ['\n\n']
print("3. Updated searchQuiescence.")

# 4. Update searchAlphaBeta signature and base cases
idx_ab = find_line('searchAlphaBeta :: MidgameWeights')
idx_ab_score = find_line('let scoreMove m =', idx_ab)
print(f"searchAlphaBeta start from line {idx_ab + 1} to {idx_ab_score + 1}")

ab_head = """searchAlphaBeta :: MidgameWeights -> Int -> Double -> Double -> State -> (Double, [Move])
searchAlphaBeta weights 0 alpha beta st =
  (searchQuiescence weights 4 alpha beta st, [])
searchAlphaBeta weights depth alpha beta st =
  let inCheck = isKingUnderAttack (turn st) (board st)
      rawMoves = generateMoves st
      f5Moves = f5SyzygyFilter st rawMoves
      (_sennichite, safeMoves) = partition (\\m -> 
        let nextSt = applyMove m st
        in isSennichite (positionKey nextSt) (history nextSt)
        ) f5Moves
      -- ★ 王手されている局面では一切の除外を行わずすべての合法手
      nonKakutoMoves = if inCheck
                       then safeMoves
                       else filter (\\m -> not (isPushingKakutoPawn st m) && not (isFutileLanceLift st m)) safeMoves
      effectiveMoves = if null nonKakutoMoves then safeMoves else nonKakutoMoves
  in if null f5Moves
     then (-10000.0, []) -- 詰み
     else if null effectiveMoves
       then (0.0, [])     -- 千日手引き分け
       else
         -- 枝刈り効率化のため、タダ捨て手を強烈ペナルティ化し、1手先静的評価で降順ソート
         let scoreMove m ="""
lines[idx_ab:idx_ab_score + 1] = [l + '\n' for l in ab_head.split('\n')]
print("4. Updated searchAlphaBeta head.")

# 5. Update searchAlphaBeta loop
idx_loop_call = find_line('topMoves alpha', idx_ab)
idx_section85 = find_line('8.5', idx_loop_call)
print(f"searchAlphaBeta loop from line {idx_loop_call + 1} to {idx_section85 + 1}")

loop_block = """         in loop topMoves alpha []
  where
    loop [] curAlpha bestPv = (curAlpha, bestPv)
    loop (m:ms) curAlpha bestPv =
      let nextSt = applyMove m st
          (childScore, childPv) = searchAlphaBeta weights (depth - 1) (-beta) (-curAlpha) nextSt
          score = -childScore
      in if score >= beta
         then (score, m : childPv)
         else if score > curAlpha
              then loop ms score (m : childPv)
              else loop ms curAlpha (if null bestPv then (m : childPv) else bestPv)
"""
lines[idx_loop_call:idx_section85] = [l + '\n' for l in loop_block.strip().split('\n')] + ['\n\n']
print("5. Updated searchAlphaBeta loop to return PV.")

# 6. Update searchBestMoveIterativeWithTimeout
idx_ids_fb = find_line('fallbackMove = case effectiveMoves')
idx_ids_ret = find_line('return (Right bestMove, state { cache = newCache })', idx_ids_fb)
print(f"searchBestMoveIterativeWithTimeout from line {idx_ids_fb + 1} to {idx_ids_ret + 1}")

ids_block = """                     let fallbackMove = case effectiveMoves of (m:_) -> m; [] -> error "no effective moves"
                     bestRef <- newIORef (0.0, fallbackMove, [fallbackMove])
                 
                     -- タイムアウト付き探索 (byoyomi に応じて depth 1..36 まで深掘り探索)
                     _ <- timeout timeLimitMicros $ do
                       let runIDS d
                             | d > 36 = return ()
                             | otherwise = do
                                 let (score, pv) = searchAlphaBeta weights d (-99999.0) 99999.0 state
                                 case pv of
                                   (mv:_) | mv `elem` effectiveMoves -> do
                                     _ <- evaluate (score `seq` mv `seq` ())
                                     writeIORef bestRef (score, mv, pv)
                                     let pvStr = unwords (map showMove pv)
                                     putStrLn $ "info depth " ++ show d ++ " score cp " ++ show (round score :: Int) ++ " pv " ++ pvStr
                                     hFlush stdout
                                     if abs score >= 9000.0
                                       then return () -- 詰みを発見した場合は直ちに停止
                                       else runIDS (d + 1)
                                   _ -> return ()
                       runIDS 1
                 
                     -- タイムアウトした場合でも、前回までに探索した最深の最善手を取得
                     (bestScore, rawBestMove, _finalPv) <- readIORef bestRef
                     let bestMove = if rawBestMove `elem` f5FilteredMoves
                                    then rawBestMove
                                    else fallbackMove
                         newCache = Map.insert (positionKey state) (bestScore, bestMove) (cache state)
                     return (Right bestMove, state { cache = newCache })"""

lines[idx_ids_fb:idx_ids_ret + 1] = [l + '\n' for l in ids_block.split('\n')]
print("6. Updated searchBestMoveIterativeWithTimeout.")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)

print("ALL CHANGES SUCCESSFULLY SAVED TO ShogiUsiMorse-v4.hs!")
