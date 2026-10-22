import sys

with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    code = f.read()

# 1. Add evalHangingPieces after evalLoosePieces
loose_target = """evalLoosePieces :: Player -> State -> Double
evalLoosePieces plr st =
  let bd = board st
      opp = opponent plr
      countLoose player =
        length [ sq | (sq, p@(Piece pPlr pt)) <- Map.toList bd
                    , pPlr == player
                    , pt `notElem` [Pawn, King] -- 王と歩以外（金銀桂香角飛）
                    , not (isPieceProtected sq p bd)
               ]
      myLoose = countLoose plr
      oppLoose = countLoose opp
  in (fromIntegral oppLoose - fromIntegral myLoose) * 40.0"""

hanging_code = """evalLoosePieces :: Player -> State -> Double
evalLoosePieces plr st =
  let bd = board st
      opp = opponent plr
      countLoose player =
        length [ sq | (sq, p@(Piece pPlr pt)) <- Map.toList bd
                    , pPlr == player
                    , pt `notElem` [Pawn, King] -- 王と歩以外（金銀桂香角飛）
                    , not (isPieceProtected sq p bd)
               ]
      myLoose = countLoose plr
      oppLoose = countLoose opp
  in (fromIntegral oppLoose - fromIntegral myLoose) * 40.0

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
  in evalPlayerHanging player - evalPlayerHanging opp"""

if loose_target in code:
    code = code.replace(loose_target, hanging_code, 1)
    print("Added evalHangingPieces successfully.")
else:
    print("ERROR: Could not find loose_target!")
    sys.exit(1)

# 2. Integrate hangingAdvantage into evaluateState
eval_target = """      materialAdvantage = evalMaterial player st * wMaterial weights
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
  in materialAdvantage + morseAdvantage + attackAdvantage + defenseAdvantage
     + kakutoKeitoAdvantage + looseAdvantage + pieceControlAdvantage
     + tempoLossAdvantage + goldKingCoordAdvantage + distanceToMateAdvantage
     + numericalAssaultAdvantage + tacticalForkAdvantage + potentialRayAdvantage"""

eval_replacement = """      materialAdvantage = evalMaterial player st * wMaterial weights
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
  in materialAdvantage + hangingAdvantage + morseAdvantage + attackAdvantage + defenseAdvantage
     + kakutoKeitoAdvantage + looseAdvantage + pieceControlAdvantage
     + tempoLossAdvantage + goldKingCoordAdvantage + distanceToMateAdvantage
     + numericalAssaultAdvantage + tacticalForkAdvantage + potentialRayAdvantage"""

if eval_target in code:
    code = code.replace(eval_target, eval_replacement, 1)
    print("Integrated hangingAdvantage into evaluateState successfully.")
else:
    print("ERROR: Could not find eval_target!")
    sys.exit(1)

# 3. Update searchQuiescence (prevent Stand Pat when Major is hanging, remove suicide drop)
quiesc_target = """searchQuiescence :: MidgameWeights -> Int -> Double -> Double -> State -> Double
searchQuiescence weights 0 _ _ st = evaluateState weights (turn st) st
searchQuiescence weights qdepth alpha beta st =
  let plr = turn st
      inCheck = isKingUnderAttack plr (board st)
  in if inCheck
     then
       -- 王手されている局面：パス（Stand Pat）は不可！全合法手脱出探索
       let rawMoves = generateMoves st
           legalEscapes = f5SyzygyFilter st rawMoves
       in if null legalEscapes
          then -10000.0 -- 逃げられなければ即詰み！
          else qLoop legalEscapes alpha beta
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
          -- 数の攻めによる突破マス判定
          oppDefenders = getPiecesCovering opp toSq (board s)
          myAttackers = getPiecesCovering plr toSq (board nextS)
          isBreakthrough = length myAttackers > length oppDefenders
          -- 飛び駒の潜在的射線の開放（ディスカバード・アタック）
          isRayUnblock = case m of
            NormalMove from _ _ ->
              any (\\(bsq, Piece bp bpt) -> bp == plr && bpt `elem` [Rook, PRook, Bishop, PBishop, Lance] && canPieceXRayTarget bsq (Piece bp bpt) from (board s)) (Map.toList (board s))
            _ -> False
      in case m of
           -- 1. 駒の捕獲、駒成り、王手、両取り、数の攻め突破、射線開放
           NormalMove _ to promo ->
             Map.member to (board s) || promo || isCheck || isFork || isBreakthrough || isRayUnblock
           -- 2. 駒の打ち込み（王手、両取り、敵玉周辺への打ち込み、要所打ち込み、数の攻め突破）
           DropMove pt to ->
             isCheck || isFork || isBreakthrough || (case findKing opp (board s) of
                           Just (kx, ky) -> abs (fst to - kx) <= 2 && abs (snd to - ky) <= 2 && pt `elem` [Gold, Silver, Rook, Bishop, Knight, Pawn]
                           Nothing -> False))

    qLoop [] curA _ = curA
    qLoop (m:ms) curA b =
      let nextSt = applyMove m st
          score = - searchQuiescence weights (qdepth - 1) (-b) (-curA) nextSt
      in if score >= b
         then b
         else qLoop ms (max curA score) b"""

quiesc_replacement = """searchQuiescence :: MidgameWeights -> Int -> Double -> Double -> State -> Double
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
         else qLoop ms (max curA score) b"""

if quiesc_target in code:
    code = code.replace(quiesc_target, quiesc_replacement, 1)
    print("Updated searchQuiescence successfully.")
else:
    print("ERROR: Could not find quiesc_target!")
    sys.exit(1)

# 4. Update searchAlphaBeta to return (Double, [Move])
alpha_target = """-- | ネガマックス α-β 探索（自玉の視点でのスコアと最善手を返す: トポロジカル・モース版）
searchAlphaBeta :: MidgameWeights -> Int -> Double -> Double -> State -> (Double, Maybe Move)
searchAlphaBeta weights 0 alpha beta st =
  (searchQuiescence weights 2 alpha beta st, Nothing)
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
     then (-10000.0, Nothing) -- 詰み
     else if null effectiveMoves
       then (0.0, Nothing)     -- 千日手引き分け
       else
         -- 枝刈り効率化のため、タダ捨て手を強烈ペナルティ化し、1手先静的評価で降順ソート
         let scoreMove m ="""

alpha_replacement = """-- | ネガマックス α-β 探索（自玉の視点でのスコアと最善読み筋PVを返す: トポロジカル・モース版）
searchAlphaBeta :: MidgameWeights -> Int -> Double -> Double -> State -> (Double, [Move])
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

if alpha_target in code:
    code = code.replace(alpha_target, alpha_replacement, 1)
    print("Updated searchAlphaBeta signature and base cases successfully.")
else:
    print("ERROR: Could not find alpha_target!")
    sys.exit(1)

# 5. Update loop in searchAlphaBeta
loop_target = """             orderedMoves = sortBy (\\m1 m2 -> compare (scoreMove m2) (scoreMove m1)) effectiveMoves
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

loop_replacement = """             orderedMoves = sortBy (\\m1 m2 -> compare (scoreMove m2) (scoreMove m1)) effectiveMoves
             -- 王手をかけられているときはすべての王手回避手を読み、それ以外は上位14手を深く読む
             topMoves = if inCheck then effectiveMoves else take (if depth >= 2 then 14 else 10) orderedMoves
         in loop topMoves alpha []
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
              else loop ms curAlpha (if null bestPv then (m : childPv) else bestPv)"""

if loop_target in code:
    code = code.replace(loop_target, loop_replacement, 1)
    print("Updated loop in searchAlphaBeta successfully.")
else:
    print("ERROR: Could not find loop_target!")
    sys.exit(1)

# 6. Update searchBestMoveIterativeWithTimeout to print full PV
ids_target = """                     -- フォールバックとして最も安全な最善手を設定
                     let fallbackMove = case effectiveMoves of (m:_) -> m; [] -> error "no effective moves"
                     bestRef <- newIORef (0.0, fallbackMove)
                 
                     -- タイムアウト付き探索 (byoyomi に応じて depth 1..31 まで探索)
                     _ <- timeout timeLimitMicros $ do
                       let runIDS d
                             | d > 36 = return ()
                             | otherwise = do
                                 let (score, maybeMv) = searchAlphaBeta weights d (-99999.0) 99999.0 state
                                 case maybeMv of
                                   Just mv | mv `elem` effectiveMoves -> do
                                     -- 厳密評価で先行評価を確定
                                     _ <- evaluate (score `seq` mv `seq` ())
                                     writeIORef bestRef (score, mv)
                                     putStrLn $ "info depth " ++ show d ++ " score cp " ++ show (round score :: Int) ++ " pv " ++ showMove mv
                                     hFlush stdout
                                     if abs score >= 9000.0
                                       then return () -- 詰みを発見した場合は直ちに停止
                                       else runIDS (d + 1)
                                   _ -> return ()
                       runIDS 1
                 
                     -- タイムアウトした場合でも、前回までに探索した最深の最善手を取得
                     (bestScore, rawBestMove) <- readIORef bestRef
                     -- 念のため安全保証：選択された手が f5FilteredMoves に含まれているか
                     let bestMove = if rawBestMove `elem` f5FilteredMoves
                                    then rawBestMove
                                    else fallbackMove
                         newCache = Map.insert (positionKey state) (bestScore, bestMove) (cache state)
                     return (Right bestMove, state { cache = newCache })"""

ids_replacement = """                     -- フォールバックとして最も安全な最善手を設定
                     let fallbackMove = case effectiveMoves of (m:_) -> m; [] -> error "no effective moves"
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

if ids_target in code:
    code = code.replace(ids_target, ids_replacement, 1)
    print("Updated searchBestMoveIterativeWithTimeout successfully.")
else:
    print("ERROR: Could not find ids_target!")
    sys.exit(1)

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(code)

print("All modifications applied successfully to ShogiUsiMorse-v4.hs!")
