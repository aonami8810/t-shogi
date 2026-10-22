with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    content = f.read()

# 1. searchPunishmentIDS512 の呼び出しで深さを 512 に
old_punish_call = "(refutedMvSeq, qegtbHit, newQ) = searchPunishmentIDS512 devSt qAcc 7"
new_punish_call = "(refutedMvSeq, qegtbHit, newQ) = searchPunishmentIDS512 devSt qAcc 512"

content = content.replace(old_punish_call, new_punish_call)

# 2. extendEquilibriumToIdeal の定義を extendEquilibriumToIdeal512 に置き換え
old_extend_def = '''extendEquilibriumToIdeal :: State -> DynamicQEGTB -> Int -> [Move] -> ([Move], Bool, DynamicQEGTB)
extendEquilibriumToIdeal s q 0 acc = (reverse acc, False, q)
extendEquilibriumToIdeal s q remSteps acc
  | isCheckmatePosition s =
      let dummyMv = DropMove Gold (5, 2)
          newQ = extractAndBackpropagateQEGTB s dummyMv 0 q
      in (reverse acc, True, newQ)
  | otherwise =
      case matchQEGTB s q of
        Just (_, mateM) -> (reverse (mateM : acc), True, q)
        Nothing ->
          case detectMidgameTsumero (turn s) s q of
            Just (_, tsumeroM) ->
              let nextSt = applyMove tsumeroM s
              in extendEquilibriumToIdeal nextSt q (remSteps - 1) (tsumeroM : acc)
            Nothing ->
              let rawLegalMoves = f5SyzygyFilter s (generateMoves s)
                  filteredMoves = filter (not . isTempoLossMove s) rawLegalMoves
                  legalMoves = if null filteredMoves then rawLegalMoves else filteredMoves
              in case legalMoves of
                   [] -> (reverse acc, False, q)
                   _  ->
                     let scored = sortBy (\\m1 m2 -> compare (scoreMoveStrategic s m2) (scoreMoveStrategic s m1)) legalMoves
                         bestM = head scored
                         nextSt = applyMove bestM s
                     in extendEquilibriumToIdeal nextSt q (remSteps - 1) (bestM : acc)'''

new_extend_def = '''-- | 最急降下モース勾配流により、自己対局で本筋局面から終局・最近傍イデアル（QEGTB/詰み/詰めろ）まで最大512手まで延伸
extendEquilibriumToIdeal512 :: State -> DynamicQEGTB -> Int -> Map.Map PositionKey Int -> [Move] -> ([Move], Bool, DynamicQEGTB)
extendEquilibriumToIdeal512 s q 0 _ acc = (reverse acc, False, q)
extendEquilibriumToIdeal512 s q remSteps visited acc
  | isCheckmatePosition s =
      let dummyMv = DropMove Gold (5, 2)
          newQ = extractAndBackpropagateQEGTB s dummyMv 0 q
      in (reverse acc, True, newQ)
  | otherwise =
      case matchQEGTB s q of
        Just (_, mateM) -> (reverse (mateM : acc), True, q)
        Nothing ->
          case detectMidgameTsumero (turn s) s q of
            Just (_, tsumeroM) ->
              let nextSt = applyMove tsumeroM s
                  nextVis = Map.insertWith (+) (positionKey nextSt) 1 visited
              in extendEquilibriumToIdeal512 nextSt q (remSteps - 1) nextVis (tsumeroM : acc)
            Nothing ->
              let rawLegalMoves = f5SyzygyFilter s (generateMoves s)
                  filteredMoves = filter (not . isTempoLossMove s) rawLegalMoves
                  legalMoves = if null filteredMoves then rawLegalMoves else filteredMoves
                  nonLoopMoves = filter (\\m ->
                    let k = positionKey (applyMove m s)
                    in Map.findWithDefault 0 k visited < 1
                    ) legalMoves
                  candidateMoves = if null nonLoopMoves then legalMoves else nonLoopMoves
              in case candidateMoves of
                   [] -> (reverse acc, False, q)
                   _  ->
                     let scored = sortBy (\\m1 m2 -> compare (scoreMoveStrategic s m2) (scoreMoveStrategic s m1)) candidateMoves
                         bestM = head scored
                         nextSt = applyMove bestM s
                         nextVis = Map.insertWith (+) (positionKey nextSt) 1 visited
                     in extendEquilibriumToIdeal512 nextSt q (remSteps - 1) nextVis (bestM : acc)'''

content = content.replace(old_extend_def, new_extend_def)

# 3. 呼び出し箇所を更新
old_extend_call = '''          let lastSt = last states
              (extendedMoves, isEquilClosed, qegtbAfterEquil) =
                extendEquilibriumToIdeal lastSt updatedQEGTB 15 []'''

new_extend_call = '''          let lastSt = last states
              totalBaseMoves = length moves
              remStepsTo512 = max 0 (512 - totalBaseMoves)
              (extendedMoves, isEquilClosed, qegtbAfterEquil) =
                extendEquilibriumToIdeal512 lastSt updatedQEGTB remStepsTo512 Map.empty []'''

content = content.replace(old_extend_call, new_extend_call)

with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
    f.write(content)

print("SUCCESS: Updated GenerateStratifiedCompleteBook.hs for 512-ply steepest descent self-play!")
