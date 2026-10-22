file_path = "c:/VS/Workspace/t-shogi/GenerateStratifiedCompleteBook.hs"
with open(file_path, "r", encoding="utf-8") as f:
    content = f.read()

# Replace searchPunishmentIDS512 devSt qAcc 15 with maxDepth 7 for ultra-fast branch closure
content = content.replace("searchPunishmentIDS512 devSt qAcc 15", "searchPunishmentIDS512 devSt qAcc 7")

# Add extendEquilibriumToIdeal definition right before runStratifiedBookGeneration
greedy_ext_code = """-- | 最急降下法により、本筋局面から最近傍イデアル（QEGTB/詰み/詰めろ）まで 1次元的に延伸 (超高速)
extendEquilibriumToIdeal :: State -> DynamicQEGTB -> Int -> [Move] -> ([Move], Bool, DynamicQEGTB)
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
                     let scored = sortBy (\\m1 m2 ->
                           let s1 = evaluateStratifiedState (turn s) (applyMove m1 s)
                               s2 = evaluateStratifiedState (turn s) (applyMove m2 s)
                           in compare s2 s1) legalMoves
                         bestM = head scored
                         nextSt = applyMove bestM s
                     in extendEquilibriumToIdeal nextSt q (remSteps - 1) (bestM : acc)

"""

target_gen_head = "runStratifiedBookGeneration :: IO ([BFSPath], DynamicQEGTB)"
content = content.replace(target_gen_head, greedy_ext_code + target_gen_head)

# Replace the lastSt call to use extendEquilibriumToIdeal
old_lastst = """        -- ★ 本筋自身を30手目以降〜終局・最近傍イデアル（QEGTB詰み）まで完全延伸！
        let lastSt = last states
            (extendedMoves, isEquilClosed, qegtbAfterEquil) =
              searchPunishmentIDS512 lastSt updatedQEGTB 15
            fullEquilibriumMoves = moves ++ extendedMoves
            equilibriumPath = BFSPath pathName fullEquilibriumMoves isEquilClosed (length extendedMoves) sampleStratum"""

new_lastst = """        -- ★ 本筋自身を30手目以降〜終局・最近傍イデアル（QEGTB詰み）まで完全延伸！
        let lastSt = last states
            (extendedMoves, isEquilClosed, qegtbAfterEquil) =
              extendEquilibriumToIdeal lastSt updatedQEGTB 15 []
            fullEquilibriumMoves = moves ++ extendedMoves
            equilibriumPath = BFSPath pathName fullEquilibriumMoves isEquilClosed (length extendedMoves) sampleStratum"""

content = content.replace(old_lastst, new_lastst)

with open(file_path, "w", encoding="utf-8", newline="\n") as f:
    f.write(content)

print("Applied extendEquilibriumToIdeal and fast branch closing!")
