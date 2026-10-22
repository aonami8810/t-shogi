file_path = "c:/VS/Workspace/t-shogi/GenerateStratifiedCompleteBook.hs"
with open(file_path, "r", encoding="utf-8") as f:
    content = f.read()

# Replace branchBudget to 2
content = content.replace("let branchBudget = if idx >= 24 then 4 else 2", "let branchBudget = 2")

# In searchPunishmentIDS512, make scoredMoves fast by using simple piece/threat evaluation
old_scoring = """                        let scoredMoves = sortBy (\\m1 m2 ->
                              let s1 = evaluateStratifiedState (turn s) (applyMove m1 s)
                                  s2 = evaluateStratifiedState (turn s) (applyMove m2 s)
                              in compare s2 s1) nonLoopMoves"""

new_scoring = """                        let scoreM m =
                              let (toSq, isPromo) = case m of
                                    NormalMove _ t p -> (t, p)
                                    DropMove _ t    -> (t, False)
                                  capVal = case Map.lookup toSq (board s) of
                                    Just (Piece _ pt) -> pieceValue pt * 10.0
                                    Nothing           -> 0.0
                                  promoVal = if isPromo then 500.0 else 0.0
                                  nextS = applyMove m s
                                  checkVal = if isKingUnderAttack (opponent (turn s)) (board nextS) then 800.0 else 0.0
                              in capVal + promoVal + checkVal
                            scoredMoves = sortBy (\\m1 m2 -> compare (scoreM m2) (scoreM m1)) nonLoopMoves"""

content = content.replace(old_scoring, new_scoring)

# In extendEquilibriumToIdeal, also use scoreM for fast gradient stream
old_ext_scoring = """                     let scored = sortBy (\\m1 m2 ->
                           let s1 = evaluateStratifiedState (turn s) (applyMove m1 s)
                               s2 = evaluateStratifiedState (turn s) (applyMove m2 s)
                           in compare s2 s1) legalMoves"""

new_ext_scoring = """                     let scoreM m =
                           let (toSq, isPromo) = case m of
                                 NormalMove _ t p -> (t, p)
                                 DropMove _ t    -> (t, False)
                               capVal = case Map.lookup toSq (board s) of
                                 Just (Piece _ pt) -> pieceValue pt * 10.0
                                 Nothing           -> 0.0
                               promoVal = if isPromo then 500.0 else 0.0
                               nextS = applyMove m s
                               checkVal = if isKingUnderAttack (opponent (turn s)) (board nextS) then 800.0 else 0.0
                           in capVal + promoVal + checkVal
                         scored = sortBy (\\m1 m2 -> compare (scoreM m2) (scoreM m1)) legalMoves"""

content = content.replace(old_ext_scoring, new_ext_scoring)

with open(file_path, "w", encoding="utf-8", newline="\n") as f:
    f.write(content)

print("Upgraded book generator to ultra-fast gradient flow!")
