with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    content = f.read()

target = """                          _  ->
                            let scoredMoves = sortBy (\m1 m2 ->
                                  let s1 = evaluateStratifiedState (turn s) (applyMove m1 s)
                                      s2 = evaluateStratifiedState (turn s) (applyMove m2 s)
                                  in compare s2 s1) nonLoopMoves"""

replacement = """                          _  ->
                            let scoreM m =
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

if target in content:
    content = content.replace(target, replacement, 1)
    with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
        f.write(content)
    print("Successfully replaced searchPunishmentIDS512 move scoring!")
else:
    # Try normalizing newlines
    c_norm = content.replace('\r\n', '\n')
    t_norm = target.replace('\r\n', '\n')
    r_norm = replacement.replace('\r\n', '\n')
    if t_norm in c_norm:
        c_norm = c_norm.replace(t_norm, r_norm, 1)
        with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
            f.write(c_norm)
        print("Successfully replaced with normalized newlines!")
    else:
        print("Target not found!")
