file_path = "c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs"
with open(file_path, "r", encoding="utf-8") as f:
    content = f.read()

# Replace scoreMove with fast/full decoupled ordering
target_block = """          let scoreMove m =
                let isBlunder = isBlunderMove st m
                    blunderPen = if isBlunder then -2000.0 else 0.0
                    isFutileLance = if inCheck then False else isFutileLanceLift st m
                    lancePen = if isFutileLance then -5000.0 else 0.0
                    
                    nextSt = applyMove m st
                    opp = opponent (turn st)
                    nextBd = board nextSt
                    isCheck = isKingUnderAttack opp nextBd"""

replacement_block = """          let scoreMoveFast m =
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

              scoreMoveFull m =
                let isBlunder = isBlunderMove st m
                    blunderPen = if isBlunder then -2000.0 else 0.0
                    isFutileLance = if inCheck then False else isFutileLanceLift st m
                    lancePen = if isFutileLance then -5000.0 else 0.0
                    
                    nextSt = applyMove m st
                    opp = opponent (turn st)
                    nextBd = board nextSt
                    isCheck = isKingUnderAttack opp nextBd"""

content = content.replace(target_block, replacement_block)

# Replace orderedMoves calculation
target_order = "orderedMoves = sortBy (\\m1 m2 -> compare (scoreMove m2) (scoreMove m1)) effectiveMoves"
replacement_order = "orderedMoves = sortBy (\\m1 m2 -> compare (if depth >= 4 then scoreMoveFast m2 else scoreMoveFull m2) (if depth >= 4 then scoreMoveFast m1 else scoreMoveFull m1)) effectiveMoves"

content = content.replace(target_order, replacement_order)

with open(file_path, "w", encoding="utf-8", newline="\n") as f:
    f.write(content)
print("Decoupled move ordering for extreme depth search!")
