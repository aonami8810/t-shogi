file_path = "c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs"
with open(file_path, "r", encoding="utf-8") as f:
    lines = f.readlines()

# Let's fix line 1502 to 1665
out = []
i = 0
while i < len(lines):
    line = lines[i]
    if "let scoreMoveFast m =" in line:
        out.append("          let scoreMoveFast m =\n")
        out.append("                let (toSq, isPromo) = case m of\n")
        out.append("                      NormalMove _ t p -> (t, p)\n")
        out.append("                      DropMove _ t    -> (t, False)\n")
        out.append("                    capVal = case Map.lookup toSq (board st) of\n")
        out.append("                      Just (Piece _ pt) -> pieceValue pt * 8.0\n")
        out.append("                      Nothing           -> 0.0\n")
        out.append("                    promoVal = if isPromo then 400.0 else 0.0\n")
        out.append("                    nextSt = applyMove m st\n")
        out.append("                    checkVal = if isKingUnderAttack (opponent (turn st)) (board nextSt) then 600.0 else 0.0\n")
        out.append("                in capVal + promoVal + checkVal\n\n")
        out.append("              scoreMoveFull m =\n")
        
        # Now read until orderedMoves
        i += 1
        # skip lines until let isBlunder
        while i < len(lines) and "let isBlunder" not in lines[i]:
            i += 1
        
        # now read scoreMoveFull body until orderedMoves
        while i < len(lines) and "orderedMoves = sortBy" not in lines[i]:
            # trim leading spaces and add 16 spaces
            l_strip = lines[i].lstrip()
            if not l_strip:
                out.append("\n")
            elif l_strip.startswith("in "):
                out.append("                " + l_strip)
            elif l_strip.startswith("+ "):
                out.append("                   " + l_strip)
            elif l_strip.startswith("let "):
                out.append("                " + l_strip)
            else:
                out.append("                  " + l_strip)
            i += 1
        
        # Now append orderedMoves and topMoves with 10 spaces
        out.append("              orderedMoves = sortBy (\\m1 m2 -> compare (if depth >= 4 then scoreMoveFast m2 else scoreMoveFull m2) (if depth >= 4 then scoreMoveFast m1 else scoreMoveFull m1)) effectiveMoves\n")
        out.append("              topMoves = if inCheck\n")
        out.append("                         then orderedMoves\n")
        out.append("                         else if depth >= 12\n")
        out.append("                         then take 3 orderedMoves\n")
        out.append("                         else if depth >= 6\n")
        out.append("                         then take 5 orderedMoves\n")
        out.append("                         else if depth >= 2\n")
        out.append("                         then take 8 orderedMoves\n")
        out.append("                         else take 12 orderedMoves\n")
        
        # skip old orderedMoves and topMoves lines in lines
        while i < len(lines) and "in loop (zip topMoves [0..]) alpha Nothing" not in lines[i]:
            i += 1
        
        # keep going
    else:
        out.append(line)
        i += 1

with open(file_path, "w", encoding="utf-8") as f:
    f.writelines(out)

print("Standardized indentation to exact column!")
