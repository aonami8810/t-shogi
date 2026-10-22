file_path = "c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs"
with open(file_path, "r", encoding="utf-8") as f:
    lines = f.readlines()

out = []
i = 0
while i < len(lines):
    line = lines[i]
    if "let scoreMove m =" in line:
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
        i += 1
    else:
        out.append(line)
        i += 1

with open(file_path, "w", encoding="utf-8") as f:
    f.writelines(out)

print("Replaced scoreMove with scoreMoveFast and scoreMoveFull!")
