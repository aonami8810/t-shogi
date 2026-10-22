file_path = "c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs"
with open(file_path, "r", encoding="utf-8") as f:
    lines = f.readlines()

out = []
in_score_full = False
for line in lines:
    if "scoreMoveFull m =" in line:
        in_score_full = True
        out.append(line)
    elif in_score_full and "orderedMoves = sortBy" in line:
        in_score_full = False
        out.append(line)
    elif in_score_full:
        # indent scoreMoveFull body by 2 extra spaces
        out.append("  " + line)
    else:
        out.append(line)

with open(file_path, "w", encoding="utf-8") as f:
    f.writelines(out)
print("Indented scoreMoveFull body correctly!")
