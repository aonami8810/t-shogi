file_path = "c:/VS/Workspace/t-shogi/GenerateStratifiedCompleteBook.hs"
with open(file_path, "r", encoding="utf-8") as f:
    lines = f.readlines()

out = []
in_tsumero_nothing = False
for i, line in enumerate(lines):
    if "Nothing ->" in line and i > 120 and i < 130:
        in_tsumero_nothing = True
        out.append(line)
    elif in_tsumero_nothing and "nonLoopMoves of" in line:
        out.append("  " + line)
        in_tsumero_nothing = False
    elif in_tsumero_nothing:
        out.append("  " + line)
    else:
        out.append(line)

with open(file_path, "w", encoding="utf-8") as f:
    f.writelines(out)

print("Fixed indentation of Nothing -> in searchBounded!")
