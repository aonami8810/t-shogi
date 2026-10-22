file_path = "c:/VS/Workspace/t-shogi/GenerateStratifiedCompleteBook.hs"
with open(file_path, "r", encoding="utf-8") as f:
    lines = f.readlines()

out = []
i = 0
while i < len(lines):
    line = lines[i]
    if "case detectMidgameTsumero (turn s) s q of" in line and i > 120 and i < 125:
        out.append(line)
        out.append(lines[i+1]) # Just (_, tsumeroM) -> ...
        out.append(lines[i+2]) # Nothing ->
        i += 3
        # indent all lines until in runIDS
        while i < len(lines) and "in runIDS depthStages" not in lines[i]:
            out.append("    " + lines[i])
            i += 1
    else:
        out.append(line)
        i += 1

with open(file_path, "w", encoding="utf-8") as f:
    f.writelines(out)

print("Properly indented entire block under Nothing -> !")
