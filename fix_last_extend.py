with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    content = f.read()

target = '''        let lastSt = last states
            (extendedMoves, isEquilClosed, qegtbAfterEquil) =
              extendEquilibriumToIdeal lastSt updatedQEGTB 15 []'''

replacement = '''        let lastSt = last states
            totalBaseMoves = length moves
            remStepsTo512 = max 0 (512 - totalBaseMoves)
            (extendedMoves, isEquilClosed, qegtbAfterEquil) =
              extendEquilibriumToIdeal512 lastSt updatedQEGTB remStepsTo512 Map.empty []'''

if target in content:
    content = content.replace(target, replacement)
    with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
        f.write(content)
    print("SUCCESS: Replaced extendEquilibriumToIdeal with extendEquilibriumToIdeal512!")
else:
    print("Target not found directly, checking normalized...")
    # 行ごとに置換
    lines = content.splitlines()
    new_lines = []
    i = 0
    while i < len(lines):
        if "extendEquilibriumToIdeal lastSt updatedQEGTB" in lines[i]:
            new_lines.pop() # let lastSt = last states
            new_lines.pop() # (extendedMoves ...
            new_lines.append("        let lastSt = last states")
            new_lines.append("            totalBaseMoves = length moves")
            new_lines.append("            remStepsTo512 = max 0 (512 - totalBaseMoves)")
            new_lines.append("            (extendedMoves, isEquilClosed, qegtbAfterEquil) =")
            new_lines.append("              extendEquilibriumToIdeal512 lastSt updatedQEGTB remStepsTo512 Map.empty []")
            i += 1
        else:
            new_lines.append(lines[i])
            i += 1
    with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
        f.write("\n".join(new_lines) + "\n")
    print("SUCCESS: Normalized replace done!")
