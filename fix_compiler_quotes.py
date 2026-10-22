with open('ShogiMorseCompiler.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

new_lines = []
for l in lines:
    if "instance SteepestDescentMorseFlow" in l:
        new_lines.append('      "instance SteepestDescentMorseFlow " ++ show (min 512 (length moves)) ++ " \\"" ++ fromNode ++ "\\" \\"" ++ toNode ++ "\\" where\\n" ++\n')
    elif 'morseFlowFrom _ = \\"' in l or 'morseFlowFrom _ = "' in l:
        new_lines.append('      "  morseFlowFrom _ = \\"" ++ fromNode ++ "\\"\\n" ++\n')
    elif 'morseFlowTo _ = \\"' in l or 'morseFlowTo _ = "' in l:
        new_lines.append('      "  morseFlowTo _ = \\"" ++ toNode ++ "\\""\n')
    else:
        new_lines.append(l)

with open('ShogiMorseCompiler.hs', 'w', encoding='utf-8') as f:
    f.writelines(new_lines)

print("SUCCESS: Fixed quote escaping in ShogiMorseCompiler.hs")
