with open('ShogiMorseCompiler.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

new_lines = []
in_class = False
for l in lines:
    if "class (KnownNat ply, KnownSymbol from, KnownSymbol to)" in l:
        new_lines.append(l)
        in_class = True
    elif in_class and "SteepestDescentMorseFlow" in l:
        new_lines.append(l)
    elif in_class and ("morseFlowPly" in l or "morseFlowFrom" in l or "morseFlowTo" in l):
        if "morseFlowPly" in l:
            new_lines.append("  morseFlowStep  :: Proxy ply -> Proxy from -> Proxy to -> (Integer, String, String)\n")
        # 他の行はスキップ
    else:
        in_class = False
        new_lines.append(l)

with open('ShogiMorseCompiler.hs', 'w', encoding='utf-8') as f:
    f.writelines(new_lines)

print("Class method updated to morseFlowStep!")
