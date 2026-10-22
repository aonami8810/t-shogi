with open('c:\\VS\\Workspace\\t-shogi\\ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

for i, l in enumerate(lines):
    if 's = evaluateState weights (turn st) nextSt' in l:
        print(f"Found line at {i+1}")
        indent = l[:l.index('s = evaluateState')]
        new_lines = [
            indent + "-- ★ 自玉周りの指し手において、駒の配置空間が相手のQEGTBにない局面になる手を最優先！\n",
            indent + "antiQuotientSafetyBonus = evalAntiQuotientSafety st m\n",
            indent + "\n",
            l
        ]
        lines[i:i+1] = new_lines
        break

# Now update the sum line
for i, l in enumerate(lines):
    if '+ quotientEquivBonus' in l:
        print(f"Found sum line at {i+1}")
        lines[i] = l.rstrip() + "\n" + l[:l.index('+ quotientEquivBonus')] + "+ antiQuotientSafetyBonus\n"
        break

with open('c:\\VS\\Workspace\\t-shogi\\ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)
print("Successfully updated ShogiUsiMorse-v4.hs")
