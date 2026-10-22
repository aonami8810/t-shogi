with open("c:\\VS\\Workspace\\t-shogi\\ShogiUsiMorse-v4.hs", "r", encoding="utf-8") as f:
    lines = f.readlines()

new_lines = []
for l in lines:
    if "tokinAssaultBonus = evalTokinVanguardAssaultFlow st m" in l:
        new_lines.append("                   tokinAssaultBonus = evalTokinVanguardAssaultFlow st m\n")
    else:
        new_lines.append(l)

with open("c:\\VS\\Workspace\\t-shogi\\ShogiUsiMorse-v4.hs", "w", encoding="utf-8") as f:
    f.writelines(new_lines)
print("Aligned indent to 19 spaces successfully")
