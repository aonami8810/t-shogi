with open("c:\\VS\\Workspace\\t-shogi\\ShogiUsiMorse-v4.hs", "r", encoding="utf-8") as f:
    content = f.read()

target = "nakabishaCastlingBonus = evalNakabishaCastlingFlow st m"
replacement = "nakabishaCastlingBonus = evalNakabishaCastlingFlow st m\n                    tokinAssaultBonus = evalTokinVanguardAssaultFlow st m"

if target in content:
    content = content.replace(target, replacement, 1)
    target2 = "+ rookFileDefenseBonus + nakabishaCastlingBonus"
    replacement2 = "+ rookFileDefenseBonus + nakabishaCastlingBonus + tokinAssaultBonus"
    content = content.replace(target2, replacement2, 1)
    with open("c:\\VS\\Workspace\\t-shogi\\ShogiUsiMorse-v4.hs", "w", encoding="utf-8") as f:
        f.write(content)
    print("SUCCESS: Replaced successfully")
else:
    print("ERROR: Target not found")
