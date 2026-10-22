with open("c:\\VS\\Workspace\\t-shogi\\ShogiUsiMorse-v4.hs", "r", encoding="utf-8") as f:
    lines = f.readlines()

for i, l in enumerate(lines):
    if "nakabishaCastlingBonus = evalNakabishaCastlingFlow st m" in l:
        prefix = l[:l.index("nakabishaCastlingBonus")]
        print(f"Line {i} prefix repr: {repr(prefix)}")
    if "tokinAssaultBonus" in l:
        prefix2 = l[:l.index("tokinAssaultBonus")]
        print(f"Line {i} prefix2 repr: {repr(prefix2)}")
