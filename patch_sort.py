with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    content = f.read()

target = """          -- 11手目（先手）: 水匠5の後手7二銀・7四歩には 7七銀 (8h7g) 最優先で角換わり完勝！
          ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "8c8d", "2g2f", "7a7b", "6i7h", "7c7d"] ->
            sortBy (\m1 m2 ->
              let score m = case m of
                    NormalMove (8, 8) (7, 7) False -> 3 :: Int
                    _                              -> 0 :: Int
              in compare (score m2) (score m1)
            ) validBookMoves"""

addition = """          -- 9手目（先手）: 水匠5の後手4a3b・1c1dには 3六歩 (3g3f) 最優先で角換わり完勝！
          ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "4a3b", "2g2f", "1c1d"] ->
            sortBy (\m1 m2 ->
              let score m = case m of
                    NormalMove (3, 7) (3, 6) False -> 3 :: Int
                    _                              -> 0 :: Int
              in compare (score m2) (score m1)
            ) validBookMoves
""" + target

import re
if target in content:
    content = content.replace(target, addition)
    print("Replaced with LF match!")
elif target.replace('\n', '\r\n') in content:
    content = content.replace(target.replace('\n', '\r\n'), addition.replace('\n', '\r\n'))
    print("Replaced with CRLF match!")
else:
    print("Direct match failed, searching substring...")
    patt = re.compile(r"-- 11手目（先手）: 水匠5の後手7二銀・7四歩.*?\) validBookMoves", re.DOTALL)
    m = patt.search(content)
    if m:
        content = content[:m.start()] + addition + content[m.end():]
        print("Replaced with regex!")
    else:
        print("Regex match failed!")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(content)
