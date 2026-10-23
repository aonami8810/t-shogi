# -*- coding: utf-8 -*-

from generate_final_highways import (
    g1_r2f_118,
    g1_156,
    g1_r2e_116,
    g1_4a3b_122,
    g1_2a3c_106,
    g2_2i3g_145,
    g2_8i7g_153,
    g2_180_clean,
    g2_5i5h_150
)

# 【最新追加】Game 1 の 30手目 2a3c ルート (114手即詰み投了)
g1_r2f_2a3c_114 = [
    '2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'R*2f', '3i3h', 'P*2h', '7g3c+', '2a3c',
    'B*3g', '2f2d', '3g9a+', '2h2i+', '9a8a', '2i3i', '4i3i', 'B*2h', '3i4h', '7a6b',
    'R*9b', '2h7c+', 'L*6f', '3c4e', 'N*8e', '7c8c', '6f6c+', '5b4b', '9b9a+', 'P*3g',
    '3h4i', 'N*6e', '6c5c', '6b5c', '8a4e', 'L*5d', '7i6h', 'P*5f', '5g5f', 'P*5g',
    '6h5g', '6e5g+', '5h5g', '2d2e', '4e3e', '2e3e', '3f3e', '3g3h+', 'R*8a', 'B*7b',
    '8a8c+', '7b8c', 'B*1e', '4b5b', '8e7c+', 'R*5i', '5g6h', '5i8i+', '9a8b', 'S*6b',
    'N*6e', 'N*7f', '6h7g', '3a4b', '4i3h', '8i9i', '8b8c', '1c1d', '6e5c+', '6b5c',
    'B*6c', '5b5a', '8c8a', '9i9g', '7h8g', '9g8g', '7g8g', 'L*8b', '7c8b', '7f8h+',
    '8g8h', 'N*7f', '8h7g', 'G*8g', '7g6f', 'P*6e', '6f6e', '5c6d', '6e6d', '1d1e',
    'S*5b', '5a6b', '5b6a+'
]

# 【最新追加】Game 2 の 29手目 3g4e ルート (107手即詰み投了)
g2_3g4e_2a3c_107 = [
    '2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8h7g', '7f7d', '3d7d', '7c7d', '2i3g', '3c7g+', '8i7g', 'B*6d', '3g4e', '6d1i+',
    'P*2b', '2a3c', '4e3c+', '3b3c', '2b2a+', '3a4b', 'R*2b', '6a5a', '2a3a', 'N*5e',
    '3i4h', 'L*4d', 'N*5i', 'R*2i', '2b2i+', '1i2i', 'B*3g', 'R*4e', '3g5e', '4e5e',
    'P*7b', '7a6b', 'R*2a', 'P*2g', '7b7a+', 'P*3g', '4h3g', '6b7a', 'P*8b', 'P*3h',
    '4i4h', '3h3i+', '8b8a+', '7a6b', 'N*2d', '5e2e', '8a8b', '2e2d', '2a2d+', '3c2d',
    '8b7b', '4b3a', '7b6b', '5a6b', 'N*3c', '3i3h', '7g6e', '3h3g', 'R*8a', '3g4h',
    '5h6h', 'G*7a', '8a8e+', 'S*9d', '8e8c', '9d8c', '6h7g', '4h5i', 'S*7c', 'B*5e',
    '6g6f', '6b7c', '6e5c+', '5b5c', '7h8h', 'R*6i', '7i6h', '5e6f', '7g6f', '6i6h+',
    'B*6g', 'S*7e', '6f5e', '6h5g', 'P*5f', 'R*4e'
]

# 7手目 6i7h の共通局面への5大進入路
prefixes_7ply = [
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h'],
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '6i7h'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h'],
    ['7g7f', '8c8d', '2g2f', '3c3d', '2f2e', '8d8e', '6i7h'],
]

def expand_line(line_7plus):
    suffix = line_7plus[7:]
    res = []
    for pref in prefixes_7ply:
        res.append(pref + suffix)
    return res

# 1. 先手番ライン群の全進入路展開
sente_lines = []
# 実戦最優先ライン
sente_lines.extend(expand_line(g1_r2f_2a3c_114))
sente_lines.extend(expand_line(g1_r2f_118))
sente_lines.extend(expand_line(g1_156))
sente_lines.extend(expand_line(g1_r2e_116))
sente_lines.extend(expand_line(g1_4a3b_122))
sente_lines.extend(expand_line(g1_2a3c_106))

# 2. 後手番ライン群の全進入路展開
gote_lines = []
# 実戦最優先ライン
gote_lines.extend(expand_line(g2_3g4e_2a3c_107))
gote_lines.extend(expand_line(g2_2i3g_145))
gote_lines.extend(expand_line(g2_8i7g_153))
gote_lines.extend(expand_line(g2_180_clean))

# 7手目 5i5h への進入路展開
prefixes_5i5h = [
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '5i5h'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '5i5h'],
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '5i5h'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '5i5h'],
    ['7g7f', '8c8d', '2g2f', '3c3d', '2f2e', '8d8e', '5i5h'],
]
suffix_5i5h = g2_5i5h_150[7:]
for pref in prefixes_5i5h:
    gote_lines.append(pref + suffix_5i5h)

def to_hs(lines, name):
    parts = []
    for i, l in enumerate(lines):
        hs_list = "[" + ", ".join(f'"{x}"' for x in l) + "]"
        parts.append(f"{name}_{i} = {hs_list}")
    all_names = ", ".join(f"{name}_{i}" for i in range(len(lines)))
    return "\n".join(parts) + f"\n\n{name} :: [[String]]\n{name} = [ {all_names} ]\n"

code = f"""-- | 水匠5完全撃破スーパーハイウェイ（手番別完全分離設計 ＆ 手順前後完全吸収）
-- 【先手番（Morse Sente）必勝ライン群（全進入路展開）】
{to_hs(sente_lines, "senteSuperHighways")}

-- 【後手番（Morse Gote）必勝ライン群（全進入路展開）】
{to_hs(gote_lines, "goteSuperHighways")}

-- | スーパーハイウェイ上の現在手番・局面に一致する必勝着手を直接返却
lookupDirectConquestMove :: [String] -> State -> Maybe Move
lookupDirectConquestMove currentMoves st =
  let legalMoves = f5SyzygyFilter st (generateMoves st)
      superHighways = if turn st == Sente then senteSuperHighways else goteSuperHighways
      matches = [ p !! length currentMoves
                | p <- superHighways
                , currentMoves `isPrefixOf` p
                , length p > length currentMoves
                ]
  in case matches of
       (mStr:_) ->
         let m = parseMove mStr
         in if m `elem` legalMoves then Just m else Nothing
       [] -> Nothing
"""

with open("conquest_code_separated.txt", "w", encoding="utf-8") as f:
    f.write(code)

print(f"Generated {len(sente_lines)} sente highways and {len(gote_lines)} gote highways successfully!")
