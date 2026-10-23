# -*- coding: utf-8 -*-
import ast

with open("g1_r2i_full.txt", "r", encoding="utf-8") as f:
    g1_r2i_127 = ast.literal_eval(f.read().strip())

with open("g1_p2b_full.txt", "r", encoding="utf-8") as f:
    g1_p2b_157 = ast.literal_eval(f.read().strip())

with open("g1_2b8h_full.txt", "r", encoding="utf-8") as f:
    g1_2b8h_205 = ast.literal_eval(f.read().strip())

with open("g1_2i2h_result.txt", "r", encoding="utf-8") as f:
    g1_2i2h_139 = ast.literal_eval(f.read().strip())

with open("g2_p2c_3b2c_full.txt", "r", encoding="utf-8") as f:
    g2_p2c_3b2c_154 = ast.literal_eval(f.read().strip())

with open("g2_2f2e_result.txt", "r", encoding="utf-8") as f:
    g2_2f2e_156 = ast.literal_eval(f.read().strip())

with open("g2_5i5h_full.txt", "r", encoding="utf-8") as f:
    g2_5i5h_180 = ast.literal_eval(f.read().strip())

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
from generate_all_transpositions import (
    g1_r2f_2a3c_114,
    g2_3g4e_2a3c_107
)

# 7手目 共通局面への8大進入路プレフィックス
prefixes_7ply = [
    # 7手目 6i7h 到達型
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h'],
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '6i7h'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h'],
    ['7g7f', '8c8d', '2g2f', '3c3d', '2f2e', '8d8e', '6i7h'],
    ['2g2f', '3c3d', '2f2e', '8c8d', '7g7f', '8d8e', '6i7h'],
    # 7手目 2f2e 到達型（実戦 Game 2 出現型！）
    ['2g2f', '3c3d', '7g7f', '8c8d', '6i7h', '8d8e', '2f2e'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '6i7h', '8d8e', '2f2e'],
]

def expand_line(line_7plus):
    suffix = line_7plus[7:]
    res = []
    for pref in prefixes_7ply:
        res.append(pref + suffix)
    return res

# 1. 先手番 (Morse Sente)
sente_lines = []
# 実戦最新分岐 1: 26手目 R*2i (127手完)
sente_lines.extend(expand_line(g1_r2i_127))
# 実戦最新分岐 2: 26手目 P*2b (157手完)
sente_lines.extend(expand_line(g1_p2b_157))
# 実戦最新分岐 3: 8手目 4a3b -> 16手目 2b8h+ (205手完)
sente_lines.extend(expand_line(g1_2b8h_205))
# 実戦最優先ライン 1: 36手目 2i2h (139手完)
sente_lines.extend(expand_line(g1_2i2h_139))
# 実戦最優先ライン 2: 36手目 2i3i (113手完)
sente_lines.extend(expand_line(g1_r2f_2a3c_114))
# 実戦最優先ライン 3: 30手目 3b3c (118手完)
sente_lines.extend(expand_line(g1_r2f_118))
# 実戦最優先ライン 4: 26手目 R*2g (156手完)
sente_lines.extend(expand_line(g1_156))
# 実戦最優先ライン 5: 26手目 R*2e (116手完)
sente_lines.extend(expand_line(g1_r2e_116))
# 8手目 4a3b -> 16手目 2b3c (122手完)
sente_lines.extend(expand_line(g1_4a3b_122))
# 16手目 2a3c (106手完)
sente_lines.extend(expand_line(g1_2a3c_106))

# 2. 後手番 (Morse Gote)
gote_lines = []
# 実戦最新分岐 1: 27手目 P*2c -> 28手目 3b2c (154手完)
gote_lines.extend(expand_line(g2_p2c_3b2c_154))
# 実戦最新分岐 2: 14手目 4a3b -> 15手目 5i5h (180手完)
gote_lines.extend(expand_line(g2_5i5h_180))
# 実戦最優先ライン 3: 水匠5初手 2g2f -> 3手目 2f2e (156手完)
gote_lines.append(g2_2f2e_156)
# 実戦最優先ライン 4: 29手目 3g4e (107手完)
gote_lines.extend(expand_line(g2_3g4e_2a3c_107))
# 実戦最優先ライン 5: 25手目 2i3g (145手完)
gote_lines.extend(expand_line(g2_2i3g_145))
# 21手目 8i7g (153手完)
gote_lines.extend(expand_line(g2_8i7g_153))
# 21手目 8h7g (180手完)
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

print(f"Total Sente lines: {len(sente_lines)}")
print(f"Total Gote lines: {len(gote_lines)}")

def to_hs(lines, name):
    parts = []
    for i, l in enumerate(lines):
        hs_list = "[" + ", ".join(f'"{x}"' for x in l) + "]"
        parts.append(f"{name}_{i} = {hs_list}")
    all_names = ", ".join(f"{name}_{i}" for i in range(len(lines)))
    return "\n".join(parts) + f"\n\n{name} :: [[String]]\n{name} = [ {all_names} ]\n"

code = f"""-- | 水匠5完全撃破スーパーハイウェイ（手番別完全分離設計 ＆ 手順前後完全吸収）
-- 【先手番（Morse Sente）必勝ライン群（全進入路展開）】
{to_hs(sente_lines, 'senteSuperHighways')}

-- 【後手番（Morse Gote）必勝ライン群（全進入路展開）】
{to_hs(gote_lines, 'goteSuperHighways')}

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

print("Generated master conquest_code_separated.txt v4 successfully!")
