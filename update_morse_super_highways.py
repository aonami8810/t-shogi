# -*- coding: utf-8 -*-
import ast

with open("g1_ply62_6a7b_win.txt", "r", encoding="utf-8") as f:
    g1_line = ast.literal_eval(f.read().strip())

with open("g1_ply12_5c5d_win.txt", "r", encoding="utf-8") as f:
    g1_5c5d_line = ast.literal_eval(f.read().strip())

with open("g1_ply16_6a5b_nodes5m_win.txt", "r", encoding="utf-8") as f:
    g1_16_6a5b_line = ast.literal_eval(f.read().strip())

with open("g1_ply28_p2g_nodes5m_win.txt", "r", encoding="utf-8") as f:
    g1_28_p2g_line = ast.literal_eval(f.read().strip())

with open("g1_ply42_2c3c_nodes5m_win.txt", "r", encoding="utf-8") as f:
    g1_42_2c3c_line = ast.literal_eval(f.read().strip())

with open("g1_ply14_2b3a_nodes5m_win.txt", "r", encoding="utf-8") as f:
    g1_14_2b3a_line = ast.literal_eval(f.read().strip())

with open("g1_ply32_7a8b_nodes5m_win.txt", "r", encoding="utf-8") as f:
    g1_32_7a8b_line = ast.literal_eval(f.read().strip())

with open("g1_ply20_5a5b_nodes5m_win.txt", "r", encoding="utf-8") as f:
    g1_20_5a5b_line = ast.literal_eval(f.read().strip())

with open("g1_ply38_p2g_nodes5m_win.txt", "r", encoding="utf-8") as f:
    g1_38_p2g_line = ast.literal_eval(f.read().strip())

with open("g2_ply39_9g9f_nodes5m_win.txt", "r", encoding="utf-8") as f:
    g2_39_9g9f_line = ast.literal_eval(f.read().strip())

with open("g2_ply39_p8c_nodes5m_win.txt", "r", encoding="utf-8") as f:
    g2_39_p8c_line = ast.literal_eval(f.read().strip())

with open("g2_ply25_p8c_nodes5m_win.txt", "r", encoding="utf-8") as f:
    g2_25_p8c_line = ast.literal_eval(f.read().strip())

with open("g2_ply19_9g9f_nodes5m_win.txt", "r", encoding="utf-8") as f:
    g2_19_9g9f_line = ast.literal_eval(f.read().strip())

with open("g2_ply39_6e5c_nodes5m_win.txt", "r", encoding="utf-8") as f:
    g2_39_6e5c_line = ast.literal_eval(f.read().strip())

g2_5i5h_150 = [
    '2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '5i5h', '4a3b', '2e2d', '2c2d',
    '2h2d', '8b8d', '2d2f', 'P*2c', '6i7h', '5a4b', '3i3h', '8e8f', '8g8f', '8d8f',
    'P*8g', '8f8b', '3g3f', '7a7b', '9g9f', '6c6d', '2i3g', '7b6c', '4g4f', '6c5d',
    '1g1f', '1c1d', '8h2b+', '3a2b', '7i8h', '9c9d', '3h4g', '7c7d', '8h7g', '4c4d',
    '4i4h', '2b3c', '5h6h', '6a6b', '2f2i', '3c2d', '4f4e', '4d4e', '4g5f', '4e4f',
    '6h7i', '6b5b', '7i8h', '8a7c', '7f7e', '7c6e', '7g8f', 'P*8e', '8f9g', 'B*2b',
    'B*6f', '2d3c', '7e7d', '8b7b', 'P*2d', '2c2d', 'P*2e', '2d2e', '3g2e', '3c2d',
    '6f2b+', '3b2b', '6g6f', 'P*7g', '7h6g', 'P*2h', '2i4i', '2d2e', '6f6e', 'N*4d',
    '4h5h', '4d5f', '5g5f', '2e3f', 'N*2f', '2b3c', 'B*2b', '3f4g', '5h6h', 'S*5h',
    'N*2e', '3c2c', '2b1a+', '5h6g', '1a7g', '6g6h+', '7g6h', 'B*4d', 'S*5e', '5d5e',
    'L*4e', '4g5f+', '4e4d', '5e4d', 'B*1a', 'S*6g', '6h6g', '5f6g', '1a4d+', '7b7d',
    'P*7f', '7d7f', 'P*7i', 'G*7h', '8h9h', '7h8i', 'S*8h', '8i8h', '9g8h', 'N*8f',
    '8g8f', '8e8f', '9h9g', 'B*7g', 'S*3c', '2a3c', '2f3d', '2c3d', '2e3c+', '3d3c',
    '4d3c', '7g3c+', 'P*4c', '5b4c', 'N*5d', '5c5d', 'N*3d', '4c3d', 'G*4c', '3c4c',
    'G*3c', '3d3c', '6e6d', 'G*8g', '8h8g', '8f8g+', '9g8g', '7f7g+', '8g9h', 'G*8h'
]

sente_prefixes15 = [
    ['7g7f', '8c8d', '2g2f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '4a3b', '2e2d', '2c2d', '2h2d', '8e8f', '8g8f', '8b8f', '2d3d'],
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d'],
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d'],
]

gote_prefixes15 = [
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '6i7h', '8d8e', '2f2e', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d'],
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '3c3d', '7g7f', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d'],
]

def to_hs_list(lst):
    return "[" + ", ".join(f'"{m}"' for m in lst) + "]"

with open("ShogiUsiMorse-v4.hs", "r", encoding="utf-8") as f:
    lines = f.readlines()

extracted = lines[1577:2850]

sente_new_defs = []
sente_new_names = []
for idx, p in enumerate(sente_prefixes15):
    name = f"senteHeroNew_{idx}"
    sente_new_names.append(name)
    sente_new_defs.append(f"{name} :: [String]\n{name} = {to_hs_list(p + g1_line[15:])}")
    name_p2g = f"senteHeroNew_p2g_{idx}"
    sente_new_names.append(name_p2g)
    sente_new_defs.append(f"{name_p2g} :: [String]\n{name_p2g} = {to_hs_list(p + g1_28_p2g_line[15:])}")
    name_2c3c = f"senteHeroNew_2c3c_{idx}"
    sente_new_names.append(name_2c3c)
    sente_new_defs.append(f"{name_2c3c} :: [String]\n{name_2c3c} = {to_hs_list(p + g1_42_2c3c_line[15:])}")
    name_7a8b = f"senteHeroNew_7a8b_{idx}"
    sente_new_names.append(name_7a8b)
    sente_new_defs.append(f"{name_7a8b} :: [String]\n{name_7a8b} = {to_hs_list(p + g1_32_7a8b_line[15:])}")
    name_20_5a5b = f"senteHeroNew_20_5a5b_{idx}"
    sente_new_names.append(name_20_5a5b)
    sente_new_defs.append(f"{name_20_5a5b} :: [String]\n{name_20_5a5b} = {to_hs_list(p + g1_20_5a5b_line[15:])}")
    name_38_p2g = f"senteHeroNew_38_p2g_{idx}"
    sente_new_names.append(name_38_p2g)
    sente_new_defs.append(f"{name_38_p2g} :: [String]\n{name_38_p2g} = {to_hs_list(p + g1_38_p2g_line[15:])}")

for label, line_data in [
    ("senteHeroNew_16_6a5b", g1_16_6a5b_line),
    ("senteHeroNew_5c5d", g1_5c5d_line),
    ("senteHeroNew_14_2b3a", g1_14_2b3a_line),
]:
    sente_new_names.append(label)
    sente_new_defs.append(f"{label} :: [String]\n{label} = {to_hs_list(line_data)}")

gote_new_defs = []
gote_new_names = []
for idx, p in enumerate(gote_prefixes15):
    name = f"goteHeroNew_{idx}"
    gote_new_names.append(name)
    gote_new_defs.append(f"{name} :: [String]\n{name} = {to_hs_list(p + g2_39_9g9f_line[15:])}")
    name_p8c = f"goteHeroNew_p8c_{idx}"
    gote_new_names.append(name_p8c)
    gote_new_defs.append(f"{name_p8c} :: [String]\n{name_p8c} = {to_hs_list(p + g2_39_p8c_line[15:])}")
    name_25_p8c = f"goteHeroNew_25_p8c_{idx}"
    gote_new_names.append(name_25_p8c)
    gote_new_defs.append(f"{name_25_p8c} :: [String]\n{name_25_p8c} = {to_hs_list(p[:14] + g2_25_p8c_line[14:])}")
    name_19_9g9f = f"goteHeroNew_19_9g9f_{idx}"
    gote_new_names.append(name_19_9g9f)
    gote_new_defs.append(f"{name_19_9g9f} :: [String]\n{name_19_9g9f} = {to_hs_list(p + g2_19_9g9f_line[15:])}")
    name_39_6e5c = f"goteHeroNew_39_6e5c_{idx}"
    gote_new_names.append(name_39_6e5c)
    gote_new_defs.append(f"{name_39_6e5c} :: [String]\n{name_39_6e5c} = {to_hs_list(p + g2_39_6e5c_line[15:])}")

for idx, p in enumerate([
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '5i5h'],
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '5i5h'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '5i5h'],
]):
    name_5i5h = f"goteHeroNew_5i5h_{idx}"
    gote_new_names.append(name_5i5h)
    gote_new_defs.append(f"{name_5i5h} :: [String]\n{name_5i5h} = {to_hs_list(p + g2_5i5h_150[7:])}")

content = "module MorseSuperHighways (senteSuperHighways, goteSuperHighways) where\n\n" + "".join(extracted)

content = content.replace(
    "senteSuperHighways :: [[String]]\nsenteSuperHighways = [ ",
    "\n".join(sente_new_defs) + "\n\nsenteSuperHighways :: [[String]]\nsenteSuperHighways = [ " + ", ".join(sente_new_names) + ", "
)

content = content.replace(
    "goteSuperHighways :: [[String]]\ngoteSuperHighways = [ ",
    "\n".join(gote_new_defs) + "\n\ngoteSuperHighways :: [[String]]\ngoteSuperHighways = [ " + ", ".join(gote_new_names) + ", "
)

with open("MorseSuperHighways.hs", "w", encoding="utf-8") as f:
    f.write(content)

print("Regenerated MorseSuperHighways.hs with G1_16_6a5b and G2_39_9g9f!")
