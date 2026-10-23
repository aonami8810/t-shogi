# -*- coding: utf-8 -*-
import re

with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    text = f.read()

sente_matches = re.findall(r'senteSuperHighways_\d+\s*=\s*\[(.*?)\]', text)
print(f'Total senteSuperHighways: {len(sente_matches)}')

gote_matches = re.findall(r'goteSuperHighways_\d+\s*=\s*\[(.*?)\]', text)
print(f'Total goteSuperHighways: {len(gote_matches)}')

# 1. 先手初手
m0 = [x.strip().strip('"').strip("'") for x in sente_matches[0].split(',')]
print("Sente Highway 0 first 3 moves:", m0[:3])

# 2. 後手で水匠5が 2g2f 3c3d 2f2e
g_2f2e = "2g2f 3c3d 2f2e".split()
g_hits = 0
for i, m in enumerate(gote_matches):
    m_list = [x.strip().strip('"').strip("'") for x in m.split(',')]
    if len(m_list) >= len(g_2f2e) and m_list[:len(g_2f2e)] == g_2f2e:
        g_hits += 1
        if g_hits == 1:
            print(f"Gote match for 2f2e in highway_{i}: next move = {m_list[3]}")
print(f"Total gote matches for 2f2e: {g_hits}")

# 3. 後手で水匠5が 2g2f 3c3d 7g7f 8c8d ... 45手目 B*9f
g_45 = "2g2f 3c3d 7g7f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d 2i3g 3c7g+ 8i7g B*6d 3i3h R*2h B*4f 6d4f 4g4f 2a3c 7g6e B*5d 6g6f 6c6d B*8c 6d6e 8c7d+ 5d6c 7d6c 5b6c B*9f".split()
g_45_hits = 0
for i, m in enumerate(gote_matches):
    m_list = [x.strip().strip('"').strip("'") for x in m.split(',')]
    if len(m_list) >= len(g_45) and m_list[:len(g_45)] == g_45:
        g_45_hits += 1
        if g_45_hits == 1:
            print(f"Gote match for B*9f in highway_{i}: total len = {len(m_list)}, next = {m_list[len(g_45):len(g_45)+3]}")
print(f"Total gote matches for B*9f: {g_45_hits}")
