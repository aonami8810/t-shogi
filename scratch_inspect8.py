# -*- coding: utf-8 -*-
import re

with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    text = f.read()

sente_matches = re.findall(r'senteSuperHighways_\d+\s*=\s*\[(.*?)\]', text)
print(f'Total senteSuperHighways: {len(sente_matches)}')

gote_matches = re.findall(r'goteSuperHighways_\d+\s*=\s*\[(.*?)\]', text)
print(f'Total goteSuperHighways: {len(gote_matches)}')

# Game 1: 34手目 B*6d まで
g1_34 = "2g2f 3c3d 7g7f 8c8d 2f2e 8d8e 6i7h 4a3b 2e2d 2c2d 2h2d 8e8f 8g8f 8b8f 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d 2i3g R*2i 3g4e 3c4d P*8b 7a8b 7g4d 4c4d B*5e B*6d".split()
print("\nG1 checking 34 moves (B*6d):")
g1_hits = 0
for i, m in enumerate(sente_matches):
    m_list = [x.strip().strip('"').strip("'") for x in m.split(',')]
    if len(m_list) >= len(g1_34) and m_list[:len(g1_34)] == g1_34:
        g1_hits += 1
        print(f"  Match in senteSuperHighways_{i}: total len={len(m_list)}, next 3 moves={m_list[len(g1_34):len(g1_34)+3]}")
print(f"G1 total matches: {g1_hits}")

# Game 2: 45手目 B*9f まで
g2_45 = "2g2f 3c3d 7g7f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d 2i3g 3c7g+ 8i7g B*6d 3i3h R*2h B*4f 6d4f 4g4f 2a3c 7g6e B*5d 6g6f 6c6d B*8c 6d6e 8c7d+ 5d6c 7d6c 5b6c B*9f".split()
print("\nG2 checking 45 moves (B*9f):")
g2_hits = 0
for i, m in enumerate(gote_matches):
    m_list = [x.strip().strip('"').strip("'") for x in m.split(',')]
    if len(m_list) >= len(g2_45) and m_list[:len(g2_45)] == g2_45:
        g2_hits += 1
        print(f"  Match in goteSuperHighways_{i}: total len={len(m_list)}, next 3 moves={m_list[len(g2_45):len(g2_45)+3]}")
print(f"G2 total matches: {g2_hits}")
