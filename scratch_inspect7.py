# -*- coding: utf-8 -*-
import re

with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    text = f.read()

sente_matches = re.findall(r'senteSuperHighways_\d+\s*=\s*\[(.*?)\]', text)
print(f'Total senteSuperHighways: {len(sente_matches)}')

gote_matches = re.findall(r'goteSuperHighways_\d+\s*=\s*\[(.*?)\]', text)
print(f'Total goteSuperHighways: {len(gote_matches)}')

# Game 1: 32手目 B*6d まで
g1_32 = "2g2f 3c3d 7g7f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d P*2d R*2g 7g3c+ 3b3c 3i3h 2g2e+ B*4f B*6d".split()
print("\nG1 checking 32 moves (B*6d):")
g1_hits = 0
for i, m in enumerate(sente_matches):
    m_list = [x.strip().strip('"').strip("'") for x in m.split(',')]
    if len(m_list) >= len(g1_32) and m_list[:len(g1_32)] == g1_32:
        g1_hits += 1
        print(f"  Match in senteSuperHighways_{i}: total len={len(m_list)}, next 3 moves={m_list[len(g1_32):len(g1_32)+3]}")
print(f"G1 total matches: {g1_hits}")

# Game 2: 29手目 P*2d まで
g2_29 = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 5i5h 8f7f 8h2b+ 3a2b 2d3d 2b3c 3d3f 7f3f 3g3f P*8b 3i3h 5a5b B*4f P*2g P*2d".split()
print("\nG2 checking 29 moves (P*2d):")
g2_hits = 0
for i, m in enumerate(gote_matches):
    m_list = [x.strip().strip('"').strip("'") for x in m.split(',')]
    if len(m_list) >= len(g2_29) and m_list[:len(g2_29)] == g2_29:
        g2_hits += 1
        print(f"  Match in goteSuperHighways_{i}: total len={len(m_list)}, next 3 moves={m_list[len(g2_29):len(g2_29)+3]}")
print(f"G2 total matches: {g2_hits}")
