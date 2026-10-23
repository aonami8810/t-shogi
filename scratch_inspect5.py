# -*- coding: utf-8 -*-
import re

with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    text = f.read()

sente_matches = re.findall(r'senteSuperHighways_\d+\s*=\s*\[(.*?)\]', text)
print(f'Total senteSuperHighways: {len(sente_matches)}')

gote_matches = re.findall(r'goteSuperHighways_\d+\s*=\s*\[(.*?)\]', text)
print(f'Total goteSuperHighways: {len(gote_matches)}')

# Game 1: 26手目 P*2b まで
g1_26 = "2g2f 3c3d 7g7f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d P*2d P*2b".split()
print("\nG1 checking 26 moves (P*2b):")
g1_hits = 0
for i, m in enumerate(sente_matches):
    m_list = [x.strip().strip('"').strip("'") for x in m.split(',')]
    if len(m_list) >= len(g1_26) and m_list[:len(g1_26)] == g1_26:
        g1_hits += 1
        print(f"  Match in senteSuperHighways_{i}: total len={len(m_list)}, next 3 moves={m_list[len(g1_26):len(g1_26)+3]}")
print(f"G1 total matches: {g1_hits}")

# Game 2: 27手目 P*2c まで
g2_27 = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8i7g 7c7d 2i3g 7a6b 3g4e 3c2b P*2c".split()
print("\nG2 checking 27 moves (P*2c):")
g2_hits = 0
for i, m in enumerate(gote_matches):
    m_list = [x.strip().strip('"').strip("'") for x in m.split(',')]
    if len(m_list) >= len(g2_27) and m_list[:len(g2_27)] == g2_27:
        g2_hits += 1
        print(f"  Match in goteSuperHighways_{i}: total len={len(m_list)}, next 3 moves={m_list[len(g2_27):len(g2_27)+3]}")
print(f"G2 total matches: {g2_hits}")
