# -*- coding: utf-8 -*-
import re

with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    text = f.read()

sente_matches = re.findall(r'senteSuperHighways_\d+\s*=\s*\[(.*?)\]', text)
print(f'Total senteSuperHighways: {len(sente_matches)}')

gote_matches = re.findall(r'goteSuperHighways_\d+\s*=\s*\[(.*?)\]', text)
print(f'Total goteSuperHighways: {len(gote_matches)}')

# Game 1: 16手目 2b8h+ まで
g1_16 = "2g2f 3c3d 7g7f 8c8d 2f2e 8d8e 6i7h 4a3b 2e2d 2c2d 2h2d 8e8f 8g8f 8b8f 2d3d 2b8h+".split()
print("\nG1 checking 16 moves (2b8h+):")
g1_hits = 0
for i, m in enumerate(sente_matches):
    m_list = [x.strip().strip('"').strip("'") for x in m.split(',')]
    if len(m_list) >= len(g1_16) and m_list[:len(g1_16)] == g1_16:
        g1_hits += 1
        print(f"  Match in senteSuperHighways_{i}: total len={len(m_list)}, next 3 moves={m_list[len(g1_16):len(g1_16)+3]}")
print(f"G1 total matches: {g1_hits}")

# Game 2: 15手目 5i5h まで
g2_15 = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 5i5h".split()
print("\nG2 checking 15 moves (5i5h):")
g2_hits = 0
for i, m in enumerate(gote_matches):
    m_list = [x.strip().strip('"').strip("'") for x in m.split(',')]
    if len(m_list) >= len(g2_15) and m_list[:len(g2_15)] == g2_15:
        g2_hits += 1
        print(f"  Match in goteSuperHighways_{i}: total len={len(m_list)}, next 3 moves={m_list[len(g2_15):len(g2_15)+3]}")
print(f"G2 total matches: {g2_hits}")
