# -*- coding: utf-8 -*-
import re

with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    text = f.read()

sente_matches = re.findall(r'senteSuperHighways_\d+\s*=\s*\[(.*?)\]', text)
print(f'Total senteSuperHighways: {len(sente_matches)}')

gote_matches = re.findall(r'goteSuperHighways_\d+\s*=\s*\[(.*?)\]', text)
print(f'Total goteSuperHighways: {len(gote_matches)}')

# Game 1: 26手目 R*2i まで
g1_26 = "2g2f 3c3d 7g7f 8c8d 2f2e 8d8e 6i7h 4a3b 2e2d 2c2d 2h2d 8e8f 8g8f 8b8f 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d 2i3g R*2i".split()
print("\nG1 checking 26 moves (R*2i):")
g1_hits = 0
for i, m in enumerate(sente_matches):
    m_list = [x.strip().strip('"').strip("'") for x in m.split(',')]
    if len(m_list) >= len(g1_26) and m_list[:len(g1_26)] == g1_26:
        g1_hits += 1
        print(f"  Match in senteSuperHighways_{i}: total len={len(m_list)}, next 3 moves={m_list[len(g1_26):len(g1_26)+3]}")
print(f"G1 total matches: {g1_hits}")

# Game 2: 実戦の 5手目 6i7h まで
g2_5 = "2g2f 3c3d 7g7f 8c8d 6i7h".split()
print("\nG2 checking 5 moves (6i7h):")
g2_hits = 0
for i, m in enumerate(gote_matches):
    m_list = [x.strip().strip('"').strip("'") for x in m.split(',')]
    if len(m_list) >= len(g2_5) and m_list[:len(g2_5)] == g2_5:
        g2_hits += 1
        if g2_hits <= 3:
            print(f"  Match in goteSuperHighways_{i}: total len={len(m_list)}, next 3 moves={m_list[len(g2_5):len(g2_5)+3]}")
print(f"G2 total matches: {g2_hits}")
