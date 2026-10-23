# -*- coding: utf-8 -*-
import re

with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    text = f.read()

sente_matches = re.findall(r'senteSuperHighways_\d+\s*=\s*\[(.*?)\]', text)
print(f'Total senteSuperHighways: {len(sente_matches)}')

gote_matches = re.findall(r'goteSuperHighways_\d+\s*=\s*\[(.*?)\]', text)
print(f'Total goteSuperHighways: {len(gote_matches)}')

# Game 1 の確認 (36手目 2i2h まで)
g1_so_far = "2g2f 3c3d 7g7f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d P*2d R*2f 3i3h P*2h 7g3c+ 2a3c B*3g 2f2d 3g9a+ 2h2i+ 9a8a 2i2h".split()
print("G1 checking 36 moves:")
g1_hits = 0
for i, m in enumerate(sente_matches):
    m_list = [x.strip().strip('"').strip("'") for x in m.split(',')]
    if len(m_list) >= len(g1_so_far) and m_list[:len(g1_so_far)] == g1_so_far:
        g1_hits += 1
        print(f"  Match in senteSuperHighways_{i}: total len={len(m_list)}, next 3 moves={m_list[len(g1_so_far):len(g1_so_far)+3]}")
print(f"G1 total matches: {g1_hits}")

# Game 2 の確認 (3手目 2f2e まで)
g2_3moves = "2g2f 3c3d 2f2e".split()
print("\nG2 checking 3 moves:", g2_3moves)
g2_hits = 0
for i, m in enumerate(gote_matches):
    m_list = [x.strip().strip('"').strip("'") for x in m.split(',')]
    if len(m_list) >= 3 and m_list[:3] == g2_3moves:
        g2_hits += 1
        print(f"  Match in goteSuperHighways_{i}: total len={len(m_list)}, next 3 moves={m_list[3:6]}")
print(f"G2 total matches: {g2_hits}")
