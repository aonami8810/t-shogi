# -*- coding: utf-8 -*-
from match_runner import UsiEngine, SUISHO_EXE, SUISHO_DIR

moves = '2g2f 3c3d 7g7f 8c8d 2f2e 8d8e 6i7h 4a3b 2e2d 2c2d 2h2d 8e8f 8g8f 8b8f 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d 2i3g R*2i 3g4e 3c4d P*8b 7a8b 7g4d 4c4d B*5e B*6d 5e6d 6c6d B*3d B*4c 4e5c+ 5b5c 3d4c+ 3b4c R*4a N*6f 6g6f 3a4b P*8c B*9d P*7f 9d7f B*6g 7f6g+ 7h6g 6a5a 4a5a+ 4b5a 8c8b+ R*8g'.split()

sente = UsiEngine('Suisho_Sente', SUISHO_EXE, SUISHO_DIR)
gote = UsiEngine('Suisho_Gote', SUISHO_EXE, SUISHO_DIR)

cur_moves = list(moves)
print(f"Starting Game 1 rollout from ply {len(cur_moves)} (R*8g)...")

for ply in range(len(cur_moves) + 1, 200):
    is_sente = (ply % 2 == 1)
    if is_sente:
        # 先手（Morse側）は高精度（3500ms）
        mv, sc, pv, _ = sente.go(cur_moves, byoyomi_ms=3500)
    else:
        # 後手（水匠5）は実戦と同一条件（500ms）
        mv, sc, pv, _ = gote.go(cur_moves, byoyomi_ms=500)
        
    print(f"Ply {ply:3d} ({'Sente (Morse)' if is_sente else 'Gote (Suisho5)'}): {mv:6s} ({sc})", flush=True)
    if mv == 'resign':
        print(f"Game over at ply {ply}: {'Sente' if is_sente else 'Gote'} resigned!")
        break
    cur_moves.append(mv)

sente.close()
gote.close()

with open("g1_ply58_r8g_full.txt", "w", encoding="utf-8") as f:
    f.write(str(cur_moves))
print(f"Saved {len(cur_moves)} plies to g1_ply58_r8g_full.txt")
