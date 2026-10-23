# -*- coding: utf-8 -*-
from match_runner import UsiEngine, SUISHO_EXE, SUISHO_DIR

moves = '2g2f 3c3d 7g7f 8c8d 2f2e 8d8e 6i7h 4a3b 2e2d 2c2d 2h2d 8e8f 8g8f 8b8f 2d3d 5a4b'.split()

sente = UsiEngine('Suisho_Sente', SUISHO_EXE, SUISHO_DIR)
gote = UsiEngine('Suisho_Gote', SUISHO_EXE, SUISHO_DIR)

cur_moves = list(moves)
print(f"Starting Game 1 rollout from ply {len(cur_moves)} (5a4b)...")

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

with open("g1_ply16_5a4b_full.txt", "w", encoding="utf-8") as f:
    f.write(str(cur_moves))
print(f"Saved {len(cur_moves)} plies to g1_ply16_5a4b_full.txt")
