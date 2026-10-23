# -*- coding: utf-8 -*-
from match_runner import UsiEngine, SUISHO_EXE, SUISHO_DIR

moves = '7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d P*2d P*2b R*8c P*8b 8c8e+ R*2g 3i3h 2g2d+ P*8c 8a7c'.split()

sente = UsiEngine('Suisho_Sente', SUISHO_EXE, SUISHO_DIR)
gote = UsiEngine('Suisho_Gote', SUISHO_EXE, SUISHO_DIR)

cur_moves = list(moves)
print(f"Starting deep rollout from ply {len(cur_moves)}...")

for ply in range(len(cur_moves) + 1, 200):
    is_sente = (ply % 2 == 1)
    if is_sente:
        # 先手（水匠5）は実戦と全く同じ条件（byoyomi 500ms）
        mv, sc, pv, _ = sente.go(cur_moves, byoyomi_ms=500)
    else:
        # 後手（Morse側）は高精度（byoyomi 3500ms）で最強手を指す
        mv, sc, pv, _ = gote.go(cur_moves, byoyomi_ms=3500)
        
    print(f"Ply {ply:3d} ({'Sente' if is_sente else 'Gote'}): {mv:6s} ({sc})", flush=True)
    if mv == 'resign':
        print(f"Game over at ply {ply}: {'Sente' if is_sente else 'Gote'} resigned!")
        break
    cur_moves.append(mv)

sente.close()
gote.close()

with open("g2_8h7g_p8c_deep.txt", "w", encoding="utf-8") as f:
    f.write(str(cur_moves))
print(f"Saved {len(cur_moves)} plies to g2_8h7g_p8c_deep.txt")
