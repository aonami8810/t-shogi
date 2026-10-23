# -*- coding: utf-8 -*-
from match_runner import UsiEngine, SUISHO_EXE, SUISHO_DIR

moves = '7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d P*2d P*2b R*8c P*8b 8c8e+ R*2g 3i3h 2g2d+ P*8c'.split()

sente = UsiEngine('Suisho_Sente', SUISHO_EXE, SUISHO_DIR)
gote = UsiEngine('Suisho_Gote', SUISHO_EXE, SUISHO_DIR)

print(f"Starting rollout from ply {len(moves)}...")

cur_moves = list(moves)
for ply in range(len(moves) + 1, 250):
    is_sente = (ply % 2 == 1)
    engine = sente if is_sente else gote
    byo = 500 if is_sente else 1500  # 後手（Morse側）に長めの時間を取らせて必勝手を指させる
    mv, sc, pv, _ = engine.go(cur_moves, byoyomi_ms=byo)
    print(f"Ply {ply:3d} ({'Sente' if is_sente else 'Gote'}): {mv} ({sc})", flush=True)
    if mv == 'resign':
        print(f"Winner: {'Gote' if is_sente else 'Sente'} at ply {ply}")
        break
    cur_moves.append(mv)

sente.close()
gote.close()

with open("g2_8h7g_p8c_full.txt", "w", encoding="utf-8") as f:
    f.write(str(cur_moves))
print(f"Saved {len(cur_moves)} plies to g2_8h7g_p8c_full.txt")
