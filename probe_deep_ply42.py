# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

moves = [
    '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '3c3d', '2e2d', '2c2d', '2h2d', '4a3b',
    '7g7f', '8e8f', '8g8f', '8b8f', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'P*2b', 'R*8c', 'P*8b', '8c8e+', 'R*2g',
    '3i3h', '2g2h+', '7g3c+', '3b3c', 'B*5e', 'B*7c', '5e2h', '7c2h+', '2i3g', '2h1i',
    '8i7g'
]

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
p.stdin.write("usi\nisready\n")
p.stdin.flush()
while "readyok" not in p.stdout.readline(): pass
p.stdin.write("usinewgame\n")

p.stdin.write(f"position startpos moves {' '.join(moves)}\n")
p.stdin.write("setoption name MultiPV value 8\n")
p.stdin.write("go nodes 10000000\n")
p.stdin.flush()

mpvs = {}
while True:
    line = p.stdout.readline().strip()
    if line.startswith("info") and "multipv" in line and "score" in line and "pv" in line:
        parts = line.split()
        idx = int(parts[parts.index("multipv") + 1])
        sc_idx = parts.index("score") + 1
        score_type = parts[sc_idx]
        score_val = parts[sc_idx + 1]
        pv_idx = parts.index("pv") + 1
        pv = parts[pv_idx:]
        mpvs[idx] = (f"{score_type} {score_val}", pv[:8])
    if line.startswith("bestmove"):
        break

for idx in sorted(mpvs.keys()):
    sc, pv = mpvs[idx]
    print(f"Rank {idx:2d}: eval {sc:10s} | pv: {' '.join(pv)}")

p.stdin.write("quit\n")
p.stdin.flush()
p.wait()
