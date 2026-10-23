# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

moves = [
    '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '3c3d', '2e2d', '2c2d', '2h2d', '4a3b',
    '7g7f', '8e8f', '8g8f', '8b8f', '2d3d', '5a4b', '5i5h', '7a7b', '9g9f', '1c1d',
    '3d2d', '4b5b', '3i3h', '7c7d', '2d2f', '7d7e', '3g3f', '7e7f', 'P*8g', '8f8e',
    '2i3g', '2b8h+', '7i8h', '3a2b', 'B*6f', 'B*3c', '4g4f', '1d1e', '3h4g', '3c6f',
    '6g6f', '1e1f', '1g1f', '2a3c', '4i3h', 'P*1g', '1i1g', 'B*4d', '2f2g', 'P*2f',
    '2g2i', '3c2e', '3g2e', '8e2e', '4f4e', '2e4e', 'B*8b', '4d5e', '8b5e+', '4e5e',
    'B*4f', '5e2e', 'N*3g', '2f2g+', '3g2e', '2g3h', '4g3h', 'B*1h', '4f9a+', '1h2i+',
    '3h2i', 'R*2g', 'L*3d', '3b4b', '3d3c+', '2b3c', '2e3c+', '4b3c', '2i2h', 'G*4g',
    '5h6i', '2g2e+', 'P*4d', '4c4d', 'P*2f', '2e3f', 'R*3a', '3f3h', 'B*2e', '5b4b',
    '2e4g', '3h4g', 'G*4a', '4b4c', 'S*5h', 'B*1d', '5h4g', '1d4g+', '6i7i', '3c3b',
    '3a3b+', '4c3b', 'R*4b', '3b3c', 'G*2d', '3c2d', '4b4d+', 'S*3d', '4d4g', 'R*5i',
    '7i6h', 'G*6i', '6h6g', 'N*6d', 'B*4b', 'P*3c', '2f2e', '2d2e', '9a6d', '6c6d',
    '4b3c+', 'N*7e', '6g7f', 'B*5h', '4g5h', '5i5h+', 'P*2f', '2e3f', 'P*3g', '3f4g',
    'B*3f', '4g3h', '3f5h', '3h2h', '5h6i'
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

print(f"=== Position at Ply 135 (Gote to move) ===")
for idx in sorted(mpvs.keys()):
    sc, pv = mpvs[idx]
    print(f"Rank {idx:2d}: eval {sc:10s} | pv: {' '.join(pv)}")

p.stdin.write("quit\n")
p.stdin.flush()
p.wait()
