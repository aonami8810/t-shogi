# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
p.stdin.write("usi\nisready\n")
p.stdin.flush()
while "readyok" not in p.stdout.readline(): pass
p.stdin.write("usinewgame\n")

# moves: 2g2f 3c3d 7g7f 2b8h+ 7i8h 3a2b
moves = ['2g2f', '3c3d', '7g7f', '2b8h+', '7i8h', '3a2b']
p.stdin.write(f"position startpos moves {' '.join(moves)}\n")
p.stdin.write("setoption name MultiPV value 5\n")
p.stdin.write("go btime 0 wtime 0 byoyomi 500\n")
p.stdin.flush()

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
        print(f"Rank {idx}: eval {score_type} {score_val:6s} | pv: {' '.join(pv[:8])}")
    if line.startswith("bestmove"):
        bm = line.split()[1]
        print(f"bestmove: {bm}")
        break

p.stdin.write("quit\n")
p.stdin.flush()
p.wait()
