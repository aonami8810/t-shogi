# -*- coding: utf-8 -*-
import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open("g1_master_7g7f_win.txt", "r", encoding="utf-8") as f:
    master_line = ast.literal_eval(f.read().strip())

# Game 2 transposition up to ply 29:
# Note in Game 2:
# 1: 2g2f, 2: 8c8d, 3: 2f2e, 4: 8d8e, 5: 6i7h, 6: 3c3d, 7: 2e2d, 8: 2c2d, 9: 2h2d, 10: 4a3b, 11: 7g7f, 12: 8e8f, 13: 8g8f, 14: 8b8f, 15: 2d3d, 16: 2b3c, 17: 5i5h, 18: 5a5b, 19: 3g3f, 20: 8f7f, 21: 8h7g, 22: 7f7d, 23: 3d7d, 24: 7c7d, 25: P*2d, 26: P*2b, 27: R*8c, 28: P*8b, 29: 8c8e+
g2_prefix = [
    '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '3c3d', '2e2d', '2c2d', '2h2d', '4a3b',
    '7g7f', '8e8f', '8g8f', '8b8f', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'P*2b', 'R*8c', 'P*8b', '8c8e+'
]

print(f"Master line total moves: {len(master_line)}")
# The master line moves from index 29 (ply 30) onwards are:
# master_line[29] is 'R*2g', master_line[30] is '3i3h', etc.

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
p.stdin.write("usi\nisready\n")
p.stdin.flush()
while "readyok" not in p.stdout.readline(): pass
p.stdin.write("usinewgame\n")

# Test what Suisho (Sente) plays at ply 31 if Gote plays R*2g at ply 30
current = list(g2_prefix)
current.append('R*2g') # Ply 30

p.stdin.write(f"position startpos moves {' '.join(current)}\n")
p.stdin.write("go btime 0 wtime 0 byoyomi 500\n")
p.stdin.flush()

while True:
    line = p.stdout.readline().strip()
    if line.startswith("bestmove"):
        bm = line.split()[1]
        print(f"At ply 31, Suisho plays: {bm} (master_line[30] was {master_line[30]})")
        break

p.stdin.write("quit\n")
p.stdin.flush()
p.wait()
