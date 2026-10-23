# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
p.stdin.write("usi\nisready\n")
p.stdin.flush()
while "readyok" not in p.stdout.readline(): pass
p.stdin.write("usinewgame\n")

# Ply 1: 2g2f
# If Gote plays 3c3d:
p.stdin.write("position startpos moves 2g2f 3c3d\n")
p.stdin.write("go btime 0 wtime 0 byoyomi 500\n")
p.stdin.flush()

while True:
    line = p.stdout.readline().strip()
    if line.startswith("bestmove"):
        bm = line.split()[1]
        print(f"If Gote plays 2... 3c3d, Suisho plays Ply 3: {bm}")
        break

p.stdin.write("quit\n")
p.stdin.flush()
p.wait()
