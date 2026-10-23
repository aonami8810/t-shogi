# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

gote_crush = [
    '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '3c3d', '2e2d', '2c2d', '2h2d', '4a3b',
    '7g7f', '8e8f', '8g8f', '8b8f', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8i7g',
    '3c5e' # Ply 22: 5五銀！
]

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
p.stdin.write("usi\nisready\n")
p.stdin.flush()
while "readyok" not in p.stdout.readline(): pass
p.stdin.write("usinewgame\n")

print("Checking Suisho response to Ply 22 3c5e...")
p.stdin.write(f"position startpos moves {' '.join(gote_crush)}\n")
p.stdin.write("go btime 0 wtime 0 byoyomi 500\n")
p.stdin.flush()

while True:
    line = p.stdout.readline().strip()
    if line.startswith("bestmove"):
        bm = line.split()[1]
        print(f"Suisho bestmove at Ply 23: {bm}")
        break

p.stdin.write("quit\n")
p.stdin.flush()
p.wait()
