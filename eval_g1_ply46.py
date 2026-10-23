# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

moves = [
    '7g7f', '8c8d', '2g2f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2a3c', 'P*8g', '8f8e', '3d2d', '1c1d',
    '5i5h', '7a7b', '8h6f', '5a5b', '8i7g', '8e3e', '7i6h', '7c7d', '3g3f', '3e3f',
    'P*3d', '3c4e', '6f2b+', '3a2b', '3d3c+', '3f3c', 'B*5e', 'P*2c', '2d2e', '8a7c',
    '2e4e', 'B*2g', '5e3c+', '2b3c', '4e3e', '2g4i+'
]

send(f"position startpos moves {' '.join(moves)}")
send("setoption name MultiPV value 5")
send("go nodes 5000000")

while True:
    l = p.stdout.readline().strip()
    if l.startswith("info") and "multipv" in l:
        print(l)
    if l.startswith("bestmove"):
        print("FINAL:", l)
        break
send("quit")
