# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

moves = [
    '2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8h7g', '7f7d', '3d7d', '7c7d', '2i3g', '3c7g+', '8i7g', 'B*6d', '3i3h', 'R*2h',
    'B*4f', '6d4f', '4g4f', '2a3c', 'P*2i', '2h2d+', 'P*8b'
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
