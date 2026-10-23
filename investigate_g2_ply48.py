# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

moves = ['2g2f', '8c8d', '7g7f', '3c3d', '6i7h', '8d8e', '2f2e', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '5i5h', '8f7f', '8h2b+', '3a2b', '2d3d', '2b3c', '3d3f', '7f3f', '3g3f', 'P*8b', '3i3h', '5a5b', '8i7g', '7a7b', '2i3g', 'R*2h', '7g6e', '3c4d', 'P*2g', 'B*2i', 'R*3i', '2i1h+', '1i1h', '2h1h+', '3i2i', 'P*3e', '4i3i', '3e3f', '3i2h', '1h2i', '2h2i', '3f3g+', '3h3g']

send(f"position startpos moves {' '.join(moves)}")
send("setoption name MultiPV value 8")
send("go nodes 6000000")

while True:
    l = p.stdout.readline().strip()
    if l.startswith("info") and "score" in l and "nodes" in l:
        print(l)
    if l.startswith("bestmove"):
        print("FINAL:", l)
        break

send("quit")
