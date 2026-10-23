# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

moves = [
    '2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '5i5h', '8e8f', '8g8f', '5a5b',
    '8h2b+', '3a2b', 'B*7g', '4a3b', '2e2d', '2c2d', '2h2d', '2a3c', '7i7h', 'P*8h',
    '7g8h', '8b8f', '8h7g', '8f7f', '2d3d', '7f2f', 'P*2h', '7c7d', '7g5e',
    'B*7c', '5e7c+', '8a7c', '3d7d'
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
