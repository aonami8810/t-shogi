# -*- coding: utf-8 -*-
import subprocess
from clean_g2_mate import PERFECT_G2_CONQUEST_LINE

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8", errors="replace")
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi")
send("isready")
while "readyok" not in p.stdout.readline(): pass

moves = list(PERFECT_G2_CONQUEST_LINE)
print("Moves length:", len(moves))
send(f"position startpos moves {' '.join(moves)}")
send("go btime 0 wtime 0 byoyomi 500")

while True:
    line = p.stdout.readline().strip()
    if line.startswith("info"):
        print(line)
    if line.startswith("bestmove"):
        print("BESTMOVE:", line)
        break
send("quit")
