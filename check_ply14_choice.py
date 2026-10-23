# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

proc = subprocess.Popen(
    [SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
    text=True, encoding="utf-8", errors="replace"
)

def send(cmd):
    proc.stdin.write(cmd + "\n")
    proc.stdin.flush()

send("usi")
while "usiok" not in proc.stdout.readline(): pass
send("setoption name MultiPV value 5")
send("isready")
while "readyok" not in proc.stdout.readline(): pass

moves = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b".split()
send(f"position startpos moves {' '.join(moves)}")
send("go btime 0 wtime 0 byoyomi 500")

while True:
    line = proc.stdout.readline().strip()
    if line.startswith("info") and "multipv" in line and "score" in line:
        print(line[:100])
    if line.startswith("bestmove"):
        print("BESTMOVE:", line)
        break

send("quit")
proc.wait()
