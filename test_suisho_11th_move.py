# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

proc = subprocess.Popen(
    [SUISHO_EXE],
    cwd=SUISHO_DIR,
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True,
    encoding="utf-8",
    errors="replace"
)

def send(cmd):
    proc.stdin.write(cmd + "\n")
    proc.stdin.flush()

send("usi")
while True:
    line = proc.stdout.readline()
    if "usiok" in line:
        break

send("setoption name MultiPV value 5")
send("isready")
while True:
    line = proc.stdout.readline()
    if "readyok" in line:
        break

moves = ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '6i7h', '8e8f', '8g8f', '8b8f']

send(f"position startpos moves {' '.join(moves)}")
send("go btime 0 wtime 0 byoyomi 500")

while True:
    line = proc.stdout.readline()
    if not line:
        break
    line = line.strip()
    if line.startswith("info") and "multipv" in line and "score" in line:
        print(line, flush=True)
    if line.startswith("bestmove"):
        print(line, flush=True)
        break

send("quit")
proc.terminate()
