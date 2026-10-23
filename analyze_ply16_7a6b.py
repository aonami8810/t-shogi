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

moves = [
    '7g7f', '8c8d', '2g2f', '8d8e', '2f2e', '4a3b', '8h7g', '3c3d', '7i8h', '2b7g+',
    '8h7g', '3a2b', '3g3f', '2b3c', '6i7h', '7a6b'
]

send(f"position startpos moves {' '.join(moves)}")
send("go nodes 10000000")

while True:
    line = proc.stdout.readline()
    if not line:
        break
    line = line.strip()
    if line.startswith("info") and "multipv" in line and "score" in line and "pv" in line:
        print(line, flush=True)
    if line.startswith("bestmove"):
        print(line, flush=True)
        break

send("quit")
proc.terminate()
