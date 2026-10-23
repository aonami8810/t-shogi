# -*- coding: utf-8 -*-
import subprocess

MORSE_EXE = r"c:\VS\Workspace\t-shogi\shogi-usi-morse.exe"
MORSE_DIR = r"c:\VS\Workspace\t-shogi"

proc = subprocess.Popen(
    [MORSE_EXE], cwd=MORSE_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
    text=True, encoding="utf-8", errors="replace"
)
def send(c): proc.stdin.write(c + "\n"); proc.stdin.flush()
def read_until(target):
    while True:
        l = proc.stdout.readline()
        if not l or target in l: break

send("usi")
read_until("usiok")
send("isready")
read_until("readyok")

moves = "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d P*2d R*2f 3i3h P*2h 7g3c+ 2a3c B*3g 2f2d 3g9a+ 2h2i+ 9a8a 2i2h"
send(f"position startpos moves {moves}")
send("go btime 0 wtime 0 byoyomi 500")

while True:
    line = proc.stdout.readline()
    if not line: break
    line = line.strip()
    print("MORSE:", line)
    if line.startswith("bestmove"):
        break

send("quit")
proc.wait()
