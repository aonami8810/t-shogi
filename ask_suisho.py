# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen(
    [SUISHO_EXE],
    cwd=SUISHO_DIR,
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    text=True,
    encoding="utf-8"
)

def send(cmd):
    p.stdin.write(cmd + "\n")
    p.stdin.flush()

send("usi")
while True:
    line = p.stdout.readline()
    if "usiok" in line:
        break

send("isready")
while True:
    line = p.stdout.readline()
    if "readyok" in line:
        break

send("usinewgame")
send("position startpos")
send("go btime 0 wtime 0 byoyomi 500")

while True:
    line = p.stdout.readline()
    if not line:
        break
    line = line.strip()
    if line.startswith("bestmove"):
        print("Suisho初手:", line)
        break

send("quit")
p.wait(timeout=2)
