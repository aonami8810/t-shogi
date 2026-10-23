# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def get_suisho():
    proc = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, encoding="utf-8")
    def send(cmd):
        proc.stdin.write(cmd + "\n")
        proc.stdin.flush()
    def read_until(target):
        while True:
            line = proc.stdout.readline()
            if not line or target in line:
                return line
    send("usi")
    read_until("usiok")
    send("isready")
    read_until("readyok")
    send("usinewgame")
    return proc

proc = get_suisho()
moves_g1 = ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2a3c', '5i5h', '5a5b', '3i4h', '1c1d', '4i3h', '7a7b', '9g9f', '9c9d']

print("=== Testing Game 1 (25th move) ===")
for cand in ['3d2d', '3g3f', '7h8h', '8h7g']:
    test_moves = moves_g1 + [cand]
    proc.stdin.write(f"position startpos moves {' '.join(test_moves)}\n")
    proc.stdin.write("go btime 0 wtime 0 byoyomi 500\n")
    proc.stdin.flush()
    bestmove = None
    info = None
    while True:
        line = proc.stdout.readline()
        if not line: break
        line = line.strip()
        if line.startswith("info depth") and "score" in line:
            info = line
        if line.startswith("bestmove"):
            bestmove = line.split()[1]
            break
    print(f"Cand {cand:6s} -> Suisho5 reply: {bestmove:6s} | {info}")

proc.kill()
