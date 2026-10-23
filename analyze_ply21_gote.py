# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def analyze():
    proc = subprocess.Popen(
        [SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
        text=True, encoding="utf-8", errors="replace"
    )
    def send(c): proc.stdin.write(c + "\n"); proc.stdin.flush()
    def read_until(target):
        while True:
            l = proc.stdout.readline()
            if not l or target in l: break

    send("usi")
    read_until("usiok")
    send("setoption name MultiPV value 5")
    send("isready")
    read_until("readyok")
    send("usinewgame")

    moves = "2g2f 3c3d 7g7f 8c8d 6i7h 8d8e 2f2e 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8i7g".split()
    send(f"position startpos moves {' '.join(moves)}")
    send("go btime 0 wtime 0 byoyomi 4000")

    pvs = {}
    bestmove = None
    while True:
        line = proc.stdout.readline()
        if not line: break
        line = line.strip()
        if line.startswith("info") and "multipv" in line and "score" in line:
            parts = line.split()
            mpv = parts[parts.index("multipv") + 1]
            pvs[mpv] = line
        if line.startswith("bestmove"):
            bestmove = line.split()[1]
            break

    send("quit")
    proc.wait()

    print("=== Analysis at ply 21 (8i7g) ===")
    for k in sorted(pvs.keys(), key=int):
        print(f"PV {k}: {pvs[k]}")
    print("Bestmove:", bestmove)

if __name__ == "__main__":
    analyze()
