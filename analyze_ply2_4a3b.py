# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def analyze(moves_str):
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
    send("setoption name MultiPV value 3")
    send("isready")
    read_until("readyok")
    send("usinewgame")

    send(f"position startpos moves {moves_str}")
    send("go btime 0 wtime 0 byoyomi 3000")

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

    print(f"=== Analysis for '{moves_str}' ===")
    for k in sorted(pvs.keys(), key=int):
        print(f"PV {k}: {pvs[k]}")
    print("Bestmove:", bestmove)
    return bestmove

if __name__ == "__main__":
    analyze("7g7f 4a3b")
    analyze("2g2f 4a3b")
