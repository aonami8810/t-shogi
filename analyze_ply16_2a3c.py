# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class SuishoSession:
    def __init__(self):
        self.proc = subprocess.Popen(
            [SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            text=True, encoding="utf-8", errors="replace"
        )
        self.send("usi")
        self.read_until("usiok")
        self.send("isready")
        self.read_until("readyok")

    def send(self, cmd):
        self.proc.stdin.write(cmd + "\n")
        self.proc.stdin.flush()

    def read_until(self, target):
        lines = []
        while True:
            l = self.proc.stdout.readline()
            if not l: break
            l = l.strip()
            lines.append(l)
            if target in l: break
        return lines

    def analyze(self, moves, byoyomi_ms=1000, multipv=5):
        self.send(f"setoption name MultiPV value {multipv}")
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")

        bestmove = None
        pv_map = {}
        while True:
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            if line.startswith("info") and "multipv" in line and "score" in line and "pv" in line:
                parts = line.split()
                if "multipv" in parts:
                    idx = parts.index("multipv")
                    mpv = int(parts[idx+1])
                    pv_map[mpv] = line
            if line.startswith("bestmove"):
                bestmove = line.split()[1]
                break
        return bestmove, pv_map

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

if __name__ == "__main__":
    session = SuishoSession()
    # 16手目 2a3c まで
    moves = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2a3c".split()
    print("=== Analyzing 17th move for Sente against 16th 2a3c ===")
    bm, pvs = session.analyze(moves, byoyomi_ms=2000, multipv=5)
    for k, v in sorted(pvs.items()):
        print(f"PV {k}: {v}")
    session.close()
