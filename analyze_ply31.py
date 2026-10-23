# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class SuishoSession:
    def __init__(self):
        self.proc = subprocess.Popen(
            [SUISHO_EXE],
            cwd=SUISHO_DIR,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            encoding="utf-8",
            errors="replace"
        )
        self.send("usi")
        self.read_until("usiok")
        self.send("isready")
        self.read_until("readyok")
        self.send("usinewgame")

    def send(self, cmd):
        self.proc.stdin.write(cmd + "\n")
        self.proc.stdin.flush()

    def read_until(self, target):
        lines = []
        while True:
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            lines.append(line)
            if target in line: break
        return lines

    def analyze(self, moves, byoyomi_ms=2000, multipv=8):
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
    # 31手目 2e4e まで
    moves = "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 5i5h 8e8f 8g8f 8b8f 8h2b+ 3a2b B*7g 8f8b P*8c 8b3b 2e2d 2c2d 2h2d P*2c 2d2e 6a7b 7i7h 7b8c 3g3f B*6d 3i2h 8c7d 2h3g 4a3a 2e4e".split()
    print("=== Analyzing Ply 31: 2e4e ===")
    bm, pvs = session.analyze(moves, byoyomi_ms=2000, multipv=8)
    for k, v in sorted(pvs.items()):
        print(f"PV {k}: {v}")
    session.close()
