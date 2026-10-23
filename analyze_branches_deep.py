import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class UsiEngine:
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

    def analyze(self, moves, byoyomi_ms=2000, multipv=3):
        self.send(f"setoption name MultiPV value {multipv}")
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")

        results = []
        bestmove = None
        while True:
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            if line.startswith("info") and "multipv" in line and "score" in line and "pv" in line:
                results.append(line)
            if line.startswith("bestmove"):
                bestmove = line.split()[1]
                break
        
        # 最後の各 multipv の info を抽出
        pv_map = {}
        for l in results:
            parts = l.split()
            if "multipv" in parts:
                idx = parts.index("multipv")
                mpv = int(parts[idx+1])
                pv_map[mpv] = l
        
        return bestmove, pv_map

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

if __name__ == "__main__":
    eng = UsiEngine()
    try:
        # Game 1: 24手目 7c7d に対する先手の手
        g1_base = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d".split()
        print("=== Analyzing Game 1: Ply 24 (7c7d) ===")
        bm, pvs = eng.analyze(g1_base, byoyomi_ms=3000, multipv=5)
        print("Bestmove:", bm)
        for k, v in sorted(pvs.items()):
            print(f"  PV {k}: {v}")

        # Game 1: 25手目 P*2d, 26手目 R*2g に対する先手の手
        g1_after_r2g = g1_base + ["P*2d", "R*2g"]
        print("\n=== Analyzing Game 1: Ply 26 (R*2g) ===")
        bm, pvs = eng.analyze(g1_after_r2g, byoyomi_ms=3000, multipv=5)
        print("Bestmove:", bm)
        for k, v in sorted(pvs.items()):
            print(f"  PV {k}: {v}")

        # Game 2: 6手目 8d8e, 7手目 5i5h に対する後手の手
        g2_after_5i5h = "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 5i5h".split()
        print("\n=== Analyzing Game 2: Ply 7 (5i5h) ===")
        bm, pvs = eng.analyze(g2_after_5i5h, byoyomi_ms=3000, multipv=5)
        print("Bestmove:", bm)
        for k, v in sorted(pvs.items()):
            print(f"  PV {k}: {v}")
    finally:
        eng.close()
