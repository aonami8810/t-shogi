import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class MateFinder:
    def __init__(self):
        self.proc = subprocess.Popen(
            [SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True
        )
        self.send("usi")
        self.read_until("usiok")
        self.send("isready")
        self.read_until("readyok")

    def send(self, cmd): self.proc.stdin.write(cmd + "\n"); self.proc.stdin.flush()
    def read_until(self, target):
        lines = []
        while True:
            l = self.proc.stdout.readline()
            if not l: break
            lines.append(l.strip())
            if target in l: break
        return lines

    def search_mate(self, moves, depth=28):
        self.send(f"position startpos moves {' '.join(moves)}")
        self.send(f"go depth {depth}")
        bestmove = None
        score = None
        pv = []
        while True:
            l = self.proc.stdout.readline().strip()
            if l.startswith("info depth") and "score" in l and "pv" in l:
                parts = l.split()
                if "mate" in parts:
                    idx = parts.index("mate")
                    score = "mate " + parts[idx+1]
                elif "cp" in parts:
                    idx = parts.index("cp")
                    score = parts[idx+1]
                if "pv" in parts:
                    pv = parts[parts.index("pv")+1:]
            if l.startswith("bestmove"):
                bestmove = l.split()[1]
                break
        return bestmove, score, pv

    def close(self):
        try: self.send("quit"); self.proc.wait(timeout=2)
        except: self.proc.kill()

if __name__ == "__main__":
    mf = MateFinder()
    try:
        # Game 1: 16 plies
        g1 = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c".split()
        print("=== Checking G1 16 plies ===")
        bm, sc, pv = mf.search_mate(g1, depth=22)
        print(f"G1 16 plies: bm={bm}, sc={sc}, pv={' '.join(pv[:10])}")

        # Game 2: 3 plies
        g2 = "7g7f 8c8d 8h7g".split()
        print("\n=== Checking G2 3 plies ===")
        bm, sc, pv = mf.search_mate(g2, depth=22)
        print(f"G2 3 plies: bm={bm}, sc={sc}, pv={' '.join(pv[:10])}")

    finally:
        mf.close()
