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
        self.send("setoption name MultiPV value 5")
        self.send("isready")
        self.read_until("readyok")

    def send(self, cmd):
        self.proc.stdin.write(cmd + "\n")
        self.proc.stdin.flush()

    def read_until(self, target):
        lines = []
        while True:
            line = self.proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            lines.append(line)
            if target in line:
                break
        return lines

    def analyze(self, moves, depth=18):
        self.send(f"position startpos moves {' '.join(moves)}")
        self.send(f"go depth {depth}")
        candidates = []
        while True:
            line = self.proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            if line.startswith("info depth") and "multipv" in line and "score" in line and "pv" in line:
                candidates.append(line)
            if line.startswith("bestmove"):
                break
        return candidates[-5:]

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

if __name__ == "__main__":
    eng = UsiEngine()
    try:
        # Game 1: 16 plies
        g1 = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c".split()
        print("=== Game 1 after 2b3c (16 plies) ===")
        for line in eng.analyze(g1, depth=20):
            print(line)

        # Game 2: 3 plies
        g2 = "7g7f 8c8d 8h7g".split()
        print("\n=== Game 2 after 8h7g (3 plies) ===")
        for line in eng.analyze(g2, depth=20):
            print(line)

    finally:
        eng.close()
