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
            if not line:
                break
            line = line.strip()
            lines.append(line)
            if target in line:
                break
        return lines

    def find_mate(self, moves, depth=25):
        self.send(f"position startpos moves {' '.join(moves)}")
        self.send(f"go mate 25")
        mate_line = None
        while True:
            line = self.proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            if line.startswith("checkmate"):
                mate_line = line
                break
            if line.startswith("bestmove"):
                break
        return mate_line

    def analyze(self, moves, depth=22):
        self.send(f"position startpos moves {' '.join(moves)}")
        self.send(f"go depth {depth}")
        bestmove = None
        score = None
        pv = None
        while True:
            line = self.proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            if line.startswith("info depth") and "score" in line and "pv" in line:
                parts = line.split()
                if "cp" in parts:
                    idx = parts.index("cp")
                    if idx + 1 < len(parts):
                        score = parts[idx + 1]
                elif "mate" in parts:
                    idx = parts.index("mate")
                    if idx + 1 < len(parts):
                        score = "mate " + parts[idx + 1]
                if "pv" in parts:
                    idx = parts.index("pv")
                    pv = parts[idx + 1:]
            if line.startswith("bestmove"):
                bestmove = line.split()[1]
                break
        return bestmove, score, pv

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
        bm, sc, pv = eng.analyze(g1, depth=24)
        print(f"G1 16 plies best: {bm} (score: {sc}) pv: {' '.join(pv[:15])}")

        # Game 2: 3 plies
        g2 = "7g7f 8c8d 8h7g".split()
        bm2, sc2, pv2 = eng.analyze(g2, depth=24)
        print(f"G2 3 plies best: {bm2} (score: {sc2}) pv: {' '.join(pv2[:15])}")
    finally:
        eng.close()
