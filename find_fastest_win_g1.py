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

    def go_byoyomi(self, moves, byoyomi_ms=500):
        self.send(f"position startpos moves {' '.join(moves)}")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
        bestmove = None
        score = None
        while True:
            line = self.proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            if line.startswith("info") and "score" in line and "pv" in line:
                parts = line.split()
                if "cp" in parts:
                    idx = parts.index("cp")
                    if idx + 1 < len(parts):
                        score = parts[idx + 1]
                elif "mate" in parts:
                    idx = parts.index("mate")
                    if idx + 1 < len(parts):
                        score = "mate " + parts[idx + 1]
            if line.startswith("bestmove"):
                bestmove = line.split()[1]
                break
        return bestmove, score

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

if __name__ == "__main__":
    eng = UsiEngine()
    try:
        # Game 1 の 160手以降、水匠5がいつ投了するか？
        from run_to_end import G1_120
        # 121手目から継続
        cur = list(G1_120)
        for ply in range(121, 200):
            bm, sc = eng.go_byoyomi(cur, byoyomi_ms=500)
            turn = "Sente" if (ply % 2 == 1) else "Gote"
            print(f"Ply {ply:3d} [{turn}]: {bm:6s} (eval: {sc})")
            if bm == "resign" or bm == "win":
                print(f"TERMINATED AT PLY {ply} with {bm}!")
                break
            cur.append(bm)
    finally:
        eng.close()
