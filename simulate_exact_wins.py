import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class Engine:
    def __init__(self):
        self.proc = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8")
        self.send("usi\nisready\n")
        while True:
            if "readyok" in self.proc.stdout.readline(): break
    def send(self, c):
        self.proc.stdin.write(c + "\n")
        self.proc.stdin.flush()
    def go(self, moves, byoyomi_ms=500):
        self.send("usinewgame")
        self.send(f"position startpos moves {' '.join(moves)}")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
        bm = None
        sc = None
        while True:
            l = self.proc.stdout.readline().strip()
            if l.startswith("info") and "score" in l:
                parts = l.split()
                if "cp" in parts: sc = parts[parts.index("cp")+1]
                elif "mate" in parts: sc = "mate " + parts[parts.index("mate")+1]
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        return bm, sc
    def close(self):
        try:
            self.send("quit\n")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

def run_sim(prefix, plies=60):
    eng = Engine()
    moves = list(prefix)
    try:
        for ply in range(len(prefix)+1, len(prefix)+1+plies):
            bm, sc = eng.go(moves)
            print(f"Ply {ply:3d}: {bm:6s} ({sc})", flush=True)
            if bm in ["resign", "win"]: break
            moves.append(bm)
    finally:
        eng.close()
    return moves

print("=== GAME 1 SIMULATION (from 8h2b+) ===")
g1_pref = ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "6i7h", "4a3b", "2e2d", "2c2d", "2h2d", "8e8f", "8g8f", "8b8f", "5i5h", "8f7f", "8h2b+"]
m1 = run_sim(g1_pref, plies=45)
print("Game 1 complete moves:", repr(m1))

print("\n=== GAME 2 SIMULATION (from 3c3d after 6i7h) ===")
g2_pref = ["2g2f", "8c8d", "7g7f", "4a3b", "6i7h", "3c3d"]
m2 = run_sim(g2_pref, plies=45)
print("Game 2 complete moves:", repr(m2))
