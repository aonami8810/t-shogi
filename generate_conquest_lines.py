import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class Engine:
    def __init__(self):
        self.proc = subprocess.Popen(
            [SUISHO_EXE], cwd=SUISHO_DIR,
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            text=True, encoding="utf-8"
        )
        self.send("usi")
        self.read_until("usiok")
        self.send("isready")
        self.read_until("readyok")

    def send(self, c):
        self.proc.stdin.write(c + "\n")
        self.proc.stdin.flush()

    def read_until(self, t):
        while True:
            l = self.proc.stdout.readline()
            if not l: break
            if t in l: break

    def go(self, moves, byoyomi_ms=500):
        self.send("usinewgame")
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
        bestmove = None
        score = None
        while True:
            l = self.proc.stdout.readline()
            if not l: break
            l = l.strip()
            if l.startswith("info") and "score" in l:
                parts = l.split()
                if "cp" in parts:
                    idx = parts.index("cp")
                    score = parts[idx+1]
                elif "mate" in parts:
                    idx = parts.index("mate")
                    score = "mate " + parts[idx+1]
            if l.startswith("bestmove"):
                bestmove = l.split()[1]
                break
        return bestmove, score

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

def simulate(prefix, max_plies=80):
    eng = Engine()
    moves = list(prefix)
    print(f"=== Simulating from prefix ({len(moves)} moves) ===")
    print("Prefix:", " ".join(prefix))
    try:
        for ply in range(len(prefix) + 1, max_plies + 1):
            bm, score = eng.go(moves, byoyomi_ms=500)
            score_str = f"({score})" if score else ""
            print(f"Ply {ply:3d}: {bm:6s} {score_str}")
            if bm in ["resign", "win"]:
                break
            moves.append(bm)
    finally:
        eng.close()
    return moves

print("\n--- SIMULATION GAME 1 ---")
g1_prefix = ["7g7f", "8c8d", "2g2f", "4a3b", "6i7h", "8d8e", "2f2e"]
m1 = simulate(g1_prefix, max_plies=60)
print("Game 1 Complete Moves:")
print(repr(m1))

print("\n--- SIMULATION GAME 2 ---")
g2_prefix = ["2g2f", "8c8d", "2f2e", "8d8e", "7g7f", "3c3d", "5i5h", "2b8h+"]
m2 = simulate(g2_prefix, max_plies=60)
print("Game 2 Complete Moves:")
print(repr(m2))
