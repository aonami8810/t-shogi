import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class Engine:
    def __init__(self):
        self.proc = subprocess.Popen(
            [SUISHO_EXE], cwd=SUISHO_DIR,
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8"
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
        self.send(f"position startpos moves {' '.join(moves)}")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
        bestmove = None
        score = None
        while True:
            l = self.proc.stdout.readline().strip()
            if not l: break
            if l.startswith("info") and "score" in l:
                parts = l.split()
                if "cp" in parts:
                    score = parts[parts.index("cp")+1]
                elif "mate" in parts:
                    score = "mate " + parts[parts.index("mate")+1]
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

prefix = ["2g2f", "8c8d", "2f2e", "8d8e", "7g7f", "3c3d", "5i5h", "2b8h+"]
eng = Engine()
moves = list(prefix)
print("=== Simulating Game 2 from 2b8h+ ===")
try:
    for ply in range(len(prefix) + 1, 100):
        bm, sc = eng.go(moves, byoyomi_ms=500)
        print(f"Ply {ply:3d}: {bm:6s} ({sc})", flush=True)
        if bm == "resign":
            winner = "Sente" if ply % 2 == 0 else "Gote"
            print(f"RESIGN at ply {ply}! Winner is {winner}")
            break
        moves.append(bm)
finally:
    eng.close()

print("Complete Moves:", len(moves))
print(repr(moves))
