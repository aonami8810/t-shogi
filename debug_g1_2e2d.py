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

# Game 1: 9手目 2e2d からシミュレーション
moves = ["7g7f", "8c8d", "2g2f", "3c3d", "2f2e", "8d8e", "6i7h", "4a3b", "2e2d"]
eng = Engine()
try:
    for ply in range(10, 40):
        bm, sc = eng.go(moves)
        print(f"Ply {ply:3d}: {bm:6s} ({sc})", flush=True)
        if bm in ["resign", "win"]: break
        moves.append(bm)
finally:
    eng.close()

print("Complete Moves:", len(moves))
print(repr(moves))
