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

moves = ['7g7f', '8c8d', '2g2f', '4a3b', '6i7h', '8d8e', '2f2e', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', 'P*2c', '2d2f', '8f8b', 'P*8g', '7a7b', '5i6h', '6c6d', '9g9f', '1c1d', '3i3h', '7b6c', '1g1f', '3c3d', '4g4f', '9c9d', '3g3f', '7c7d', '2i3g', '6a6b', '2f2i', '8a7c', '4i4h', '5a5b', '3h4g', '8b8a', '6h5h', '4c4d', '7i6h', '3a4b', '4f4e', '4d4e', '8h2b+', '3b2b', 'B*6f', '4e4f', '4g4f', '4b3c', '3g4e', '2b3b', '4e3c+', '2a3c', '4f5e', 'P*4g', '5h4g', '6d6e', '6f8h', '6c5d', 'S*6d', '5d5e', '8h5e', 'S*5d', '5e8h', '1d1e', '1f1e', 'P*4f', '4g5h', '6e6f', '8h6f', 'B*4g', '4h4g', '4f4g+', '5h4g', 'G*6e', 'S*4d', 'N*4f', '4g4f', '6e6d', '4f4g', 'S*5e', '4g5h', '3c4e', '4d5e', '5d5e', 'S*5f', 'S*4f', 'P*4g', '4f5g+', '6h5g', '4e5g+', '5h5g', '5e5f', '5g5f', 'S*6e', '5f5g', 'S*5f', '5g4h', '6e6f', 'N*4d', '5b4c', '6g6f', 'B*5g', '4h3h', 'P*4f', '4g4f', '5g4f+', '2i4i', 'P*4h', '4i4h', '4f1i', '4h4i', '1i5e', '4d3b+', 'L*4d', 'G*4b', '4c5d', 'P*4h', '5d6c', '3h2g', '5f5g', 'P*5f', '5e5f', 'S*6g', '5f5e', 'S*5f', '4d4h+', '4i2i', '5g4f+', '5f5e', '6d5e', 'B*3c', '5e4e', 'N*5e', '6c6d', '2g1f', 'S*2d', '3c2d+', '2c2d', '2i2d', 'B*3g', '1f2e', '3g1e+', '2e3d', 'P*3a', '3b2b', '1e2d', '3d2d', 'R*1g', 'P*1b', '1a1b', '2d3c', '6d5e', '2b3a', '1g2g+', 'B*2c', '4f5g', 'S*3d', '4e4d']

eng = Engine()
try:
    for ply in range(161, 210):
        bm, sc = eng.go(moves, byoyomi_ms=500)
        print(f"Ply {ply:3d}: {bm:6s} ({sc})", flush=True)
        if bm == "resign":
            print("GOTE RESIGNED at ply", ply)
            break
        moves.append(bm)
finally:
    eng.close()

print("Final moves count:", len(moves))
print(repr(moves))
