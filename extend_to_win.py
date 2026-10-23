import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class UsiEngine:
    def __init__(self, exe_path, cwd):
        self.proc = subprocess.Popen(
            [exe_path],
            cwd=cwd,
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
        self.send("usinewgame")

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

    def go(self, moves, byoyomi_ms=500):
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
        
        bestmove = None
        score = None
        pv = None
        
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

def simulate_to_finish(initial_moves, max_plies=140, byoyomi_ms=500):
    engine = UsiEngine(SUISHO_EXE, SUISHO_DIR)
    moves = list(initial_moves)
    print(f"=== Continuing simulation from ply {len(moves)} ===", flush=True)
    try:
        for ply in range(len(moves) + 1, max_plies + 1):
            mv, score, pv = engine.go(moves, byoyomi_ms=byoyomi_ms)
            turn = "Sente" if (ply % 2 == 1) else "Gote"
            print(f"Ply {ply:3d} [{turn:5s}]: {mv:6s} (eval: {score})", flush=True)
            if mv == "resign" or mv == "win":
                print(f"FINISHED at ply {ply} with {mv}!", flush=True)
                break
            moves.append(mv)
    finally:
        engine.close()
    return moves

if __name__ == "__main__":
    g1_70 = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d P*2d P*2b R*8c P*8b 8c8e+ 3c5a 2d2c+ 3b2c 8e3e P*3d 3e2f 5a7c P*2h 2c3c 3f3e 4c4d 3e3d 3c4c 7i6h 7a6b 2f7f 5c5d 7h7i 3a4b 7f2f 4b3a 1g1f 6b5c 1f1e 7c6d 3i4h 7d7e 9g9f 5d5e 7g8f R*2c 2f3g 5e5f 4g4f 4d4e 4i3i 4e4f 5g5f 3a4b 8f7g 5c4d".split()
    print("\n--- Extending Game 1 to win ---")
    g1_all = simulate_to_finish(g1_70, max_plies=120)
    print("G1_ALL = " + repr(g1_all))

    g2_70 = "7g7f 8c8d 8h7g 4a3b 7i7h 3c3d 3i4h 2b7g+ 7h7g 3a2b 6i7h 7a6b 2g2f 1c1d 1g1f 9c9d 9g9f 2b3c 4g4f 6c6d 3g3f 7c7d 4h4g 6b6c 5i6h 8a7c 2i3g 8b8a 2h2i 6a6b 4g5f 6c5d 4i4h 5a5b 7g6f 5d6c 2f2e 5b4b 6f7g 6c5d 4f4e 4b5b B*4f 5d6c 7g6f 5c5d 7f7e 8d8e 6h5h 6b5c 8i7g 8e8f 8g8f 8a8f 7e7d 6c7d P*8g 8f8a P*7f P*8f 8g8f 8a8f 5h4g 7d6c 4g5h 3c4b 6f7e 8f8a 2e2d 2c2d".split()
    print("\n--- Extending Game 2 to win ---")
    g2_all = simulate_to_finish(g2_70, max_plies=120)
    print("G2_ALL = " + repr(g2_all))
