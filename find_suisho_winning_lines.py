import subprocess
import time

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

def simulate(initial_moves, max_plies=80, byoyomi_ms=500):
    engine = UsiEngine(SUISHO_EXE, SUISHO_DIR)
    moves = list(initial_moves)
    print(f"=== Starting simulation from: {' '.join(moves)} ===", flush=True)
    try:
        for ply in range(len(moves) + 1, max_plies + 1):
            mv, score, pv = engine.go(moves, byoyomi_ms=byoyomi_ms)
            turn = "Sente" if (ply % 2 == 1) else "Gote"
            print(f"Ply {ply:3d} [{turn:5s}]: {mv:6s} (eval: {score})", flush=True)
            if mv == "resign" or mv == "win":
                break
            moves.append(mv)
    finally:
        engine.close()
    return moves

if __name__ == "__main__":
    # 1. Game 1: 16手目 2b3c（3三銀）に対するシミュレーション（先手番）
    g1_init = [
        "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "6i7h",
        "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b",
        "2d3d", "2b3c"
    ]
    print("\n" + "="*80)
    print("SIMULATION 1: Suisho5 vs Suisho5 from Game 1 (16 plies, after 2b3c)")
    print("="*80)
    g1_moves = simulate(g1_init, max_plies=70, byoyomi_ms=500)
    print(f"G1 Final Moves: {' '.join(g1_moves)}")

    # 2. Game 2: 3手目 8h7g（7七角）に対するシミュレーション（後手番）
    g2_init = [
        "7g7f", "8c8d", "8h7g"
    ]
    print("\n" + "="*80)
    print("SIMULATION 2: Suisho5 vs Suisho5 from Game 2 (3 plies, after 8h7g)")
    print("="*80)
    g2_moves = simulate(g2_init, max_plies=70, byoyomi_ms=500)
    print(f"G2 Final Moves: {' '.join(g2_moves)}")
