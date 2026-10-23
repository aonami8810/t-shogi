import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class SuishoSelfPlay:
    def __init__(self, byoyomi_ms=500):
        self.byoyomi_ms = byoyomi_ms
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
        self.init()

    def send(self, cmd):
        self.proc.stdin.write(cmd + "\n")
        self.proc.stdin.flush()

    def read_until(self, target):
        lines = []
        while True:
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            lines.append(line)
            if target in line: break
        return lines

    def init(self):
        self.send("usi")
        self.read_until("usiok")
        self.send("isready")
        self.read_until("readyok")
        self.send("usinewgame")

    def get_bestmove(self, moves):
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send(f"go btime 0 wtime 0 byoyomi {self.byoyomi_ms}")
        
        bestmove = None
        score = None
        while True:
            line = self.proc.stdout.readline()
            if not line: break
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
        except:
            self.proc.kill()

def play_out(start_moves, max_plies=250):
    session = SuishoSelfPlay(500)
    moves = list(start_moves)
    print(f"Starting rollout from {len(moves)} plies: {' '.join(moves)}", flush=True)
    
    for ply in range(len(moves) + 1, max_plies + 1):
        bm, score = session.get_bestmove(moves)
        turn_str = "先手" if ply % 2 == 1 else "後手"
        print(f"Ply {ply:3d} ({turn_str}): {bm:6s} (eval: {score})", flush=True)
        if bm == "resign":
            print(f"Resign at ply {ply}! Winner is {'先手' if ply % 2 == 0 else '後手'}", flush=True)
            break
        moves.append(bm)
    
    session.close()
    return moves

if __name__ == "__main__":
    # Game 1: 17手目 5i5h からの自己対戦
    g1_prefix = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 5a4b 5i5h".split()
    print("=== Rolling out Game 1 (from 5a4b 5i5h) ===")
    res_g1 = play_out(g1_prefix, 250)
    print("G1_FINAL_MOVES =", res_g1)
