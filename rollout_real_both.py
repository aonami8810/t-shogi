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

def rollout(name, start_moves, max_plies=250):
    session = SuishoSelfPlay(500)
    moves = list(start_moves)
    print(f"\n=== Rolling out {name} from {len(moves)} plies ===", flush=True)
    for ply in range(len(moves) + 1, max_plies + 1):
        bm, score = session.get_bestmove(moves)
        turn_str = "先手" if ply % 2 == 1 else "後手"
        print(f"Ply {ply:3d} ({turn_str}): {bm:6s} (eval: {score})", flush=True)
        if bm == "resign":
            winner = "先手" if ply % 2 == 0 else "後手"
            print(f"Resign at ply {ply}! Winner = {winner}", flush=True)
            moves.append(bm)
            break
        moves.append(bm)
    session.close()
    return moves

if __name__ == "__main__":
    # Game 1: 7g7f 4a3b 2g2f 3c3d 2f2e 8c8d 6i7h
    g1_moves = rollout("Game 1 (Sente Win)", ["7g7f", "4a3b", "2g2f", "3c3d", "2f2e", "8c8d", "6i7h"], 250)
    print("G1_FINAL =", g1_moves)
    
    # Game 2: 7g7f 8c8d 2g2f 8d8e 8h7g 3c3d
    g2_moves = rollout("Game 2 (Gote Win)", ["7g7f", "8c8d", "2g2f", "8d8e", "8h7g", "3c3d"], 250)
    print("G2_FINAL =", g2_moves)
