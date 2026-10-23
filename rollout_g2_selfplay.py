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

def rollout_gote(candidate_16, max_plies=250):
    prefix = "2g2f 8c8d 7g7f 4a3b 6i7h 8d8e 8h7g 3c3d 7i6h 2b7g+ 6h7g 3a2b 3i3h 2b3c 4g4f".split()
    moves = list(prefix) + [candidate_16]
    session = SuishoSelfPlay(500)
    print(f"\n=== Testing Gote rollout with 16. {candidate_16} ===")
    
    for ply in range(len(moves) + 1, max_plies + 1):
        bm, score = session.get_bestmove(moves)
        turn_str = "先手" if ply % 2 == 1 else "後手"
        print(f"Ply {ply:3d} ({turn_str}): {bm:6s} (eval: {score})", flush=True)
        if bm == "resign":
            winner = "後手" if ply % 2 == 1 else "先手"
            print(f"Resign at ply {ply}! Winner = {winner}", flush=True)
            moves.append(bm)
            break
        moves.append(bm)
    session.close()
    return moves

if __name__ == "__main__":
    # 候補 16. 7a6b からロールアウト
    res = rollout_gote("7a6b", 250)
    print("GOTE_7A6B_MOVES =", res)
