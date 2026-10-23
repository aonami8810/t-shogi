import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class SuishoSession:
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

    def query(self, moves):
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send(f"go btime 0 wtime 0 byoyomi {self.byoyomi_ms}")
        
        bestmove = None
        score = None
        pv = ""
        while True:
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            if line.startswith("info") and "score" in line and "pv" in line:
                pv = line
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
        return bestmove, score, pv

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except:
            self.proc.kill()

def play_to_end(name, start_moves, max_plies=250):
    session = SuishoSession(500)
    moves = list(start_moves)
    print(f"\n==========================================", flush=True)
    print(f"  Rolling out {name} from {len(moves)} plies", flush=True)
    print(f"==========================================", flush=True)
    
    for ply in range(len(moves) + 1, max_plies + 1):
        bm, score, pv = session.query(moves)
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
    # 1. Game 1: 27手目 R*2h 28手目 P*2f 29手目 4i3h
    g1_prefix = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d P*2d R*2g R*2h P*2f 4i3h".split()
    res_g1 = play_to_end("Game 1 (Morse 先手)", g1_prefix, 250)
    print("G1_DECISIVE =", res_g1)

    # 2. Game 2: 7手目 5i5h 8手目 8e8f
    g2_prefix = "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 5i5h 8e8f".split()
    res_g2 = play_to_end("Game 2 (Morse 後手)", g2_prefix, 250)
    print("G2_DECISIVE =", res_g2)
