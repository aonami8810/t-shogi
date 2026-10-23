import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class Engine:
    def __init__(self):
        self.proc = subprocess.Popen(
            [SUISHO_EXE],
            cwd=SUISHO_DIR,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        self.send("usi")
        self.wait_for("usiok")
        self.send("isready")
        self.wait_for("readyok")

    def send(self, s):
        self.proc.stdin.write(s + "\n")
        self.proc.stdin.flush()

    def wait_for(self, target):
        while True:
            line = self.proc.stdout.readline()
            if not line: break
            if target in line: break

    def go(self, moves, byoyomi=500):
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi}")
        bm = None
        score = None
        while True:
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            if line.startswith("info") and "score" in line:
                parts = line.split()
                if "cp" in parts:
                    score = "cp " + parts[parts.index("cp")+1]
                elif "mate" in parts:
                    score = "mate " + parts[parts.index("mate")+1]
            if line.startswith("bestmove"):
                bm = line.split()[1]
                break
        return bm, score

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except:
            self.proc.kill()

def play_out(initial_moves, max_moves=110):
    eng = Engine()
    moves = list(initial_moves)
    print(f"=== Playing out from: {' '.join(initial_moves)} ===")
    for i in range(len(initial_moves) + 1, max_moves + 1):
        bm, score = eng.go(moves, byoyomi=500)
        ply = len(moves) + 1
        turn = "先手" if ply % 2 == 1 else "後手"
        print(f"Ply {ply:3d} ({turn}): {bm:6s} (eval: {score})")
        if bm in ["resign", "win"]:
            print(f"Game Over at Ply {ply}: {turn} {bm}")
            moves.append(bm)
            break
        moves.append(bm)
    eng.close()
    print("Moves list:")
    print(" ".join(moves))
    return moves

if __name__ == "__main__":
    import sys
    test_id = sys.argv[1] if len(sys.argv) > 1 else "1"
    if test_id == "gote_6g6f":
        # Game 2: 先手水匠5が 6g6f と指した後の展開
        play_out(["2g2f", "8c8d", "7g7f", "4a3b", "2f2e", "8d8e", "8h7g", "3c3d", "6g6f", "2b3c"])
    elif test_id == "sente_2g2f":
        # Game 1: Morse 初手 2g2f
        play_out(["2g2f"])
    elif test_id == "sente_7g7f_2g2f":
        # Game 1: Morse 初手 7g7f 3c3d に対し 2g2f
        play_out(["7g7f", "3c3d", "2g2f"])
