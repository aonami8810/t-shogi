import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class Engine:
    def __init__(self):
        self.proc = subprocess.Popen(
            [SUISHO_EXE], cwd=SUISHO_DIR,
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            text=True, encoding="utf-8"
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
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
        bestmove = None
        score = None
        while True:
            l = self.proc.stdout.readline()
            if not l: break
            l = l.strip()
            if l.startswith("info") and "score" in l:
                parts = l.split()
                if "cp" in parts:
                    idx = parts.index("cp")
                    score = parts[idx+1]
                elif "mate" in parts:
                    idx = parts.index("mate")
                    score = "mate " + parts[idx+1]
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

def play_until_end(prefix, max_plies=160):
    eng = Engine()
    moves = list(prefix)
    print(f"=== Playing until end from prefix ({len(moves)} moves) ===")
    print("Prefix:", " ".join(prefix))
    winner = None
    try:
        for ply in range(len(prefix) + 1, max_plies + 1):
            bm, score = eng.go(moves, byoyomi_ms=500)
            score_str = f"({score})" if score else ""
            print(f"Ply {ply:3d}: {bm:6s} {score_str}", flush=True)
            if bm == "resign":
                winner = "Sente" if ply % 2 == 0 else "Gote"
                print(f"RESIGN at ply {ply}! Winner is {winner}")
                break
            if bm == "win":
                winner = "Sente" if ply % 2 == 1 else "Gote"
                print(f"WIN declared at ply {ply}! Winner is {winner}")
                break
            moves.append(bm)
    finally:
        eng.close()
    return winner, moves

print("\n--- TEST GAME 1 ---")
# 7手目 2f2e から水匠5同士の対局を最後まで実行
w1, m1 = play_until_end(["7g7f", "8c8d", "2g2f", "4a3b", "6i7h", "8d8e", "2f2e"], max_plies=160)
print(f"Result 1: Winner = {w1}, Plies = {len(m1)}")
print(repr(m1))
