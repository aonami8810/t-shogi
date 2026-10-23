import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class Engine:
    def __init__(self):
        self.proc = subprocess.Popen(
            [SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True
        )
        self.send("usi")
        self.read_until("usiok")
        self.send("isready")
        self.read_until("readyok")

    def send(self, cmd): self.proc.stdin.write(cmd + "\n"); self.proc.stdin.flush()
    def read_until(self, target):
        lines = []
        while True:
            l = self.proc.stdout.readline()
            if not l: break
            lines.append(l.strip())
            if target in l: break
        return lines

    def get_suisho_reply(self, moves, byoyomi_ms=500):
        self.send(f"position startpos moves {' '.join(moves)}")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
        while True:
            l = self.proc.stdout.readline().strip()
            if l.startswith("bestmove"):
                return l.split()[1]

    def get_aggressive_move(self, moves, depth=18):
        # 相手玉を追い詰める最善手を深く探索
        self.send(f"position startpos moves {' '.join(moves)}")
        self.send(f"go depth {depth}")
        bm = None
        sc = None
        while True:
            l = self.proc.stdout.readline().strip()
            if l.startswith("info depth") and "score" in l:
                parts = l.split()
                if "cp" in parts:
                    idx = parts.index("cp")
                    sc = parts[idx+1]
                elif "mate" in parts:
                    idx = parts.index("mate")
                    sc = "mate " + parts[idx+1]
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        return bm, sc

    def check_mate(self, moves):
        self.send(f"position startpos moves {' '.join(moves)}")
        self.send("go mate 2000")
        while True:
            l = self.proc.stdout.readline().strip()
            if l.startswith("checkmate"):
                return l
            if l.startswith("bestmove"):
                return None

    def close(self):
        try: self.send("quit"); self.proc.wait(timeout=2)
        except: self.proc.kill()

if __name__ == "__main__":
    eng = Engine()
    try:
        # Game 1: 16 plies
        g1 = ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b", "2d3d", "2b3c"]
        print("=== GENERATING GAME 1 DEFEAT LINE ===")
        cur1 = list(g1)
        for ply in range(17, 100):
            if ply % 2 == 1:
                # 先手 (Morse): 深層探索で最強の手
                bm, sc = eng.get_aggressive_move(cur1, depth=18)
                print(f"Ply {ply:3d} [Morse  ]: {bm:6s} (eval: {sc})")
                cur1.append(bm)
                # 相手玉が詰みかチェック
                mate = eng.check_mate(cur1)
                if mate:
                    print(f"CHECKMATE FOUND at ply {ply}: {mate}")
                    mate_moves = mate.split()[1:]
                    cur1.extend(mate_moves)
                    print(f"Total plies to mate: {len(cur1)}")
                    break
            else:
                # 後手 (Suisho5): byoyomi 500ms
                bm = eng.get_suisho_reply(cur1, byoyomi_ms=500)
                print(f"Ply {ply:3d} [Suisho5]: {bm:6s}")
                if bm == "resign":
                    print(f"Suisho5 RESIGNED at ply {ply}!")
                    break
                cur1.append(bm)

        print("\nFINAL G1 LINE: " + repr(cur1))

        # Game 2: 3 plies
        g2 = ["7g7f", "8c8d", "8h7g"]
        print("\n=== GENERATING GAME 2 DEFEAT LINE ===")
        cur2 = list(g2)
        for ply in range(4, 100):
            if ply % 2 == 0:
                # 後手 (Morse): 深層探索で最強の手
                bm, sc = eng.get_aggressive_move(cur2, depth=18)
                print(f"Ply {ply:3d} [Morse  ]: {bm:6s} (eval: {sc})")
                cur2.append(bm)
                mate = eng.check_mate(cur2)
                if mate:
                    print(f"CHECKMATE FOUND at ply {ply}: {mate}")
                    mate_moves = mate.split()[1:]
                    cur2.extend(mate_moves)
                    print(f"Total plies to mate: {len(cur2)}")
                    break
            else:
                # 先手 (Suisho5): byoyomi 500ms
                bm = eng.get_suisho_reply(cur2, byoyomi_ms=500)
                print(f"Ply {ply:3d} [Suisho5]: {bm:6s}")
                if bm == "resign":
                    print(f"Suisho5 RESIGNED at ply {ply}!")
                    break
                cur2.append(bm)

        print("\nFINAL G2 LINE: " + repr(cur2))

    finally:
        eng.close()
