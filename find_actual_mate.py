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

    def get_suisho_byoyomi(self, moves, byoyomi_ms=500):
        self.send(f"position startpos moves {' '.join(moves)}")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
        bm = None
        sc = None
        while True:
            l = self.proc.stdout.readline().strip()
            if l.startswith("info") and "score" in l and "pv" in l:
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

    def get_attacking_move(self, moves, depth=18):
        self.send(f"position startpos moves {' '.join(moves)}")
        self.send(f"go depth {depth}")
        bm = None
        sc = None
        pv = []
        while True:
            l = self.proc.stdout.readline().strip()
            if l.startswith("info depth") and "score" in l and "pv" in l:
                parts = l.split()
                if "cp" in parts:
                    idx = parts.index("cp")
                    sc = parts[idx+1]
                elif "mate" in parts:
                    idx = parts.index("mate")
                    sc = "mate " + parts[idx+1]
                if "pv" in parts:
                    idx = parts.index("pv")
                    pv = parts[idx+1:]
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        return bm, sc, pv

    def close(self):
        try: self.send("quit"); self.proc.wait(timeout=2)
        except: self.proc.kill()

def play_to_mate(is_morse_sente, initial_moves, max_plies=160):
    eng = Engine()
    moves = list(initial_moves)
    print(f"\nSearching mate for {'Sente' if is_morse_sente else 'Gote'} from {len(moves)} plies...")
    try:
        while len(moves) < max_plies:
            ply = len(moves) + 1
            is_sente = (ply % 2 == 1)
            is_morse = (is_sente == is_morse_sente)

            if is_morse:
                bm, sc, pv = eng.get_attacking_move(moves, depth=18)
                print(f"Ply {ply:3d} [Morse  ]: {bm:6s} (eval: {sc})")
                moves.append(bm)
                if sc and "mate" in sc:
                    print(f"*** Morse found mate: {sc} ! PV: {' '.join(pv)}")
                    # 残りの詰み手順を一気に追加
                    for m in pv:
                        moves.append(m)
                    break
            else:
                bm, sc = eng.get_suisho_byoyomi(moves, byoyomi_ms=500)
                print(f"Ply {ply:3d} [Suisho5]: {bm:6s} (eval: {sc})")
                if bm == "resign":
                    print(f"*** Suisho5 RESIGNED at ply {ply}!")
                    break
                moves.append(bm)
                if sc and "mate" in sc and "-" in sc: # 相手視点で負け
                    print(f"*** Suisho5 sees being mated: {sc}")
    finally:
        eng.close()
    return moves

if __name__ == "__main__":
    from run_to_end import G1_120
    # G1: 120手目以降から
    print("=== Extending G1 to Mate ===")
    g1_result = play_to_mate(is_morse_sente=True, initial_moves=G1_120, max_plies=160)
    print("G1 RESULT MOVES: " + repr(g1_result))
