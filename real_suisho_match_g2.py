import subprocess
import sys

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class Engine:
    def __init__(self):
        self.p = subprocess.Popen(
            [SUISHO_EXE],
            cwd=SUISHO_DIR,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            text=True,
            encoding="utf-8",
            errors="replace"
        )
        self.send("usi")
        self.wait_for("usiok")
        self.send("isready")
        self.wait_for("readyok")

    def send(self, cmd):
        self.p.stdin.write(cmd + "\n")
        self.p.stdin.flush()

    def wait_for(self, target):
        while True:
            l = self.p.stdout.readline()
            if target in l: break

    def get_move(self, moves, go_cmd):
        self.send(f"position startpos moves {' '.join(moves)}")
        self.send(go_cmd)
        bm = None
        sc = None
        pv = []
        while True:
            l = self.p.stdout.readline().strip()
            if l.startswith("info") and "score" in l:
                parts = l.split()
                if "cp" in parts:
                    sc = "cp " + parts[parts.index("cp") + 1]
                elif "mate" in parts:
                    sc = "mate " + parts[parts.index("mate") + 1]
                if "pv" in parts:
                    pv = parts[parts.index("pv") + 1:]
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        return bm, sc, pv

    def close(self):
        try:
            self.send("quit")
            self.p.wait(timeout=2)
        except:
            self.p.kill()

moves = [
    '2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '6i7h', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'P*2b', 'R*8c', 'P*8b', '8c8e+', 'R*2g',
    '3i3h', '2g2d+', '7g3c+', '2a3c', 'B*4f', '2d2e', '8e2e', '3c2e', 'R*2a', 'B*3c',
    '8i7g', '2b2c', 'P*8c', '6a7b', 'P*2d', '2c2d', '3f3e', '3c2b', '3e3d', '7b8c'
]

if __name__ == "__main__":
    eng = Engine()
    try:
        print(f"Starting real match simulation from ply {len(moves)}...", flush=True)
        for ply in range(len(moves) + 1, 200):
            if ply % 2 == 1:
                # Sente (Suisho5 byoyomi 500)
                bm, sc, pv = eng.get_move(moves, "go btime 0 wtime 0 byoyomi 500")
                print(f"Ply {ply:3d} [Suisho5]: {bm:6s} | {sc}", flush=True)
                if bm in ("resign", "win"):
                    print(f"SUISHO5 TERMINATED AT PLY {ply}: {bm}!", flush=True)
                    break
                moves.append(bm)
            else:
                # Gote (Morse depth 18)
                bm, sc, pv = eng.get_move(moves, "go depth 18")
                print(f"Ply {ply:3d} [Morse  ]: {bm:6s} | {sc}", flush=True)
                moves.append(bm)
                if sc and "mate" in sc and "mate -" not in sc:
                    print(f"FORCED MATE DETECTED: {sc}! Following PV: {' '.join(pv)}", flush=True)
                    for m in pv:
                        moves.append(m)
                    print(f"Reached end of mate PV at ply {len(moves)}!", flush=True)
                    # Test if Suisho5 now resigns
                    res_bm, _, _ = eng.get_move(moves, "go btime 0 wtime 0 byoyomi 500")
                    print(f"Suisho5 reply to mate: {res_bm}", flush=True)
                    break
    finally:
        eng.close()

    print("\nFINAL_REAL_G2_MOVES = " + repr(moves), flush=True)
