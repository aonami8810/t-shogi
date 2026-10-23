import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class Engine:
    def __init__(self):
        self.p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
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

moves = ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'P*2b', 'R*8c', 'P*8b', '8c8e+', 'R*2g', '3i3h', '2g2d+', '7g3c+', '2a3c', 'B*4f', '2d2e', '8e2e', '3c2e', 'R*2a', 'B*3c', '8i7g', '2b2c', 'P*8c', '6a7b', 'P*2d', '2c2d', '3f3e', '3c2b', '3e3d', '7b8c', '4f2d', 'P*3g', '2i3g', '2e3g+', '3h3g', 'P*3f', '3g4h', 'R*2h', '2d1e', '1c1d', '3d3c+', '1d1e', '3c2b', '2h2b+', '2a2b+', '3a2b', '7g6e', 'R*2i', '6e5c+', '5b5c', 'R*5a', '5c6b', '5a4a+', '3b3a', 'N*5d', '6b5c', '4a7a', '5c5d', 'P*3i', 'B*7c', '7a8a', 'P*5f', '5g5f', 'N*6e', 'N*4f', '5d4d', '6g6f', 'N*4e', 'S*5d', '4d3e', '5d4e', '3e4e', '6f6e', 'N*6f', '5h6g', '6f7h+', '7i7h', 'P*5a', '8a9a', '7c8d', 'N*6f', '4e3e', 'L*8i', '8d6f', '6g6f', 'S*7e', '6f6g', '2i1i+', '9a5a', '3e2f', 'P*7f', 'G*6f', '6g6h', 'L*3c', 'B*5c', 'N*4d', '7f7e', '3f3g+', '4h5g', '1i2h', 'N*5h', '6f5g', '6h5g', 'P*3h', 'B*5d', '3c3f', 'P*2i', '2h2i', '5g6f', '3h3i+', '8i8c+', '3i4i', 'G*5b', '2i3h', '5d6c+', '3h5h', '6f5e', '5h7h', 'N*3d', 'N*4a', '5c6b+', '8b8c', '5b4a', '3a4a', '5a4a', 'G*3a', '4a6a', 'G*4a', 'S*5b']

if __name__ == "__main__":
    eng = Engine()
    try:
        print(f"Starting solve from ply {len(moves)}...", flush=True)
        for ply in range(len(moves) + 1, 230):
            if ply % 2 == 1:
                # Sente (Suisho5 byoyomi 500)
                bm, sc, pv = eng.get_move(moves, "go btime 0 wtime 0 byoyomi 500")
                print(f"Ply {ply:3d} [Suisho5]: {bm:6s} | {sc}", flush=True)
                if bm in ("resign", "win"):
                    print(f"SUISHO5 RESIGNED AT PLY {ply}!", flush=True)
                    break
                moves.append(bm)
            else:
                # Gote (Morse depth 18)
                bm, sc, pv = eng.get_move(moves, "go depth 18")
                print(f"Ply {ply:3d} [Morse  ]: {bm:6s} | {sc}", flush=True)
                moves.append(bm)
                if sc and "mate" in sc and "mate -" not in sc:
                    print(f"MATE IN SIGHT: {sc}! PV: {' '.join(pv)}", flush=True)
                    for m in pv:
                        moves.append(m)
                    print(f"FULL MATE REACHED AT PLY {len(moves)}!", flush=True)
                    # Check if Suisho5 now resigns
                    bm_final, _, _ = eng.get_move(moves, "go btime 0 wtime 0 byoyomi 500")
                    print(f"Final reply from Suisho5: {bm_final}", flush=True)
                    break
    finally:
        eng.close()
    print("ALL_G2_FINAL_MOVES = " + repr(moves), flush=True)
