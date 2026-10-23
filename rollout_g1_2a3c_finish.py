# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class SuishoSession:
    def __init__(self):
        self.proc = subprocess.Popen(
            [SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            text=True, encoding="utf-8", errors="replace"
        )
        self.send("usi")
        self.read_until("usiok")
        self.send("isready")
        self.read_until("readyok")
        self.send("usinewgame")

    def send(self, cmd):
        self.proc.stdin.write(cmd + "\n")
        self.proc.stdin.flush()

    def read_until(self, target):
        lines = []
        while True:
            l = self.proc.stdout.readline()
            if not l: break
            l = l.strip()
            lines.append(l)
            if target in l: break
        return lines

    def search_best(self, moves, byoyomi_ms=1000):
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")

        bestmove = None
        score_str = ""
        pv_str = ""
        while True:
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            if line.startswith("info") and "score" in line and "pv" in line:
                pv_str = line
                parts = line.split()
                if "cp" in parts:
                    score_str = f"cp {parts[parts.index('cp') + 1]}"
                elif "mate" in parts:
                    score_str = f"mate {parts[parts.index('mate') + 1]}"
            if line.startswith("bestmove"):
                bestmove = line.split()[1]
                break
        return bestmove, score_str, pv_str

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

if __name__ == "__main__":
    session = SuishoSession()
    # 81手目 2a3a+ まで
    g1_81 = [
        '7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
        '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2a3c', 'P*8g', '8f8e', '3d2d', '1c1d',
        '5i5h', '7a7b', '8h6f', '8e6e', '2d2h', 'P*2e', '7i6h', '6e6d', '8i7g', '6d7d',
        '3g3f', '5a6b', '3i4h', '7d7f', '4i3h', '3a4b', '2i3g', 'P*3e', '3h2g', '6c6d',
        '5g5f', '3b2c', '3f3e', '2c2d', '2g3f', '2b1c', '5h6i', '6b7a', '4h5i', '2d3e',
        '3f3e', '1c3e', 'P*3f', '3e2d', 'P*7d', '7f7d', 'G*7e', '2d1e', '3g2e', '7d7e',
        '6f7e', '3c4e', '7e6d', '1e3g+', '6d3g', '4e3g+', '2e3c+', '3g2h', '3c4b', 'R*3i',
        'S*5b', 'G*4h', 'R*2a', 'B*5h', '6i7i', '4h5i', '7i8h', '5h6i+', '7h7i', 'P*3a',
        '2a3a+'
    ]
    moves = list(g1_81)
    print(f"Rolling out Game 1 branch 2a3c from ply 81 (Score is +775)...", flush=True)

    try:
        for ply in range(82, 160):
            is_sente = (ply % 2 == 1)
            byo = 1500 if is_sente else 500
            bm, sc, pv = session.search_best(moves, byoyomi_ms=byo)
            role = "Morse (Sente)" if is_sente else "Suisho (Gote)"
            print(f"Ply {ply:3d} [{role:14s}]: {bm:6s} ({sc:10s}) | {pv[:70]}", flush=True)
            if bm == "resign":
                winner = "Morse" if not is_sente else "Suisho"
                print(f"\n>>> TERMINATED! Winner: {winner} at ply {ply} <<<", flush=True)
                moves.append(bm)
                break
            moves.append(bm)
        print("\nG1_2A3C_FULL_LINE =", repr(moves))
    finally:
        session.close()
