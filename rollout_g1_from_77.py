# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class SuishoSession:
    def __init__(self):
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
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            lines.append(line)
            if target in line: break
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
    # 77手目までの手順
    g1_77 = [
        '7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
        '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
        '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'R*2g', '7g3c+', '3b3c', '3i3h', '2g2e+',
        'B*4f', 'B*8b', '2i3g', '2e2h', '4f8b+', '7a8b', '2d2c+', '2h2c', 'P*8c', '8b8c',
        'B*4f', 'B*6d', '4f6d', '6c6d', 'B*8b', '2c2h', '8b6d+', 'B*7c', '6d7c', '8a7c',
        'B*6d', '8c7b', '3g4e', 'P*3g', '4e5c+', '5b5a', '6d3g', '2h2e', '5c6c', 'P*5f',
        '5g5f', '7b6c', '3g7c+', '6a6b', 'R*8a', 'P*6a', '7c6b', '5a6b', '8a9a+', '6b5a',
        'L*6f', 'P*5g', '5h6h', '6c5b', '9a8b', 'B*9e', 'P*7g'
    ]
    moves = list(g1_77)
    print(f"Rolling out Game 1 from ply 77 (Score is +1003)...", flush=True)

    try:
        for ply in range(78, 160):
            is_morse = (ply % 2 == 1) # 先手Morse
            byo = 1500 if is_morse else 500
            bm, sc, pv = session.search_best(moves, byoyomi_ms=byo)
            role = "Morse (Sente)" if is_morse else "Suisho (Gote)"
            print(f"Ply {ply:3d} [{role:14s}]: {bm:6s} ({sc:10s}) | {pv[:70]}", flush=True)
            if bm == "resign":
                winner = "Morse" if not is_morse else "Suisho"
                print(f"\n>>> TERMINATED! Winner: {winner} at ply {ply} <<<", flush=True)
                moves.append(bm)
                break
            moves.append(bm)
        print("\nFULL_G1_LINE =", repr(moves))
    finally:
        session.close()
