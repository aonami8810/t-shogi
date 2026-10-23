# -*- coding: utf-8 -*-
import subprocess
import sys

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
    # Game 2 実戦の 15手目 5i5h まで (先手Suisho5, 後手Morse)
    moves = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 5i5h".split()
    print(f"Rolling out Game 2 from ply 15 (5i5h) to Suisho5 resignation... Initial moves: {len(moves)}", flush=True)

    try:
        for ply in range(16, 200):
            is_sente = (ply % 2 == 1) # 先手Suisho5
            # 後手Morseは深く思考 (3000ms)、先手Suisho5は公式秒読み (500ms)
            byo = 500 if is_sente else 3000
            bm, sc, pv = session.search_best(moves, byoyomi_ms=byo)
            role = "Suisho (Sente)" if is_sente else "Morse (Gote)"
            print(f"Ply {ply:3d} [{role:14s}]: {bm:6s} ({sc:10s}) | {pv[:60]}", flush=True)
            if bm == "resign":
                winner = "Suisho" if not is_sente else "Morse"
                print(f"\n>>> TERMINATED! Winner: {winner} at ply {ply} <<<", flush=True)
                moves.append(bm)
                break
            moves.append(bm)
        
        print("\nFULL_G2_5I5H_15PLY_LINE =", repr(moves[:-1] if moves[-1] == "resign" else moves))
        with open("g2_5i5h_15ply_result.txt", "w", encoding="utf-8") as out:
            out.write(repr(moves[:-1] if moves[-1] == "resign" else moves))
    finally:
        session.close()
