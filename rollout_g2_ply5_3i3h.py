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
    # Game 2: 5手目 3i3h まで
    moves = ['2g2f', '8c8d', '2f2e', '4a3b', '3i3h']
    print(f"Rolling out Game 2 branch (5: 3i3h) from ply 5 (total {len(moves)} moves)...", flush=True)

    try:
        start_ply = len(moves) + 1
        for ply in range(start_ply, 220):
            is_sente = (ply % 2 == 1) # 先手Suisho5 (500ms)
            byo = 500 if is_sente else 1500 # 後手Morse (1500ms)
            bm, sc, pv = session.search_best(moves, byoyomi_ms=byo)
            role = "Suisho (Sente)" if is_sente else "Morse (Gote)"
            print(f"Ply {ply:3d} [{role:14s}]: {bm:6s} ({sc:10s}) | {pv[:70]}", flush=True)
            if bm == "resign":
                winner = "Morse" if is_sente else "Suisho"
                print(f"\n>>> TERMINATED! Winner: {winner} at ply {ply} <<<", flush=True)
                break
            moves.append(bm)
        
        with open("g2_ply5_3i3h_win.txt", "w", encoding="utf-8") as f:
            f.write(repr(moves))
        print(f"\nSaved g2_ply5_3i3h_win.txt with {len(moves)} moves!")
    finally:
        session.close()
