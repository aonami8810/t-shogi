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
    base_moves = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 5i5h".split()
    
    candidates = ["2b8h+", "8f7f", "5a4b", "1c1d", "3a2b"]
    
    for c in candidates:
        print(f"\n=== Testing Candidate for Ply 16: {c} ===")
        moves = list(base_moves) + [c]
        # 水匠5の17手目 (500ms) を調べる
        bm17, sc17, pv17 = session.search_best(moves, byoyomi_ms=500)
        print(f"Suisho5 ply 17: {bm17} ({sc17}) | {pv17[:60]}")
        
        # 18手目 (Morse)
        moves.append(bm17)
        bm18, sc18, pv18 = session.search_best(moves, byoyomi_ms=2500)
        print(f"Morse   ply 18: {bm18} ({sc18}) | {pv18[:60]}")
        
        # 19手目 (Suisho5)
        moves.append(bm18)
        bm19, sc19, pv19 = session.search_best(moves, byoyomi_ms=500)
        print(f"Suisho5 ply 19: {bm19} ({sc19}) | {pv19[:60]}")

    session.close()
