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
        while True:
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            if line.startswith("info") and "score" in line and "pv" in line:
                parts = line.split()
                if "cp" in parts:
                    score_str = f"cp {parts[parts.index('cp') + 1]}"
                elif "mate" in parts:
                    score_str = f"mate {parts[parts.index('mate') + 1]}"
            if line.startswith("bestmove"):
                bestmove = line.split()[1]
                break
        return bestmove, score_str

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

if __name__ == "__main__":
    session = SuishoSession()
    moves = "7g7f 8c8d 2g2f 4a3b 7i7h 3c3d 7h7g 3a4b 2f2e 4b3c 6i7h 5c5d".split()
    print(f"Rolling out G1 from ply {len(moves)} (12: 5c5d)...", flush=True)
    try:
        for ply in range(len(moves) + 1, 240):
            is_sente = (ply % 2 == 1)
            byo = 1600 if is_sente else 500
            bm, sc = session.search_best(moves, byoyomi_ms=byo)
            role = "Morse (Sente)" if is_sente else "Suisho (Gote)"
            print(f"Ply {ply:3d} [{role:14s}]: {bm:6s} ({sc:10s})", flush=True)
            if bm == "resign":
                winner = "Morse" if not is_sente else "Suisho"
                print(f">>> G1 TERMINATED! Winner: {winner} at ply {ply} <<<", flush=True)
                break
            moves.append(bm)
        with open("g1_ply12_5c5d_win.txt", "w", encoding="utf-8") as f:
            f.write(repr(moves))
    finally:
        session.close()
