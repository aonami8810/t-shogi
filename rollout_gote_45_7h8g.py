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
    moves = [
        '2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
        '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
        '8i7g', '3c5e', 'P*2b', '2a3c', '2b2a+', '3a4b', '3d8d', '5e1i+', '8d8a+', '3c4e',
        '8a8e', 'L*5e', '8e7f', '4e5g+', '5h6i', '1i2i', '7h8g', '2i4g', '6i7h', '4g5f',
        'N*5i', 'N*9e', 'R*8a', '9e8g+', '7h8g'
    ]
    try:
        for ply in range(len(moves) + 1, 220):
            is_sente = (ply % 2 == 1)
            byo = 500 if is_sente else 1500
            bm, sc = session.search_best(moves, byoyomi_ms=byo)
            role = "Suisho (Sente)" if is_sente else "Morse (Gote)"
            print(f"Ply {ply:3d} [{role:14s}]: {bm:6s} ({sc:10s})", flush=True)
            if bm == "resign":
                winner = "Morse" if is_sente else "Suisho"
                print(f">>> TERMINATED! Winner: {winner} at ply {ply} <<<", flush=True)
                break
            moves.append(bm)
        with open("g2_ply45_7h8g_win.txt", "w", encoding="utf-8") as f:
            f.write(repr(moves))
    finally:
        session.close()
