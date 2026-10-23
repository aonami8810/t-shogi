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

def rollout_g1():
    session = SuishoSession()
    moves = "7g7f 8c8d 2g2f 3c3d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d P*2d R*2f 3i3h P*2h 7g3c+ 2a3c B*3g 2f2d 3g9a+ 2h2i+ 9a8a 2i2h N*7c 6a7b 7c6a+ 3a4b 3h3g 2h3h 4i3h 2d2i+ R*3i 2i2a P*7c 2a6a 7c7b+ 7a7b 8a8b B*7c P*8c P*8a 8b7c 7b7c 3i2i N*4e 3g4f P*2f B*9e 6a7b".split()
    print(f"Rolling out G1 from ply {len(moves)}...", flush=True)
    try:
        for ply in range(len(moves) + 1, 240):
            is_sente = (ply % 2 == 1)
            byo = 1500 if is_sente else 500
            bm, sc, pv = session.search_best(moves, byoyomi_ms=byo)
            role = "Morse (Sente)" if is_sente else "Suisho (Gote)"
            print(f"Ply {ply:3d} [{role:14s}]: {bm:6s} ({sc:10s})", flush=True)
            if bm == "resign":
                winner = "Morse" if not is_sente else "Suisho"
                print(f">>> G1 TERMINATED! Winner: {winner} at ply {ply} <<<", flush=True)
                break
            moves.append(bm)
        return moves
    finally:
        session.close()

def rollout_g2():
    session = SuishoSession()
    moves = "2g2f 3c3d 7g7f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8i7g 7c7d 2i3g 7a6b P*8c 3b2c 3d3e 2c2d 3e8e 8a7c 8e8d 3c5e 3i4h 7f3f P*3h P*8g 7h8g 5e6d 9g9f".split()
    print(f"Rolling out G2 from ply {len(moves)}...", flush=True)
    try:
        for ply in range(len(moves) + 1, 240):
            is_sente = (ply % 2 == 1)
            byo = 500 if is_sente else 1500
            bm, sc, pv = session.search_best(moves, byoyomi_ms=byo)
            role = "Suisho (Sente)" if is_sente else "Morse (Gote)"
            print(f"Ply {ply:3d} [{role:14s}]: {bm:6s} ({sc:10s})", flush=True)
            if bm == "resign":
                winner = "Morse" if is_sente else "Suisho"
                print(f">>> G2 TERMINATED! Winner: {winner} at ply {ply} <<<", flush=True)
                break
            moves.append(bm)
        return moves
    finally:
        session.close()

if __name__ == "__main__":
    g1_moves = rollout_g1()
    with open("g1_ply62_6a7b_win.txt", "w", encoding="utf-8") as f:
        f.write(repr(g1_moves))
    g2_moves = rollout_g2()
    with open("g2_ply39_9g9f_win.txt", "w", encoding="utf-8") as f:
        f.write(repr(g2_moves))
