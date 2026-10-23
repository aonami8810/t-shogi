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

def rollout(moves, morse_is_sente, label, out_file):
    session = SuishoSession()
    print(f"Rolling out {label} from ply {len(moves)}...", flush=True)
    try:
        for ply in range(len(moves) + 1, 240):
            is_sente = (ply % 2 == 1)
            is_morse = (is_sente == morse_is_sente)
            byo = 1600 if is_morse else 500
            bm, sc = session.search_best(moves, byoyomi_ms=byo)
            role = "Morse" if is_morse else "Suisho"
            print(f"Ply {ply:3d} [{role:6s}]: {bm:6s} ({sc:10s})", flush=True)
            if bm == "resign":
                winner = "Morse" if not is_morse else "Suisho"
                print(f">>> {label} TERMINATED! Winner: {winner} at ply {ply} <<<", flush=True)
                break
            moves.append(bm)
        with open(out_file, "w", encoding="utf-8") as f:
            f.write(repr(moves))
    finally:
        session.close()

if __name__ == "__main__":
    g1_moves = "7g7f 8c8d 2g2f 4a3b 7i7h 3c3d 7h7g 3a4b 2f2e 4b3c 6i7h 5c5d 3i4h 7a6b 3g3f 6a5b".split()
    rollout(g1_moves, True, "G1 (16: 6a5b)", "g1_ply16_6a5b_win.txt")

    g2_moves = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 5a4b 5i5h 7a7b 3d2d 7c7d 2d2f 8f8e 8i7g 8e8b".split()
    rollout(g2_moves, False, "G2 (23: 8i7g -> 24: 8e8b)", "g2_ply23_8i7g_win.txt")
