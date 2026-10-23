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
    # 2g2f 3c3d 2f2e 2b3c 7g7f 3a2b 8h3c+
    moves = "2g2f 3c3d 2f2e 2b3c 7g7f 3a2b 8h3c+".split()
    print(f"Starting rollout from ply 7 ({len(moves)} moves)...", flush=True)

    try:
        for ply in range(8, 250):
            is_gote_turn = (ply % 2 == 0) # Gote = Morse
            byo = 4000 if is_gote_turn else 500
            bm, sc, pv = session.search_best(moves, byoyomi_ms=byo)
            role = "Morse (Gote)" if is_gote_turn else "Suisho (Sente)"
            print(f"Ply {ply:3d} [{role:14s}]: {bm:6s} ({sc:10s}) | {pv[:60]}", flush=True)
            if bm == "resign":
                winner = "Morse" if not is_gote_turn else "Suisho"
                print(f"\n>>> TERMINATED! Winner: {winner} at ply {ply} <<<", flush=True)
                moves.append(bm)
                break
            moves.append(bm)

        final_line = moves[:-1] if moves[-1] == "resign" else moves
        with open("g2_2f2e_8h3c_full.txt", "w", encoding="utf-8") as out:
            out.write(repr(final_line))
        print(f"Wrote g2_2f2e_8h3c_full.txt, total plies: {len(final_line)}", flush=True)
    finally:
        session.close()
