# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class UsiEngine:
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

    def go(self, moves, is_morse=True):
        self.send(f"position startpos moves {' '.join(moves)}")
        if is_morse:
            self.send("go nodes 5000000")
        else:
            self.send("go btime 0 wtime 0 byoyomi 500")
        
        bestmove = None
        score = None
        while True:
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            if line.startswith("info") and "score" in line:
                parts = line.split()
                if "score" in parts:
                    idx = parts.index("score")
                    if idx + 2 < len(parts) and parts[idx + 1] == "cp":
                        score = int(parts[idx + 2])
                    elif idx + 2 < len(parts) and parts[idx + 1] == "mate":
                        score = "mate " + parts[idx + 2]
            if line.startswith("bestmove"):
                bestmove = line.split()[1]
                break
        return bestmove, score

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

def solve_line(moves, morse_is_sente, label, out_file):
    print(f"Solving {label} from ply {len(moves)}...", flush=True)
    engine = UsiEngine()
    try:
        for ply in range(len(moves) + 1, 350):
            is_sente = (ply % 2 == 1)
            is_morse = (is_sente == morse_is_sente)
            turn_str = "Morse" if is_morse else "Suisho"
            mv, sc = engine.go(moves, is_morse=is_morse)
            print(f"{label} Ply {ply:3d} | {turn_str:8s}: {mv:6s} (eval: {sc})", flush=True)
            if mv == "resign":
                winner = "Morse" if not is_morse else "Suisho"
                print(f"\n>>> {label} Complete! Resignation at Ply {ply}! Winner: {winner} <<<\n", flush=True)
                break
            moves.append(mv)
        with open(out_file, "w", encoding="utf-8") as f:
            f.write(repr(moves))
    finally:
        engine.close()

if __name__ == "__main__":
    g1_moves = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d P*2d R*2g 7g3c+ 3b3c 3i3h 2g2e+ B*4f B*6d 2i3g 2e2h 4f6d 6c6d B*4f P*2g".split()
    solve_line(g1_moves, True, "G1_38_P2g", "g1_ply38_p2g_nodes5m_win.txt")
    g2_moves = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 5i5h 8e8f 8g8f 5a5b 2e2d 2c2d 6i7h 8b8f 8h2b+".split()
    solve_line(g2_moves, False, "G2_15_8h2b", "g2_ply15_8h2b_5a5b_nodes5m_win.txt")
