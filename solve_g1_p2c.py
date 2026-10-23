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
        pv = None
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
                if "pv" in parts:
                    idx = parts.index("pv")
                    pv = parts[idx + 1:]
            if line.startswith("bestmove"):
                bestmove = line.split()[1]
                break
        return bestmove, score, pv

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

moves = [
    '7g7f', '8c8d', '2g2f', '4a3b', '2f2e', '8d8e', '6i7h', '8e8f',
    '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', 'P*2c'
]

print(f"Starting Game 1 solve from ply {len(moves)} (after Suisho 14: P*2c)...")
engine = UsiEngine()
try:
    for ply in range(len(moves) + 1, 260):
        is_sente = (ply % 2 == 1)
        is_morse = is_sente  # Game 1 では Morse が先手
        turn_str = "Morse (Sente)" if is_morse else "Suisho (Gote)"
        
        mv, sc, pv = engine.go(moves, is_morse=is_morse)
        print(f"G1 Ply {ply:3d} | {turn_str:15s}: {mv:6s} (eval: {sc})", flush=True)
        
        if mv == "resign":
            winner = 'Morse' if not is_morse else 'Suisho'
            print(f"\nGame 1 Complete! Resignation at Ply {ply}! Winner: {winner}\n", flush=True)
            break
        moves.append(mv)
        
    with open("g1_ply14_p2c_win.txt", "w", encoding="utf-8") as f:
        f.write(repr(moves))
    print(f"Saved Game 1 winning line to g1_ply14_p2c_win.txt (Total moves: {len(moves)})")
finally:
    engine.close()
