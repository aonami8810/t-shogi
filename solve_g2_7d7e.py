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
    '7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '5i5h', '8e8f',
    '8g8f', '5a5b', '8h2b+', '3a2b', 'B*7g', '4a3b', '2e2d', '2c2d',
    '2h2d', '2a3c', '7i7h', 'P*8h', '7g8h', '8b8f', '8h7g', '8f7f',
    '2d3d', '7f2f', 'P*2h', '7c7d', '7g5e', 'B*7c', '5e7c+', '8a7c',
    '3d7d', '7a6b', '7d7e'
]

print(f"Starting Game 2 solve from ply {len(moves)} (after Suisho 35: 7d7e)...")
engine = UsiEngine()
try:
    for ply in range(len(moves) + 1, 260):
        is_sente = (ply % 2 == 1)
        is_morse = not is_sente  # Game 2 では Morse が後手
        turn_str = "Morse (Gote)" if is_morse else "Suisho (Sente)"
        
        mv, sc, pv = engine.go(moves, is_morse=is_morse)
        print(f"G2 Ply {ply:3d} | {turn_str:15s}: {mv:6s} (eval: {sc})", flush=True)
        
        if mv == "resign":
            winner = 'Morse' if not is_morse else 'Suisho'
            print(f"\nGame 2 Complete! Resignation at Ply {ply}! Winner: {winner}\n", flush=True)
            break
        moves.append(mv)
        
    with open("g2_ply35_7d7e_win.txt", "w", encoding="utf-8") as f:
        f.write(repr(moves))
    print(f"Saved Game 2 winning line to g2_ply35_7d7e_win.txt (Total moves: {len(moves)})")
finally:
    engine.close()
