# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class UsiEngine:
    def __init__(self, exe_path, cwd):
        self.proc = subprocess.Popen(
            [exe_path],
            cwd=cwd,
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
            if not line:
                break
            line = line.strip()
            lines.append(line)
            if target in line:
                break
        return lines

    def go(self, moves, is_sente=True):
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        
        # Morse is Sente -> nodes 5000000
        # Suisho5 is Gote -> byoyomi 500ms
        if is_sente:
            self.send("go nodes 5000000")
        else:
            self.send("go btime 0 wtime 0 byoyomi 500")
        
        bestmove = None
        score = None
        pv = None
        
        while True:
            line = self.proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            if line.startswith("info") and "score" in line and "pv" in line:
                parts = line.split()
                if "cp" in parts:
                    idx = parts.index("cp")
                    if idx + 1 < len(parts):
                        score = parts[idx + 1]
                elif "mate" in parts:
                    idx = parts.index("mate")
                    if idx + 1 < len(parts):
                        score = "mate " + parts[idx + 1]
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

if __name__ == "__main__":
    moves = [
        '7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
        '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
        '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'R*2g', '7g3c+', '2a3c', '3i3h', '2g2e+',
        'R*2a', 'B*2b', '2i3g', '2e2f'
    ]
    
    print(f"Starting Game 1 rollout from ply {len(moves)} (2e2f)...")
    engine = UsiEngine(SUISHO_EXE, SUISHO_DIR)
    try:
        for ply in range(len(moves) + 1, 300):
            is_sente = (ply % 2 == 1)
            turn = "Sente" if is_sente else "Gote"
            mv, score, pv = engine.go(moves, is_sente=is_sente)
            print(f"Ply {ply:3d} [{turn:5s}]: {mv:6s} (eval: {score})", flush=True)
            if mv == "resign":
                opp = "Gote" if turn == "Sente" else "Sente"
                print(f"*** Resign at ply {ply}! Winner = {opp} ***", flush=True)
                break
            moves.append(mv)
    finally:
        engine.close()
        
    with open("g1_ply34_2e2f_win.txt", "w", encoding="utf-8") as f:
        f.write(repr(moves))
    print(f"Saved g1_ply34_2e2f_win.txt with {len(moves)} plies!")
