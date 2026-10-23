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
        
        # In Game 1, Morse is Sente (is_sente=True) -> nodes 5000000
        # Suisho 5 is Gote (is_sente=False) -> byoyomi 500ms
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

if __name__ == "__main__":
    moves = [
        '7g7f', '4a3b', '2g2f', '8c8d', '2f2e', '8d8e', '8h7g', '3c3d', '7i8h', '2b7g+',
        '8h7g', '3a2b', '3g3f', '2b3c', '6i7h', '7c7d', '3i4h', '7a7b', '4h3g', '7b7c',
        '3g4f', '7c6d', '9g9f', '1c1d', '6g6f' # 25手目に最善手 6g6f を投入！
    ]
    
    print(f"Starting Game 1 rollout from ply {len(moves)} (6g6f kakugawari line)...")
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
        
    with open("g1_ply25_6g6f_win.txt", "w", encoding="utf-8") as f:
        f.write(repr(moves))
    print(f"Saved g1_ply25_6g6f_win.txt with {len(moves)} plies!")
