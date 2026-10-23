# -*- coding: utf-8 -*-
import subprocess
import ast

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

    def go(self, moves, byoyomi_ms=500):
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
        
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
    with open("g2_ply45_8e7d_full.txt", "r", encoding="utf-8") as f:
        moves = ast.literal_eval(f.read().strip())
    
    print(f"Resuming Game 2 simulation from ply {len(moves)}...")
    engine = UsiEngine(SUISHO_EXE, SUISHO_DIR)
    try:
        for ply in range(len(moves) + 1, 350):
            turn = "Sente" if (ply % 2 == 1) else "Gote"
            mv, score, pv = engine.go(moves, byoyomi_ms=500)
            print(f"Ply {ply:3d} [{turn:5s}]: {mv:6s} (eval: {score})", flush=True)
            if mv == "resign":
                opp = "Gote" if turn == "Sente" else "Sente"
                print(f"*** Resign at ply {ply}! Winner = {opp} ***", flush=True)
                break
            moves.append(mv)
    finally:
        engine.close()
        
    with open("g2_ply45_8e7d_full.txt", "w", encoding="utf-8") as f:
        f.write(repr(moves))
    print(f"Updated g2_ply45_8e7d_full.txt with {len(moves)} plies!")
