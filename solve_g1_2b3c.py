# -*- coding: utf-8 -*-
import subprocess
import time

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

def play_match(initial_moves, target_turn="Sente", max_plies=250):
    engine = UsiEngine(SUISHO_EXE, SUISHO_DIR)
    moves = list(initial_moves)
    print(f"=== Starting match for {target_turn} Win from ply {len(moves)} ===", flush=True)
    try:
        for ply in range(len(moves) + 1, max_plies + 1):
            is_target = (ply % 2 == 1 and target_turn == "Sente") or (ply % 2 == 0 and target_turn == "Gote")
            byoyomi = 3500 if is_target else 500
            turn = "Sente" if (ply % 2 == 1) else "Gote"
            
            mv, score, pv = engine.go(moves, byoyomi_ms=byoyomi)
            print(f"Ply {ply:3d} [{turn:5s} {'*HERO*' if is_target else 'opp'   }]: {mv:6s} (eval: {score})", flush=True)
            
            if mv == "resign":
                opp = "Gote" if turn == "Sente" else "Sente"
                print(f"*** Resign at ply {ply}! Winner = {opp} ***", flush=True)
                break
            moves.append(mv)
    finally:
        engine.close()
    return moves

if __name__ == "__main__":
    g1_moves_init = [
        '7g7f', '8c8d', '2g2f', '8d8e', '2f2e', '4a3b',
        '8h7g', '3c3d', '7i8h', '2b7g+', '8h7g', '3a2b',
        '3g3f', '2b3c'
    ]
    print("\n" + "="*80)
    print("SIMULATING GAME 1: from ply 14 (2b3c) to Sente Win")
    print("="*80)
    g1_full = play_match(g1_moves_init, target_turn="Sente", max_plies=250)
    with open("g1_ply14_2b3c_hero_full.txt", "w", encoding="utf-8") as f:
        f.write(repr(g1_full))
    print(f"Saved Game 1 to g1_ply14_2b3c_hero_full.txt, total plies: {len(g1_full)}")
