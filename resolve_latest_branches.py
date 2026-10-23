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

def simulate_to_end(initial_moves, target_winner, max_plies=300, byoyomi_ms=500):
    engine = UsiEngine(SUISHO_EXE, SUISHO_DIR)
    moves = list(initial_moves)
    print(f"=== Starting simulation from {len(moves)} plies ===", flush=True)
    try:
        for ply in range(len(moves) + 1, max_plies + 1):
            turn = "Sente" if (ply % 2 == 1) else "Gote"
            mv, score, pv = engine.go(moves, byoyomi_ms=byoyomi_ms)
            print(f"Ply {ply:3d} [{turn:5s}]: {mv:6s} (eval: {score})", flush=True)
            if mv == "resign":
                opp = "Gote" if turn == "Sente" else "Sente"
                print(f"*** Resign at ply {ply}! Winner = {opp} ***", flush=True)
                break
            moves.append(mv)
    finally:
        engine.close()
    return moves

if __name__ == "__main__":
    # 1. Game 1: 28手目 7a7b に対するシミュレーション（先手 Morse 勝利）
    g1_moves_init = [
        '2g2f', '8c8d', '7g7f', '8d8e', '2f2e', '3c3d', '6i7h', '8e8f',
        '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c',
        '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d',
        'P*2d', 'P*2b', 'R*8c', '7a7b'
    ]
    print("\n" + "="*80)
    print("SIMULATING GAME 1: from ply 28 (7a7b) to Sente Win")
    print("="*80)
    g1_full = simulate_to_end(g1_moves_init, target_winner="Sente", max_plies=300, byoyomi_ms=500)
    with open("g1_ply28_7a7b_full.txt", "w", encoding="utf-8") as f:
        f.write(repr(g1_full))
    print(f"Game 1 full line saved to g1_ply28_7a7b_full.txt, total plies: {len(g1_full)}")

    # 2. Game 2: 43手目 7i6h に対するシミュレーション（後手 Morse 勝利）
    g2_moves_init = [
        '2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f',
        '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c',
        '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d',
        'P*2d', 'P*2b', '3i3h', '3a4b', 'R*8c', 'P*8b', '8c8e+', '3c7g+',
        '8i7g', 'B*2h', '2d2c+', '2b2c', 'B*3g', '2h3g+', '3h3g', '2a3c',
        'P*2h', '4c4d', '7i6h'
    ]
    print("\n" + "="*80)
    print("SIMULATING GAME 2: from ply 43 (7i6h) to Gote Win")
    print("="*80)
    g2_full = simulate_to_end(g2_moves_init, target_winner="Gote", max_plies=300, byoyomi_ms=500)
    with open("g2_ply43_7i6h_full.txt", "w", encoding="utf-8") as f:
        f.write(repr(g2_full))
    print(f"Game 2 full line saved to g2_ply43_7i6h_full.txt, total plies: {len(g2_full)}")
