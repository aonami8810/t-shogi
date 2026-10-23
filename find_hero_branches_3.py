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

def play_match(initial_moves, target_turn, max_plies=250):
    engine = UsiEngine(SUISHO_EXE, SUISHO_DIR)
    moves = list(initial_moves)
    print(f"=== Starting match for {target_turn} Win from ply {len(moves)} ===", flush=True)
    try:
        for ply in range(len(moves) + 1, max_plies + 1):
            is_target = (ply % 2 == 1 and target_turn == "Sente") or (ply % 2 == 0 and target_turn == "Gote")
            byoyomi = 2000 if is_target else 500
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
    # 1. Game 1: 36手目 P*3g に対する先手必勝ライン
    g1_moves_init = [
        '2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f',
        '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c',
        '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d',
        'P*2d', 'R*2f', '3i3h', 'P*2h', '7g3c+', '2a3c', 'B*3g', '2f2d',
        '3g9a+', '2h2i+', '9a8a', 'P*3g'
    ]
    print("\n" + "="*80)
    print("SIMULATING GAME 1: from ply 36 (P*3g) to Sente Win")
    print("="*80)
    g1_full = play_match(g1_moves_init, target_turn="Sente", max_plies=250)
    with open("g1_ply36_p3g_hero_full.txt", "w", encoding="utf-8") as f:
        f.write(repr(g1_full))
    print(f"Saved Game 1 to g1_ply36_p3g_hero_full.txt, total plies: {len(g1_full)}")

    # 2. Game 2: 31手目 4g4f に対する後手必勝ライン
    g2_moves_init = [
        '2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f',
        '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c',
        '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d',
        '2i3g', '3c7g+', '8i7g', 'B*6d', '3i3h', 'R*2h', '4g4f'
    ]
    print("\n" + "="*80)
    print("SIMULATING GAME 2: from ply 31 (4g4f) to Gote Win")
    print("="*80)
    g2_full = play_match(g2_moves_init, target_turn="Gote", max_plies=250)
    with open("g2_ply31_4g4f_hero_full.txt", "w", encoding="utf-8") as f:
        f.write(repr(g2_full))
    print(f"Saved Game 2 to g2_ply31_4g4f_hero_full.txt, total plies: {len(g2_full)}")
