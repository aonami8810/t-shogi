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

def play_match(initial_moves, target_turn, out_filename, max_plies=250):
    engine = UsiEngine(SUISHO_EXE, SUISHO_DIR)
    moves = list(initial_moves)
    print(f"\n=======================================================", flush=True)
    print(f"=== Starting match for {target_turn} Win from ply {len(moves)} ===", flush=True)
    print(f"=======================================================", flush=True)
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
                if opp == target_turn:
                    print(f"SUCCESS! Target {target_turn} WON in {ply - 1} moves!", flush=True)
                    with open(out_filename, "w", encoding="utf-8") as f:
                        f.write(repr(moves))
                    return True, moves
                else:
                    print(f"FAILURE! Target {target_turn} LOST!", flush=True)
                    return False, moves
            
            moves.append(mv)
    finally:
        engine.close()

if __name__ == "__main__":
    # 45手目 8e3e に対し 46手目 2a3c を指定してスタート
    g2_moves = [
        '2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
        '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
        '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'P*2b', '3i3h', '3a4b', 'R*8c', 'P*8b',
        '8c8e+', '3c7g+', '8i7g', 'B*2h', 'B*3g', '2h3g+', '2i3g', 'B*2h', 'P*8c', '8b8c',
        'B*4f', '2h1i+', '4f9a+', '7a7b', '8e3e', '2a3c'
    ]
    ok, m = play_match(g2_moves, "Gote", "g2_ply45_8e3e_hero_full.txt")
    print("Done:", ok)
