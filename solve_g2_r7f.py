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
            byoyomi = 4000 if is_target else 500
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
    # 94手目 R*7f を指定
    g2_moves = [
        '2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
        '2e2d', '2c2d', '2h2d', '4a3b', '5i5h', '8f7f', '8h2b+', '3a2b', '2d3d', '2b3c',
        '3d3f', '7f3f', '3g3f', 'P*8b', '3i3h', '5a5b', 'B*4f', 'P*2g', 'R*2f', '3c2b',
        '2f2g', '6a7b', '2i3g', '7a6b', '4f5e', 'P*2c', '5e6f', '2b3c', 'P*2d', '2c2d',
        '3g4e', '3c4d', '2g2d', 'P*2b', '7i6h', '6c6d', '6f4d', '4c4d', '2d4d', 'R*3a',
        '5g5f', 'B*2h', 'P*2c', '2h1i+', '2c2b+', '3b2b', 'S*4c', '5b6c', 'P*2c', '2b1b',
        '4c4b', '3a3f', '4d4c+', 'L*5b', '4e3c+', '2a3c', '4b3c+', 'B*2e', 'N*5e', '6c7d',
        '4c4e', '2e1d', '3c4b', '1i1h', '4e4d', 'P*4h', 'P*3g', '3f5f', '6h5g', '5f5g+',
        '5h5g', '4h4i+', '4b5b', '1h5d', '4d5d', '5c5d', 'B*9f', '7d8c', '5b6b', 'N*6e',
        '5g6f', 'G*8f', '6f7e', 'R*7f'
    ]
    ok, m = play_match(g2_moves, "Gote", "g2_ply29_r2f_r7f_full.txt")
    print("Done:", ok)
