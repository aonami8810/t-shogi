# -*- coding: utf-8 -*-
import subprocess
import time

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class UsiEngine:
    def __init__(self, name):
        self.name = name
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
            if not line:
                break
            line = line.strip()
            lines.append(line)
            if target in line:
                break
        return lines

    def go(self, moves, is_morse=True):
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        
        # Morse uses deep search (nodes 5000000) to find the winning crush
        # Suisho uses the exact match condition (byoyomi 500)
        if is_morse:
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

def rollout_game1():
    print("=" * 70)
    print("ROLLOUT GAME 1 (Morse Sente vs Suisho Gote)")
    print("=" * 70)
    g1_moves = [
        '7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
        '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b8h+', '7i8h', '2a3c', '3d2d', '3a2b',
        '5i5h', '7a7b'
    ]
    
    engine = UsiEngine("Game1")
    try:
        for ply in range(len(g1_moves) + 1, 200):
            is_sente = (ply % 2 == 1)
            is_morse = is_sente
            turn_str = "Morse (Sente)" if is_morse else "Suisho (Gote)"
            
            mv, sc, pv = engine.go(g1_moves, is_morse=is_morse)
            print(f"G1 Ply {ply:3d} | {turn_str:15s}: {mv:6s} (eval: {sc})", flush=True)
            
            if mv == "resign":
                print(f"\nGame 1 Complete! Resignation at Ply {ply}! Winner: {'Morse' if not is_morse else 'Suisho'}\n", flush=True)
                break
            g1_moves.append(mv)
            
        with open("g1_real_suisho_win.txt", "w", encoding="utf-8") as f:
            f.write(repr(g1_moves))
        print(f"Saved Game 1 winning line to g1_real_suisho_win.txt (Total moves: {len(g1_moves)})")
        return g1_moves
    finally:
        engine.close()

def rollout_game2():
    print("=" * 70)
    print("ROLLOUT GAME 2 (Suisho Sente vs Morse Gote)")
    print("=" * 70)
    g2_moves = [
        '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '3c3d', '2e2d', '2c2d', '2h2d', '4a3b',
        '7g7f'
    ]
    
    engine = UsiEngine("Game2")
    try:
        for ply in range(len(g2_moves) + 1, 200):
            is_sente = (ply % 2 == 1)
            is_morse = not is_sente # Gote = Morse
            turn_str = "Morse (Gote)" if is_morse else "Suisho (Sente)"
            
            mv, sc, pv = engine.go(g2_moves, is_morse=is_morse)
            print(f"G2 Ply {ply:3d} | {turn_str:15s}: {mv:6s} (eval: {sc})", flush=True)
            
            if mv == "resign":
                print(f"\nGame 2 Complete! Resignation at Ply {ply}! Winner: {'Morse' if not is_morse else 'Suisho'}\n", flush=True)
                break
            g2_moves.append(mv)
            
        with open("g2_real_suisho_win.txt", "w", encoding="utf-8") as f:
            f.write(repr(g2_moves))
        print(f"Saved Game 2 winning line to g2_real_suisho_win.txt (Total moves: {len(g2_moves)})")
        return g2_moves
    finally:
        engine.close()

if __name__ == "__main__":
    t0 = time.time()
    g1 = rollout_game1()
    g2 = rollout_game2()
    print(f"\nAll rollouts finished in {time.time() - t0:.1f}s")
