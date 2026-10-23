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
        
        # In Game 2, Morse is Gote (is_sente=False) -> nodes 5000000
        # Suisho 5 is Sente (is_sente=True) -> byoyomi 500ms
        if not is_sente:
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
        '2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '5i5h', '8e8f', '8g8f', '5a5b',
        '3i3h', '7a7b', '8h2b+', '3a2b', '7i8h', '4a3b', '8h7g', '2b3c', '6i7h', '7c7d',
        '4g4f', '7b7c', '3g3f', '7c6d', '2i3g', '7d7e', '7f7e', '6d7e', '2e2d', '2c2d',
        'P*2b', '3c2b', '2h2d', '7e6d', '4i4h', 'P*7f', '7g6f', '8b8f', 'P*8g', '8f8b',
        '2d2i', 'P*2c', '3h4g', '1c1d', '9g9f', '5b4b', '4g5f', '3d3e', '5f4g', 'B*5d',
        'B*4e', '5d4e', '4f4e', '3e3f', '4g3f', 'P*3d', '4e4d', '4c4d', 'P*4e', '4d4e',
        '3f4e', 'P*4c', 'P*3f', '2b3c', 'B*5f', '5c5d', '4e3d', '3c3d', '5f3d', 'S*2d',
        'P*7b', '6a7b', 'P*7d', 'P*3e' # 74手目に千日手打開手 P*3e を確定投入！
    ]
    
    print(f"Starting Game 2 rollout from ply {len(moves)} (P*3e breakthrough)...")
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
        
    with open("g2_ply13_5i5h_win.txt", "w", encoding="utf-8") as f:
        f.write(repr(moves))
    print(f"Saved g2_ply13_5i5h_win.txt with {len(moves)} plies!")
