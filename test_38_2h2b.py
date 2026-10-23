# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class Engine:
    def __init__(self):
        self.p = subprocess.Popen(
            [SUISHO_EXE],
            cwd=SUISHO_DIR,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            text=True,
            encoding="utf-8"
        )
        self.send("usi")
        self.read_until("usiok")
        self.send("setoption name USI_OwnBook value false")
        self.send("isready")
        self.read_until("readyok")
        self.send("usinewgame")

    def send(self, cmd):
        self.p.stdin.write(cmd + "\n")
        self.p.stdin.flush()

    def read_until(self, target):
        while True:
            line = self.p.stdout.readline()
            if not line or target in line:
                break

    def go(self, moves, is_sente=True):
        self.send(f"position startpos moves {' '.join(moves)}")
        if not is_sente:
            self.send("go nodes 5000000")
        else:
            self.send("go btime 0 wtime 0 byoyomi 500")
        bestmove = None
        score = None
        while True:
            l = self.p.stdout.readline()
            if not l: break
            l = l.strip()
            if l.startswith("info") and "score" in l:
                parts = l.split()
                if "cp" in parts:
                    score = parts[parts.index("cp") + 1]
                elif "mate" in parts:
                    score = "mate " + parts[parts.index("mate") + 1]
            if l.startswith("bestmove"):
                bestmove = l.split()[1]
                break
        return bestmove, score

    def close(self):
        try:
            self.send("quit")
            self.p.wait(timeout=2)
        except:
            self.p.kill()

if __name__ == "__main__":
    moves = [
        '7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
        '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
        '8h7g', '7f7d', '3d7d', '7c7d', '2i3g', '3c7g+', '8i7g', 'B*6d', '3i3h', 'R*2h',
        '4g4f', 'P*3e', '3g4e', '3e3f', '7g6e', '3a4b', 'R*3d',
        '2h2b+' # 38手目: 竜作り！
    ]

    print("Testing 38. 2h2b+ rollout:")
    eng = Engine()
    try:
        for ply in range(39, 100):
            is_sente = (ply % 2 == 1)
            turn = "Sente" if is_sente else "Gote"
            mv, score = eng.go(moves, is_sente=is_sente)
            print(f"Ply {ply:3d} [{turn:5s}]: {mv:6s} (eval: {score})", flush=True)
            if mv == "resign":
                print(f"*** Resign at ply {ply}! ***")
                break
            moves.append(mv)
    finally:
        eng.close()
