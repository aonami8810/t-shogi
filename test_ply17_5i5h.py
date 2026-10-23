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
        while True:
            if "usiok" in self.p.stdout.readline(): break
        self.send("isready")
        while True:
            if "readyok" in self.p.stdout.readline(): break
        self.send("usinewgame")

    def send(self, cmd):
        self.p.stdin.write(cmd + "\n")
        self.p.stdin.flush()

    def go(self, moves):
        self.send(f"position startpos moves {' '.join(moves)}")
        self.send("go btime 0 wtime 0 byoyomi 500")
        mv = None
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
                mv = l.split()[1]
                break
        return mv, score

    def close(self):
        try:
            self.send("quit")
            self.p.wait(timeout=2)
        except:
            self.p.kill()

if __name__ == "__main__":
    eng = Engine()
    try:
        moves = [
            '7g7f', '8c8d', '2g2f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
            '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2a3c',
            '5i5h' # 17手目: 5八金！
        ]
        print("Testing Suisho response at ply 18 to 17. 5i5h:")
        mv18, s18 = eng.go(moves)
        print(f"Ply 18 [Suisho]: {mv18} (eval: {s18})")
    finally:
        eng.close()
