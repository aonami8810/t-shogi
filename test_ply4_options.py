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
        # Option A: 2g2f 8c8d 7g7f 3c3d
        print("Testing Option A: 2g2f 8c8d 7g7f 3c3d")
        mv5_a, s5_a = eng.go(['2g2f', '8c8d', '7g7f', '3c3d'])
        print(f"  Ply 5 [Suisho]: {mv5_a} (eval: {s5_a})")
        
        # Option B: 2g2f 8c8d 7g7f 8d8e
        print("Testing Option B: 2g2f 8c8d 7g7f 8d8e")
        mv5_b, s5_b = eng.go(['2g2f', '8c8d', '7g7f', '8d8e'])
        print(f"  Ply 5 [Suisho]: {mv5_b} (eval: {s5_b})")
        
        # Option C: 2g2f 8c8d 7g7f 4a3b
        print("Testing Option C: 2g2f 8c8d 7g7f 4a3b")
        mv5_c, s5_c = eng.go(['2g2f', '8c8d', '7g7f', '4a3b'])
        print(f"  Ply 5 [Suisho]: {mv5_c} (eval: {s5_c})")
    finally:
        eng.close()
