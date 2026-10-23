import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class UsiEngine:
    def __init__(self):
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

    def check_mate(self, moves):
        self.send(f"position startpos moves {' '.join(moves)}")
        self.send("go mate 3000") # 3秒で詰み探索
        res = []
        while True:
            line = self.proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            res.append(line)
            if line.startswith("checkmate") or line.startswith("bestmove"):
                break
        return res

    def analyze(self, moves, depth=18):
        self.send(f"position startpos moves {' '.join(moves)}")
        self.send(f"go depth {depth}")
        while True:
            line = self.proc.stdout.readline().strip()
            if line.startswith("info depth") and "score" in line and "pv" in line:
                last_info = line
            if line.startswith("bestmove"):
                return line.split()[1], last_info

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

g1_moves = ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', '3a2b', '2d2c+', '2b2c', 'P*2b', '3b2b', '2i3g', '4c4d', '7g5e', 'P*8b', '8i7g', '5c5d', '5e4f', '3c4b', 'P*8c', '6c6d', '6g6f', '7d7e', 'R*8d', 'R*9d', '8d8f', '6a7b', '6f6e', '8b8c', '4f6d', '9d6d', '6e6d', '4b6d', '5h4h', '2b3b', '3i3h', 'P*2g', '4i3i', '7a6b', '7i6h', '2c3d', '3h2g', 'P*2f', '2g3h', '3d4c', 'P*8d', 'B*9d', '8d8c+', '7b8c', 'P*8e', '8c7d', 'P*6c', '6b7a', '6h6g', '9d7b', '3i4i', '8a7c', '8e8d', 'P*8b', '9g9f', 'P*6e', 'R*2d', '3b3a', '2d2c+', '1c1d', '1g1f', '2f2g+', '2c2g', 'P*2b', '4g4f', '3a3b', '4i5h', '5b6c', '8f8i', '7a6b', 'P*6f', '6e6f', '6g6f', 'P*6e', '8d8c+', '8b8c']

if __name__ == "__main__":
    eng = UsiEngine()
    try:
        cur = list(g1_moves)
        for i in range(20):
            bm, info = eng.analyze(cur, depth=16)
            turn = "Sente" if (len(cur) % 2 == 0) else "Gote"
            print(f"Ply {len(cur)+1:3d} [{turn}]: {bm} | {info}")
            cur.append(bm)
            if "mate" in info:
                print("MATE DETECTED!")
    finally:
        eng.close()
