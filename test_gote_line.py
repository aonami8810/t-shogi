import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class SuishoPlayer:
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

    def get_suisho_move(self, moves, byoyomi_ms=500):
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
        bestmove = None
        score = None
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
            if line.startswith("bestmove"):
                bestmove = line.split()[1]
                break
        return bestmove, score

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

if __name__ == "__main__":
    suisho = SuishoPlayer()
    # 4手目 4a3b, 6手目 3c3d 固定
    moves = ["7g7f", "8c8d", "8h7g", "4a3b"]
    try:
        # 5手目 水匠5
        m5, s5 = suisho.get_suisho_move(moves, byoyomi_ms=500)
        print(f"Ply 5 (Suisho5): {m5} (eval: {s5})")
        moves.append(m5)
        # 6手目 Morse 3c3d
        moves.append("3c3d")
        print(f"Ply 6 (Morse): 3c3d")
        
        # 7手目以降、水匠5の手を追跡
        for ply in range(7, 30):
            if ply % 2 == 1:
                # 先手（水匠5）
                mv, sc = suisho.get_suisho_move(moves, byoyomi_ms=500)
                print(f"Ply {ply:2d} [Suisho5]: {mv:6s} (eval: {sc})")
                moves.append(mv)
            else:
                # 後手（Morse）: 水匠5の深層探索で最善応手
                suisho.send(f"position startpos moves {' '.join(moves)}")
                suisho.send("go depth 16")
                while True:
                    line = suisho.proc.stdout.readline().strip()
                    if line.startswith("bestmove"):
                        mv = line.split()[1]
                        break
                print(f"Ply {ply:2d} [Morse  ]: {mv:6s}")
                moves.append(mv)
    finally:
        suisho.close()

    print("Resulting moves: " + repr(moves))
