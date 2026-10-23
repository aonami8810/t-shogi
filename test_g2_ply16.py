import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class SuishoSelfPlay:
    def __init__(self, byoyomi_ms=500):
        self.byoyomi_ms = byoyomi_ms
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
        self.init()

    def send(self, cmd):
        self.proc.stdin.write(cmd + "\n")
        self.proc.stdin.flush()

    def read_until(self, target):
        lines = []
        while True:
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            lines.append(line)
            if target in line: break
        return lines

    def init(self):
        self.send("usi")
        self.read_until("usiok")
        self.send("isready")
        self.read_until("readyok")
        self.send("usinewgame")

    def get_bestmove(self, moves):
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send(f"go btime 0 wtime 0 byoyomi {self.byoyomi_ms}")
        
        bestmove = None
        score = None
        while True:
            line = self.proc.stdout.readline()
            if not line: break
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
        except:
            self.proc.kill()

def test_gote16_candidates():
    prefix = "2g2f 8c8d 7g7f 4a3b 6i7h 8d8e 8h7g 3c3d 7i6h 2b7g+ 6h7g 3a2b 3i3h 2b3c 4g4f".split()
    session = SuishoSelfPlay(500)
    
    # 後手 16手目の候補
    cands = ["7c7d", "6c6d", "5a4b", "1c1d", "7a6b", "4c4d", "8a7c"]
    print("=== Testing Gote 16th ply candidates ===")
    for c in cands:
        bm, score = session.get_bestmove(prefix + [c])
        print(f"16. {c} -> Suisho5 (先手): {bm} (先手 eval: {score})")
    
    session.close()

if __name__ == "__main__":
    test_gote16_candidates()
