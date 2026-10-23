import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class SuishoSession:
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

    def query(self, moves):
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send(f"go btime 0 wtime 0 byoyomi {self.byoyomi_ms}")
        
        bestmove = None
        score = None
        pv = ""
        while True:
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            if line.startswith("info") and "score" in line and "pv" in line:
                pv = line
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
        return bestmove, score, pv

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except:
            self.proc.kill()

if __name__ == "__main__":
    session = SuishoSession(500)
    
    # Game 1: 7g7f 4a3b に対する先手の最善手候補
    cands_g1_3 = ["2g2f", "3g3f", "5i5h", "8h2b+", "7i6h"]
    print("=== Testing Sente 3rd ply candidates against 7g7f 4a3b ===")
    for c in cands_g1_3:
        bm, sc, pv = session.query(["7g7f", "4a3b", c])
        print(f"3. {c} -> Suisho5 (4th ply): {bm} (Suisho5 eval: {sc})")
    
    # Game 2: 水匠5の初手 7g7f に対する後手の最善手候補
    cands_g2_2 = ["3c3d", "8c8d", "5c5d", "4c4d"]
    print("\n=== Testing Gote 2nd ply candidates against 7g7f ===")
    for c in cands_g2_2:
        bm, sc, pv = session.query(["7g7f", c])
        print(f"2. {c} -> Suisho5 (3rd ply): {bm} (Suisho5 eval: {sc})")
        
    session.close()
