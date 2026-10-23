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
    
    # Game 1: 7g7f 4a3b 2g2f 3c3d 2f2e に対する水匠5の6手目
    bm, sc, pv = session.query(["7g7f", "4a3b", "2g2f", "3c3d", "2f2e"])
    print(f"Game 1 (5 plies: 7g7f 4a3b 2g2f 3c3d 2f2e) -> Suisho5 (6th ply): {bm} (eval: {sc})")
    print(f"  PV: {pv}")
    
    # Game 2: 7g7f 8c8d 2g2f に対する後手の手候補（8d8e または 3c3d）
    for g4 in ["8d8e", "3c3d"]:
        bm, sc, pv = session.query(["7g7f", "8c8d", "2g2f", g4])
        print(f"\nGame 2 (4 plies: 7g7f 8c8d 2g2f {g4}) -> Suisho5 (5th ply): {bm} (eval: {sc})")
        print(f"  PV: {pv}")
        
    session.close()
