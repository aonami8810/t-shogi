import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class SuishoSession:
    def __init__(self, byoyomi_ms=1000):
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
    session = SuishoSession(1000)
    
    # 1. Game 1: 26手目 R*2g の局面（先手27手目の最善手探索）
    g1_26 = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d P*2d R*2g".split()
    bm1, sc1, pv1 = session.query(g1_26)
    print(f"Game 1 after R*2g (26 plies): Bestmove = {bm1} (eval: {sc1})")
    print(f"  PV: {pv1}")
    
    # 2. Game 2: 7手目 5i5h の局面（後手8手目の最善手探索）
    g2_7 = "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 5i5h".split()
    bm2, sc2, pv2 = session.query(g2_7)
    print(f"\nGame 2 after 5i5h (7 plies): Bestmove = {bm2} (eval: {sc2})")
    print(f"  PV: {pv2}")
    
    session.close()
