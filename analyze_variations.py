import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def query_suisho(moves, byoyomi_ms=1000):
    proc = subprocess.Popen(
        [SUISHO_EXE],
        cwd=SUISHO_DIR,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8"
    )
    def send(c):
        proc.stdin.write(c + "\n")
        proc.stdin.flush()
    def read_until(t):
        while True:
            l = proc.stdout.readline()
            if not l: break
            if t in l: break

    send("usi")
    read_until("usiok")
    send("isready")
    read_until("readyok")
    send("usinewgame")
    
    if moves:
        send(f"position startpos moves {' '.join(moves)}")
    else:
        send("position startpos")
    send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
    
    bestmove = None
    score = None
    pv = None
    while True:
        l = proc.stdout.readline()
        if not l: break
        l = l.strip()
        if l.startswith("info") and "score" in l and "pv" in l:
            pv = l
            parts = l.split()
            if "cp" in parts:
                idx = parts.index("cp")
                score = parts[idx+1]
            elif "mate" in parts:
                idx = parts.index("mate")
                score = "mate " + parts[idx+1]
        if l.startswith("bestmove"):
            bestmove = l.split()[1]
            break
            
    proc.stdin.write("quit\n")
    proc.stdin.flush()
    proc.wait()
    return bestmove, score, pv

print("=== Analyze Game 1: 7th move for Sente ===")
g1 = ["7g7f", "8c8d", "2g2f", "4a3b", "6i7h", "8d8e"]
bm, sc, pv = query_suisho(g1, byoyomi_ms=1000)
print(f"Game 1 7th move: {bm}, score: {sc}")
print(f"PV: {pv}")

print("\n=== Analyze Game 2: 7th move for Gote after 5i5h ===")
g2_7 = ["2g2f", "8c8d", "2f2e", "8d8e", "7g7f", "3c3d", "5i5h"]
bm2, sc2, pv2 = query_suisho(g2_7, byoyomi_ms=1000)
print(f"Game 2 8th move: {bm2}, score: {sc2}")
print(f"PV: {pv2}")
