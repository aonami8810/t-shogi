import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def query_pv(moves):
    p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8")
    p.stdin.write("usi\nisready\n")
    p.stdin.flush()
    while True:
        l = p.stdout.readline()
        if "readyok" in l: break
    p.stdin.write(f"position startpos moves {' '.join(moves)}\ngo btime 0 wtime 0 byoyomi 1000\n")
    p.stdin.flush()
    bm = None
    pv = None
    sc = None
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("info") and "score" in l and "pv" in l:
            pv = l
            parts = l.split()
            if "cp" in parts: sc = parts[parts.index("cp")+1]
            elif "mate" in parts: sc = "mate " + parts[parts.index("mate")+1]
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    p.stdin.write("quit\n")
    p.stdin.flush()
    p.wait()
    return bm, sc, pv

print("=== Game 1: 17th move for Sente after 8f7f ===")
g1_16 = ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "6i7h", "4a3b", "2e2d", "2c2d", "2h2d", "8e8f", "8g8f", "8b8f", "5i5h", "8f7f"]
bm1, sc1, pv1 = query_pv(g1_16)
print(f"Sente 17th move: {bm1} ({sc1})")
print(pv1)

print("\n=== Game 2: 6th move for Gote after 6i7h ===")
g2_5 = ["2g2f", "8c8d", "7g7f", "4a3b", "6i7h"]
bm2, sc2, pv2 = query_pv(g2_5)
print(f"Gote 6th move: {bm2} ({sc2})")
print(pv2)
