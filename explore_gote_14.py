import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def query(moves, byoyomi=1000):
    p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8", errors="replace")
    p.stdin.write("usi\nisready\n")
    p.stdin.write(f"position startpos moves {' '.join(moves)}\n")
    p.stdin.write(f"go btime 0 wtime 0 byoyomi {byoyomi}\n")
    p.stdin.flush()
    bm, sc, pv = None, None, None
    while True:
        l = p.stdout.readline()
        if not l: break
        l = l.strip()
        if "info" in l and "score" in l and "pv" in l:
            pv = l
            parts = l.split()
            if "cp" in parts:
                sc = parts[parts.index("cp") + 1]
            elif "mate" in parts:
                sc = "mate " + parts[parts.index("mate") + 1]
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    p.stdin.write("quit\n")
    p.stdin.flush()
    p.wait()
    return bm, sc, pv

# 13手目までの相掛かり必然手順
base_moves = ['2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '4a3b', '3i3h', '7a7b', '2e2d', '2c2d', '2h2d', 'P*2c', '2d2h']

# 後手の手番。候補手を探る
print("=== Candidates for Gote at Move 14 ===")
for candidate in ["9c9d", "1c1d", "3c3d", "8e8f", "5a4b"]:
    bm, sc, pv = query(base_moves + [candidate], 1000)
    print(f"Move 14: {candidate:6s} -> Sente bm: {bm:6s} (eval: {sc})")
