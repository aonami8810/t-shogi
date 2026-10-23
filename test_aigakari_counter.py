import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def query(moves, byoyomi=1000):
    p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8", errors="replace")
    p.stdin.write("usi\nisready\n")
    p.stdin.write(f"position startpos moves {' '.join(moves)}\n")
    p.stdin.write(f"go btime 0 wtime 0 byoyomi {byoyomi}\n")
    p.stdin.flush()
    bm, sc = None, 0
    while True:
        l = p.stdout.readline()
        if not l: break
        l = l.strip()
        if "score cp" in l:
            parts = l.split()
            if "cp" in parts:
                sc = int(parts[parts.index("cp") + 1])
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    p.stdin.write("quit\n")
    p.stdin.flush()
    p.wait()
    return bm, sc

# 9手目 2h2d (2四同飛) に対し、後手 10手目 4a3b (3二金)
test_moves = ['2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '7a7b', '2e2d', '2c2d', '2h2d', '4a3b']
bm, sc = query(test_moves, 1000)
print(f"After 4a3b, Sente move 11: {bm} (eval: {sc})")
