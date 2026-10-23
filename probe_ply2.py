import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def ask(moves):
    p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
    def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
    send("usi")
    send("isready")
    while "readyok" not in p.stdout.readline(): pass
    mv_str = " ".join(moves)
    send(f"position startpos moves {mv_str}")
    send("go btime 0 wtime 0 byoyomi 500")
    bm, sc = None, None
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("info") and "score" in l: sc = l
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    send("quit")
    p.wait()
    return bm, sc

print("Ply 1: 7g7f")
for m2 in ['8c8d', '3c3d', '5c5d', '4c4d']:
    bm, sc = ask(['7g7f', m2])
    print(f"If Gote plays {m2} -> Suisho5 plays {bm} ({sc})")
