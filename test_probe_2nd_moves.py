import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi")
send("isready")
while "readyok" not in p.stdout.readline(): pass

candidates = ["8c8d", "3c3d", "4a3b", "5a4b", "5a5b", "8b3b", "8b4b", "8b5b", "1c1d", "9c9d", "7c7d"]

for c in candidates:
    send(f"position startpos moves 2g2f {c}")
    send("go btime 0 wtime 0 byoyomi 500")
    bm = None
    sc = ""
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("info") and "score" in l:
            sc = l
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    print(f"2g2f -> {c:5s} => Suisho Ply 3: {bm:6s} ({sc})")

send("quit")
