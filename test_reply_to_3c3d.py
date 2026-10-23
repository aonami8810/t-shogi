import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

moves = ['2g2f', '3c3d', '7g7f']
send(f"position startpos moves {' '.join(moves)}")
send("go btime 0 wtime 0 byoyomi 500")
while True:
    l = p.stdout.readline().strip()
    if l.startswith("info") and "score" in l: sc = l
    if l.startswith("bestmove"):
        print("Suisho ply 4 response to 2g2f 3c3d 7g7f:", l, "|", sc)
        break

send("quit")
