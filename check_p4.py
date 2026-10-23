import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8")
def cmd(c):
    p.stdin.write(c + "\n")
    p.stdin.flush()

cmd("usi")
cmd("isready")
while True:
    l = p.stdout.readline()
    if "readyok" in l: break

cmd("position startpos moves 7g7f 8c8d 8h2b+")
cmd("go btime 0 wtime 0 byoyomi 500")
while True:
    l = p.stdout.readline().strip()
    if l.startswith("bestmove"):
        print("Ply 4 bestmove:", l)
        break

cmd("quit")
p.wait()
