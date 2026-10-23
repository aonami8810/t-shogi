import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

# 15手目 2d3d まで
moves = ['7g7f', '4a3b', '2g2f', '3c3d', '2f2e', '8c8d', '6i7h', '8d8e', '2e2d', '2c2d', '2h2d', '8e8f', '8g8f', '8b8f', '2d3d']

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(cmd): p.stdin.write(cmd + "\n"); p.stdin.flush()

send("usi")
while True:
    if "usiok" in p.stdout.readline(): break
send("isready")
while True:
    if "readyok" in p.stdout.readline(): break
send("usinewgame")

send(f"position startpos moves {' '.join(moves)}")
send("go btime 0 wtime 0 byoyomi 500")

bm = None
info = []
while True:
    l = p.stdout.readline().strip()
    if l.startswith("info"): info.append(l)
    if l.startswith("bestmove"):
        bm = l.split()[1]
        break

print("Suisho5 response after 15 plies (2d3d) with 4a3b opening:")
for inf in info[-2:]:
    print(" ", inf)
print("BESTMOVE =", bm)

send("quit")
p.kill()
