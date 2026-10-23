import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(cmd): p.stdin.write(cmd + "\n"); p.stdin.flush()

send("usi")
send("isready")
while True:
    if "readyok" in p.stdout.readline(): break

# 14手目まで (相掛かり横歩取り局面)
moves_14 = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b"
# 15手目 5i5h (5八玉中住まい)
moves_15 = moves_14 + " 5i5h"
send(f"position startpos moves {moves_15}")
send("go btime 0 wtime 0 byoyomi 500")

while True:
    line = p.stdout.readline().strip()
    if line.startswith("info") and "score" in line:
        info = line
    if line.startswith("bestmove"):
        bm = line.split()[1]
        print(f"Suisho5 reply to 5i5h: {bm} | {info}")
        break

send("quit")
