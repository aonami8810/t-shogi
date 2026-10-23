import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

proc = subprocess.Popen(
    [SUISHO_EXE],
    cwd=SUISHO_DIR,
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True,
    encoding="utf-8"
)

def send(cmd):
    proc.stdin.write(cmd + "\n")
    proc.stdin.flush()

send("usi")
while True:
    line = proc.stdout.readline()
    if "usiok" in line:
        break

send("isready")
while True:
    line = proc.stdout.readline()
    if "readyok" in line:
        break

moves = [
    '7g7f', '3c3d', '2g2f', '8c8d', '6i7h', '8d8e', '2f2e', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'P*2b', '3i3h', '3a4b', 'R*8c', 'P*8b',
    '8c8e+', '3c7g+', '8i7g', 'B*2h', '2d2c+', '2b2c', 'B*3g', '2h3g+', '3h3g', '2a3c',
    'P*2h', '4c4d', '7i6h', '1c1d', '7h7i', '1d1e', '3f3e'
]

send("setoption name MultiPV value 5")
send(f"position startpos moves {' '.join(moves)}")
send("go btime 0 wtime 0 byoyomi 3000")

while True:
    line = proc.stdout.readline()
    if not line:
        break
    line = line.strip()
    if line.startswith("info") and "multipv" in line and "score" in line:
        print(line)
    if line.startswith("bestmove"):
        print(line)
        break

send("quit")
proc.terminate()
