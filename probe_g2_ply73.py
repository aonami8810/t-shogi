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
    '2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '5i5h', '8e8f', '8g8f', '5a5b',
    '3i3h', '7a7b', '8h2b+', '3a2b', '7i8h', '4a3b', '8h7g', '2b3c', '6i7h', '7c7d',
    '4g4f', '7b7c', '3g3f', '7c6d', '2i3g', '7d7e', '7f7e', '6d7e', '2e2d', '2c2d',
    'P*2b', '3c2b', '2h2d', '7e6d', '4i4h', 'P*7f', '7g6f', '8b8f', 'P*8g', '8f8b',
    '2d2i', 'P*2c', '3h4g', '1c1d', '9g9f', '5b4b', '4g5f', '3d3e', '5f4g', 'B*5d',
    'B*4e', '5d4e', '4f4e', '3e3f', '4g3f', 'P*3d', '4e4d', '4c4d', 'P*4e', '4d4e',
    '3f4e', 'P*4c', 'P*3f', '2b3c', 'B*5f', '5c5d', '4e3d', '3c3d', '5f3d', 'S*2d',
    'P*7b', '6a7b', 'P*7d'
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
