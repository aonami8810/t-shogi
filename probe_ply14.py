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
    '7g7f', '4a3b', '2g2f', '8c8d', '2f2e', '8d8e', '8h7g', '3c3d', '7i8h', '2b7g+',
    '8h7g', '3a2b', '3g3f'
]

send("setoption name MultiPV value 5")
send(f"position startpos moves {' '.join(moves)}")
send("go btime 0 wtime 0 byoyomi 500")

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
