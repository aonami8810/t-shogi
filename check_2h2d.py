import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8")
p.stdin.write("usi\n")
p.stdin.write("setoption name MultiPV value 5\n")
p.stdin.write("isready\n")
p.stdin.flush()
while True:
    if "readyok" in p.stdout.readline():
        break

moves = [
    "2g2f", "8c8d", "2f2e", "8d8e", "7g7f", "3c3d", "6i7h",
    "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d"
]
p.stdin.write(f"position startpos moves {' '.join(moves)}\n")
p.stdin.write("go btime 0 wtime 0 byoyomi 3000\n")
p.stdin.flush()
while True:
    line = p.stdout.readline()
    if not line:
        break
    if "multipv" in line:
        print(line.strip())
    if line.startswith("bestmove"):
        print(line.strip())
        break
p.stdin.write("quit\n")
p.stdin.flush()
p.terminate()
