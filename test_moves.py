import subprocess

MORSE_EXE = r"c:\VS\Workspace\t-shogi\shogi-usi-morse.exe"
MORSE_DIR = r"c:\VS\Workspace\t-shogi"

p = subprocess.Popen([MORSE_EXE], cwd=MORSE_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8")
p.stdin.write("usi\n")
p.stdin.write("isready\n")
p.stdin.flush()
while True:
    if "readyok" in p.stdout.readline():
        break

moves13 = ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d']
p.stdin.write(f"position startpos moves {' '.join(moves13)}\n")
p.stdin.write("go btime 0 wtime 0 byoyomi 1000\n")
p.stdin.flush()
while True:
    line = p.stdout.readline().strip()
    print("13 moves response:", line)
    if line.startswith("bestmove"):
        break

moves15 = moves13 + ['P*2c', '2d2c+']
p.stdin.write(f"position startpos moves {' '.join(moves15)}\n")
p.stdin.write("go btime 0 wtime 0 byoyomi 1000\n")
p.stdin.flush()
while True:
    line = p.stdout.readline().strip()
    print("15 moves response:", line)
    if line.startswith("bestmove"):
        break

p.stdin.write("quit\n")
p.stdin.flush()
p.terminate()
