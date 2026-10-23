import subprocess

MORSE_EXE = r"c:\VS\Workspace\t-shogi\shogi-usi-morse.exe"
MORSE_DIR = r"c:\VS\Workspace\t-shogi"

proc = subprocess.Popen(
    [MORSE_EXE],
    cwd=MORSE_DIR,
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True,
    encoding="utf-8",
    errors="replace"
)

def send(cmd):
    proc.stdin.write(cmd + "\n")
    proc.stdin.flush()

send("usi")
while True:
    line = proc.stdout.readline().strip()
    if line == "usiok":
        break

send("isready")
while True:
    line = proc.stdout.readline().strip()
    if line == "readyok":
        break

moves = "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d"
send(f"position startpos moves {moves}")
send("go btime 0 wtime 0 byoyomi 500")

while True:
    line = proc.stdout.readline()
    if not line:
        break
    line = line.strip()
    print("ENGINE:", line)
    if line.startswith("bestmove"):
        break

send("quit")
