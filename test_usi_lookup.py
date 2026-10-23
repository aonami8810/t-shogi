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
    encoding="utf-8"
)

def query(moves):
    proc.stdin.write(f"position startpos moves {' '.join(moves)}\n")
    proc.stdin.write("go btime 0 wtime 0 byoyomi 500\n")
    proc.stdin.flush()
    while True:
        line = proc.stdout.readline()
        if not line:
            break
        line = line.strip()
        print("  [OUT]", line)
        if line.startswith("bestmove"):
            break

print("=== Test Game 1: 11th move after 8b8f ===")
m1 = ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f"]
query(m1)

print("\n=== Test Game 2: 24th move after 7g3c+ ===")
m2 = ["2g2f", "8c8d", "2f2e", "8d8e", "7g7f", "3c3d", "6i7h", "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b", "2d3d", "2b8h+", "7i8h", "2a3c", "B*7g", "8f7f", "8h8g", "7f2f", "7g3c+"]
query(m2)

proc.stdin.write("quit\n")
proc.stdin.flush()
proc.wait()
