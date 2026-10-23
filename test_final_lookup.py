import subprocess

MORSE_EXE = r"c:\VS\Workspace\t-shogi\shogi-usi-morse.exe"
MORSE_DIR = r"c:\VS\Workspace\t-shogi"

proc = subprocess.Popen(
    [MORSE_EXE], cwd=MORSE_DIR,
    stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
    text=True, encoding="utf-8"
)

def query(moves):
    proc.stdin.write(f"position startpos moves {' '.join(moves)}\n")
    proc.stdin.write("go btime 0 wtime 0 byoyomi 500\n")
    proc.stdin.flush()
    bm = None
    is_book = False
    while True:
        l = proc.stdout.readline()
        if not l: break
        l = l.strip()
        if "book hit" in l:
            is_book = True
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    return bm, is_book

cases = [
    ("Game 1 (3c3d -> 2e2d)", ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f"]),
    ("Game 1 (4a3b -> 2f2e)", ["7g7f", "8c8d", "2g2f", "4a3b"]),
    ("Game 1 (4a3b, 8d8e -> 8h7g)", ["7g7f", "8c8d", "2g2f", "4a3b", "2f2e", "8d8e"]),
    ("Game 2 (7i7h / 6i7h -> 3b3c)", ["2g2f", "8c8d", "2f2e", "8d8e", "7g7f", "3c3d", "6i7h", "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b", "2d3d", "2b8h+", "7i8h", "2a3c", "B*7g", "8f7f", "8h8g", "7f2f", "7g3c+"]),
    ("Game 2 (5i5h -> 2b8h+)", ["2g2f", "8c8d", "2f2e", "8d8e", "7g7f", "3c3d", "5i5h"]),
]

for label, m in cases:
    bm, book = query(m)
    print(f"[{'PASS' if book else 'FAIL'}] {label:30s} -> {bm:6s} (book={book})")

proc.stdin.write("quit\n")
proc.stdin.flush()
proc.wait()
