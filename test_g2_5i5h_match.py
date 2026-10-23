import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

with open("g2_5i5h_full.txt", "r") as f:
    g2_5i5h = ast.literal_eval(f.read().strip())

# g2_5i5h has 180 plies.
# Ply 1-14 in g2_5i5h:
# ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '5i5h', ...]
# Prefix with 2g2f:
prefix = ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '5i5h']
full_line = prefix + g2_5i5h[15:]

print(f"Testing g2_5i5h line (total {len(full_line)} plies)...")

actual = []
diverged = False
for ply in range(1, len(full_line) + 1):
    expected = full_line[ply - 1]
    if ply % 2 == 1:
        # Suisho5 (Sente)
        send(f"position startpos moves {' '.join(actual)}")
        send("go btime 0 wtime 0 byoyomi 500")
        bm = None
        while True:
            l = p.stdout.readline().strip()
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        if bm != expected:
            print(f"Diverged at ply {ply}: Suisho5 played {bm}, expected {expected}")
            diverged = True
            break
        else:
            if ply % 10 == 1 or ply <= 25:
                print(f"Ply {ply:3d} [Suisho5]: {bm:6s} MATCHED")
            actual.append(bm)
    else:
        # Morse (Gote)
        actual.append(expected)

if not diverged:
    print(f"\nALL {len(full_line)} PLIES MATCHED COMPLETELY! Checking final reply...")
    send(f"position startpos moves {' '.join(actual)}")
    send("go btime 0 wtime 0 byoyomi 500")
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("bestmove"):
            print("Final bestmove:", l)
            break

send("quit")
