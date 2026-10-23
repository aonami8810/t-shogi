import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open("g2_ply47_4f2d_full.txt", "r") as f:
    g2_4f2d = ast.literal_eval(f.read().strip())

# Transposition for prefix:
# ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h'] + g2_4f2d[7:]
prefix = ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h'] + g2_4f2d[7:]

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

actual = []
diverged = False
print(f"Testing g2_ply47_4f2d_full against Suisho5 (total {len(prefix)} plies)...")

for ply in range(1, len(prefix) + 1):
    expected = prefix[ply - 1]
    if ply % 2 == 1:
        # Sente = Suisho5
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
            if ply % 10 == 1 or ply <= 35:
                print(f"Ply {ply:3d} [Suisho5]: {bm:6s} MATCHED")
            actual.append(bm)
    else:
        # Gote = Morse
        actual.append(expected)

if not diverged:
    print(f"\nALL {len(prefix)} PLIES MATCHED COMPLETELY! Checking ply 99...")
    send(f"position startpos moves {' '.join(actual)}")
    send("go btime 0 wtime 0 byoyomi 500")
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("bestmove"):
            print("Ply 99 bestmove:", l)
            break

send("quit")
