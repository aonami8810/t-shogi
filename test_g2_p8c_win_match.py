import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open("g2_p8c_win_full.txt", "r") as f:
    g2_p8c_win = ast.literal_eval(f.read().strip())

# Transposition for 2g2f 8c8d 7g7f 3c3d:
# g2_p8c_win starts:
# ['2g2f', '3c3d', '7g7f', '8c8d', '6i7h', '8d8e', '2f2e', ...]
# If water artisan starts 2g2f and Morse plays 8c8d:
# ['2g2f', '8c8d', '7g7f', '3c3d', '6i7h', '8d8e', '2f2e', ...]
prefix_8c8d = ['2g2f', '8c8d', '7g7f', '3c3d', '6i7h', '8d8e', '2f2e'] + g2_p8c_win[7:]

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

actual = []
diverged = False
print(f"Testing g2_p8c_win against Suisho5 (total {len(prefix_8c8d)} plies)...")

for ply in range(1, len(prefix_8c8d) + 1):
    expected = prefix_8c8d[ply - 1]
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
            if ply % 10 == 1 or ply <= 25:
                print(f"Ply {ply:3d} [Suisho5]: {bm:6s} MATCHED")
            actual.append(bm)
    else:
        # Gote = Morse
        actual.append(expected)

if not diverged:
    print(f"\nALL {len(prefix_8c8d)} PLIES MATCHED! Checking ply {len(prefix_8c8d)+1}...")
    send(f"position startpos moves {' '.join(actual)}")
    send("go btime 0 wtime 0 byoyomi 500")
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("bestmove"):
            print("Final bestmove:", l)
            break

send("quit")
