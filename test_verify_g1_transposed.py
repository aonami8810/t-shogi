import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open("g1_pure_suisho_2g2f_win.txt", "r") as f:
    g1_win = ast.literal_eval(f.read().strip())

# Transposed prefix:
# 1: 2g2f, 2: 8c8d, 3: 2f2e, 4: 4a3b, 5: 7g7f, 6: 8d8e, 7: 8h7g
prefix_4a3b = ['2g2f', '8c8d', '2f2e', '4a3b', '7g7f', '8d8e', '8h7g']
full_line = prefix_4a3b + g1_win[7:]

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

actual = []
diverged = False
print(f"Testing transposed Game 1 line against Suisho5 (total {len(full_line)} plies)...")

for ply in range(1, len(full_line) + 1):
    expected = full_line[ply - 1]
    if ply % 2 == 0:
        # Gote = Suisho5
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
            if ply % 10 == 0 or ply <= 15:
                print(f"Ply {ply:3d} [Suisho5 (Gote)]: {bm:6s} MATCHED")
            actual.append(bm)
    else:
        # Sente = Morse
        actual.append(expected)

if not diverged:
    print(f"\nALL {len(full_line)} PLIES MATCHED COMPLETELY! Checking ply {len(full_line)+1} (Suisho5 resign)...")
    send(f"position startpos moves {' '.join(actual)}")
    send("go btime 0 wtime 0 byoyomi 500")
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("bestmove"):
            print("Ply 122 Suisho5 move:", l)
            break

send("quit")
