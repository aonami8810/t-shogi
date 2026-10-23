import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open("g1_pure_suisho_2g2f_win.txt", "r") as f:
    g1_win = ast.literal_eval(f.read().strip())

perfect_g1_line = [
    '2g2f', '8c8d', '2f2e', '4a3b', '7g7f', '8d8e', '8h7g', '3c3d',
    '7i6h', '2b7g+', '6h7g', '3a2b', '6i7h', '2b3c', '4g4f', '7c7d',
    '3i3h', '5a4b', '3g3f', '7a7b'
] + g1_win[20:]

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

# Start from actual moves with 8c8d at ply 2
actual = ['2g2f', '8c8d']
diverged = False
print(f"Testing perfect_g1_line from ply 3 to {len(perfect_g1_line)}...")

for ply in range(3, len(perfect_g1_line) + 1):
    expected = perfect_g1_line[ply - 1]
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
            if ply % 10 == 0 or ply <= 22 or ply >= 110:
                print(f"Ply {ply:3d} [Suisho5 (Gote)]: {bm:6s} MATCHED")
            actual.append(bm)
    else:
        # Sente = Morse
        actual.append(expected)

if not diverged:
    print(f"\n>>> ALL {len(perfect_g1_line)} PLIES MATCHED COMPLETELY! Checking ply 122 (Suisho5 resign)... <<<")
    send(f"position startpos moves {' '.join(actual)}")
    send("go btime 0 wtime 0 byoyomi 500")
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("bestmove"):
            print("Ply 122 Suisho5 move:", l)
            break

send("quit")
