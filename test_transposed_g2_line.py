import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi")
send("isready")
while "readyok" not in p.stdout.readline(): pass

with open("g2_ply45_8e3e_hero_full.txt", "r") as f:
    g2_hero = ast.literal_eval(f.read().strip())

# g2_hero begins:
# ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', ...]
# If transposed:
# ['2g2f', '8c8d', '7g7f', '3c3d', '6i7h', '8d8e', ...]

transposed_g2 = ['2g2f', '8c8d', '7g7f', '3c3d', '6i7h', '8d8e'] + g2_hero[6:]

print(f"Testing transposed Game 2 line (total {len(transposed_g2)} plies)...")

actual = []
diverged = False
for ply in range(1, len(transposed_g2) + 1):
    expected = transposed_g2[ply - 1]
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
            if ply % 20 == 1 or ply <= 15:
                print(f"Ply {ply:3d} [Suisho5]: {bm:6s} MATCHED")
            actual.append(bm)
    else:
        # Morse (Gote)
        actual.append(expected)

if not diverged:
    print(f"\nALL {len(transposed_g2)} PLIES MATCHED COMPLETELY! Checking ply 161 Suisho5 reply...")
    send(f"position startpos moves {' '.join(actual)}")
    send("go btime 0 wtime 0 byoyomi 500")
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("bestmove"):
            print("Ply 161 bestmove:", l)
            break

send("quit")
