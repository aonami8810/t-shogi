import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open("g1_ply32_p8d_hero_full.txt", "r") as f:
    g1_hero = ast.literal_eval(f.read().strip())

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

actual = []
diverged = False
print(f"Testing g1_ply32_p8d_hero against Suisho5 (total {len(g1_hero)} plies)...")

for ply in range(1, len(g1_hero) + 1):
    expected = g1_hero[ply - 1]
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
            if ply % 10 == 0 or ply <= 16:
                print(f"Ply {ply:3d} [Suisho5 (Gote)]: {bm:6s} MATCHED")
            actual.append(bm)
    else:
        # Sente = Morse
        actual.append(expected)

if not diverged:
    print(f"\nALL {len(g1_hero)} PLIES MATCHED COMPLETELY! Checking ply 116 (Suisho5 resign)...")
    send(f"position startpos moves {' '.join(actual)}")
    send("go btime 0 wtime 0 byoyomi 500")
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("bestmove"):
            print("Ply 116 Suisho5 move:", l)
            break

send("quit")
