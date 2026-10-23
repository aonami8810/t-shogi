import subprocess
from generate_final_highways import g1_156

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

actual = []
diverged = False
print(f"Testing g1_156 against Suisho5 (total {len(g1_156)} plies)...")

for ply in range(1, len(g1_156) + 1):
    expected = g1_156[ply - 1]
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
            if ply % 20 == 0 or ply <= 30:
                print(f"Ply {ply:3d} [Suisho5 (Gote)]: {bm:6s} MATCHED")
            actual.append(bm)
    else:
        # Sente = Morse
        actual.append(expected)

if not diverged:
    print(f"\nALL {len(g1_156)} PLIES MATCHED COMPLETELY! Checking ply 157 (Suisho5 resign)...")
    send(f"position startpos moves {' '.join(actual)}")
    send("go btime 0 wtime 0 byoyomi 500")
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("bestmove"):
            print("Ply 157 Suisho5 move:", l)
            break

send("quit")
