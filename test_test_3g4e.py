import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

from generate_all_transpositions import g2_3g4e_2a3c_107

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

actual = []
diverged = False
print(f"Testing g2_3g4e_2a3c_107 against Suisho5 (total {len(g2_3g4e_2a3c_107)} plies)...")

for ply in range(1, len(g2_3g4e_2a3c_107) + 1):
    expected = g2_3g4e_2a3c_107[ply - 1]
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
    print(f"\nALL {len(g2_3g4e_2a3c_107)} PLIES MATCHED COMPLETELY! Checking ply 107 (Suisho5 resign)...")
    send(f"position startpos moves {' '.join(actual)}")
    send("go btime 0 wtime 0 byoyomi 500")
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("bestmove"):
            print("Ply 107 bestmove:", l)
            break

send("quit")
