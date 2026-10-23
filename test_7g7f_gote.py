import subprocess
from gote_perfect_mate import gote_mate

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

# When Suisho5 plays 7g7f at ply 1:
# Ply 1: 7g7f (Suisho5)
# Ply 2: 3c3d (Morse)
# Ply 3: 2g2f (Suisho5)
# Ply 4: 8c8d (Morse)
# Ply 5+: identical to gote_mate[4:]
line_7g7f = ["7g7f", "3c3d", "2g2f", "8c8d"] + gote_mate[4:]

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi")
send("isready")
while "readyok" not in p.stdout.readline(): pass

actual = []
all_matched = True
for ply in range(1, len(line_7g7f) + 1):
    expected = line_7g7f[ply - 1]
    if ply % 2 == 1:
        # Sente = Suisho5's turn
        send(f"position startpos moves {' '.join(actual)}")
        send("go btime 0 wtime 0 byoyomi 500")
        bm = None
        while True:
            l = p.stdout.readline().strip()
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        print(f"Ply {ply:3d} [Suisho5]: {bm:6s} (expected: {expected})")
        if bm != expected:
            print(f"--> Diverged at ply {ply}: Suisho5 played {bm}, expected {expected}!")
            all_matched = False
            actual.append(bm)
            break
        actual.append(bm)
    else:
        # Gote = Morse's turn
        print(f"Ply {ply:3d} [Morse  ]: {expected:6s}")
        actual.append(expected)

if all_matched:
    print(f"\nALL 119 MOVES MATCHED! Testing reply to ply 119...")
    send(f"position startpos moves {' '.join(actual)}")
    send("go btime 0 wtime 0 byoyomi 500")
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("bestmove"):
            print("Ply 120 Suisho5 final reply:", l)
            break

send("quit")
