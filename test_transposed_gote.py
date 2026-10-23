import subprocess
from gote_perfect_mate import gote_mate

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

# We swap moves 2 and 4:
# Ply 2: 3c3d (instead of 8c8d)
# Ply 3: Suisho5 should play 7g7f
# Ply 4: 8c8d (instead of 3c3d)
# From ply 5 onwards, identical to gote_mate!
new_gote_mate = list(gote_mate)
new_gote_mate[1] = "3c3d" # ply 2
new_gote_mate[2] = "7g7f" # ply 3 (Suisho5)
new_gote_mate[3] = "8c8d" # ply 4

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi")
send("isready")
while "readyok" not in p.stdout.readline(): pass

actual = []
all_matched = True
for ply in range(1, len(new_gote_mate) + 1):
    expected = new_gote_mate[ply - 1]
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
            break
        actual.append(bm)
    else:
        # Gote = Morse's turn
        print(f"Ply {ply:3d} [Morse  ]: {expected:6s}")
        actual.append(expected)

send("quit")
if all_matched:
    print(f"\nSUCCESS! All {len(new_gote_mate)} plies matched Suisho5!")
