import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open("g2_p2d_r2e_full.txt", "r") as f:
    g2_p2d_r2e = ast.literal_eval(f.read().strip())

# Actual moves up to ply 41:
actual_41 = "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 5i5h 8f7f 8h2b+ 3a2b 2d3d 2b3c 3d3f 7f3f 3g3f P*8b 3i3h 5a5b B*4f P*2g P*2d R*2e 2i3g 2e2f 2d2c+ 2f2c P*2d 3c2d P*2e 2d2e 3h2g 2e3d 4i3h".split()

# If Morse plays 4c4d at ply 42:
# In g2_p2d_r2e, moves 41-43 were: P*2f, 4c4d, 4i3h.
# Here, 41 is 4i3h, so 42 is 4c4d, 43 is P*2f.
# Then from ply 44 onwards, it matches g2_p2d_r2e[43:]!
transposed_g2_line = actual_41 + ['4c4d', 'P*2f'] + g2_p2d_r2e[43:]

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

actual = list(actual_41)
diverged = False
print(f"Testing transposed Game 2 line from ply 42 onwards...")

for ply in range(42, len(transposed_g2_line) + 1):
    expected = transposed_g2_line[ply - 1]
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
            if ply % 10 == 1 or ply <= 50:
                print(f"Ply {ply:3d} [Suisho5]: {bm:6s} MATCHED")
            actual.append(bm)
    else:
        # Gote = Morse
        actual.append(expected)

if not diverged:
    print(f"\nALL PLIES MATCHED COMPLETELY! Checking final reply...")
    send(f"position startpos moves {' '.join(actual)}")
    send("go btime 0 wtime 0 byoyomi 500")
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("bestmove"):
            print("Final reply:", l)
            break

send("quit")
