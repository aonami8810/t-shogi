import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open("g2_ply45_8e3e_hero_full.txt", "r") as f:
    g2_hero = ast.literal_eval(f.read().strip())

# Start from Trial 2/3 prefix (15 plies)
prefix = ['2g2f', '8c8d', '7g7f', '3c3d', '6i7h', '8d8e', '2f2e', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d']
full_line = prefix + g2_hero[15:]

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

actual = list(prefix)
diverged = False
print(f"Testing from ply 16 to {len(full_line)}...")

for ply in range(16, len(full_line) + 1):
    expected = full_line[ply - 1]
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
            if ply % 10 == 1 or ply <= 30:
                print(f"Ply {ply:3d} [Suisho5]: {bm:6s} MATCHED")
            actual.append(bm)
    else:
        # Gote = Morse
        actual.append(expected)

if not diverged:
    print(f"\nALL {len(full_line)} PLIES MATCHED COMPLETELY! Checking ply 161...")
    send(f"position startpos moves {' '.join(actual)}")
    send("go btime 0 wtime 0 byoyomi 500")
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("bestmove"):
            print("Ply 161 bestmove:", l)
            break

send("quit")
