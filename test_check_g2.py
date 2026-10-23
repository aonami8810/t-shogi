import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi")
send("isready")
while "readyok" not in p.stdout.readline(): pass

# Test g2_ply45_8e3e_hero_full.txt
with open("g2_ply45_8e3e_hero_full.txt", "r") as f:
    import ast
    g2_hero = ast.literal_eval(f.read().strip())

print(f"Total plies in g2_hero: {len(g2_hero)}")

actual = []
for ply in range(1, len(g2_hero) + 1):
    expected = g2_hero[ply - 1]
    if ply % 2 == 1:
        # Sente = Suisho5
        pos = f"position startpos moves {' '.join(actual)}" if actual else "position startpos"
        send(pos)
        send("go btime 0 wtime 0 byoyomi 500")
        bm = None
        while True:
            l = p.stdout.readline().strip()
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        if bm != expected:
            print(f"Divergence at ply {ply}: Suisho5 played {bm} instead of expected {expected}!")
            actual.append(bm)
            break
        else:
            if ply % 10 == 1 or ply < 10:
                print(f"Ply {ply:3d} [Suisho5]: {bm} MATCHED")
            actual.append(bm)
    else:
        # Gote = Morse
        actual.append(expected)

if len(actual) == len(g2_hero):
    print("ALL PLIES MATCHED COMPLETELY! Checking ply 161 (Suisho5 resign)...")
    send(f"position startpos moves {' '.join(actual)}")
    send("go btime 0 wtime 0 byoyomi 500")
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("bestmove"):
            print("Ply 161 Suisho5 move:", l)
            break

send("quit")
