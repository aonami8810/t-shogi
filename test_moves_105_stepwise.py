import subprocess
from test_mate23 import moves_105

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi")
send("isready")
while "readyok" not in p.stdout.readline(): pass
send("usinewgame")

actual = []
all_matched = True
for ply in range(1, len(moves_105) + 1):
    expected = moves_105[ply - 1]
    if ply % 2 == 1:
        # Sente = Suisho5's turn
        pos_cmd = f"position startpos moves {' '.join(actual)}\n" if actual else "position startpos\n"
        send(pos_cmd.strip())
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
    print(f"\nALL 105 MOVES MATCHED! Testing mate from ply 106...")

send("quit")
