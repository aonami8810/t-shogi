import subprocess
from gote_perfect_mate import gote_mate

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi")
send("isready")
while "readyok" not in p.stdout.readline(): pass

actual = []
for ply in range(1, len(gote_mate) + 1):
    expected = gote_mate[ply - 1]
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
            print(f"--> Diverged at ply {ply}!")
            break
        actual.append(bm)
    else:
        # Gote = Morse's turn
        print(f"Ply {ply:3d} [Morse  ]: {expected:6s}")
        actual.append(expected)

send("quit")
