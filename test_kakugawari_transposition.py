import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(cmd): p.stdin.write(cmd + "\n"); p.stdin.flush()

send("usi")
send("isready")
while True:
    if "readyok" in p.stdout.readline(): break

moves = ['7g7f', '8c8d', '8h7g', '3c3d', '7i6h', '2b7g+', '6h7g', '3a2b', '3i3h', '2b3c', '3g3f', '7a6b', '5i6h', '4a3b', '4g4f', '1c1d', '1g1f', '9c9d', '9g9f', '6c6d', '3h4g', '7c7d', '6i7h', '6b6c', '2i3g', '8a7c', '4i4h', '6a6b', '2g2f', '5a4b', '2h2i', '4c4d', '6g6f', '8b8a', '6h7i', '6c5d', '4g5f', '4b3a', '2f2e']

for ply in range(40, 85):
    if ply % 2 == 1:
        send(f"position startpos moves {' '.join(moves)}")
        send("go btime 0 wtime 0 byoyomi 500")
        while True:
            l = p.stdout.readline().strip()
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        print(f"Ply {ply:2d} [Suisho5]: {bm}")
        if bm == "resign": break
        moves.append(bm)
    else:
        send(f"position startpos moves {' '.join(moves)}")
        send("go depth 16")
        while True:
            l = p.stdout.readline().strip()
            if l.startswith("info depth") and "score" in l:
                sc = l.split()[l.split().index("score")+2]
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        print(f"Ply {ply:2d} [Morse  ]: {bm} (eval: {sc})")
        moves.append(bm)

send("quit")
print("ALL MOVES: " + repr(moves))
