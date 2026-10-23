import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

moves_105 = [
    "7g7f", "8c8d", "2g2f", "4a3b", "2f2e", "8d8e", "8h7g", "3c3d",
    "7i6h", "2b7g+", "6h7g", "3a2b", "3i3h", "2b3c", "3g3f", "7c7d",
    "3h3g", "7a6b", "6i7h", "6b7c", "3g4f", "1c1d", "5i5h", "9c9d",
    "1g1f", "9d9e", "2h2f", "7c6d", "4i3h", "5a5b", "2i3g", "8a7c",
    "3f3e", "3d3e", "4f3e", "8b8a", "2e2d", "3c2d", "3e2d", "2c2d",
    "2f2d", "P*2c", "2d3d", "2a3c", "7f7e", "6d7e", "S*2a", "3b4b",
    "B*2b", "7c6e", "2b1a+", "P*3f", "3d3f", "B*2d", "7g6h", "S*3e",
    "3f3e", "2d3e", "P*3d", "P*3f", "3d3c+", "3f3g+", "3c4b", "5b6b",
    "3h3g", "N*4e", "G*7c", "6b7c", "1a5e", "7c8c", "5e4e", "R*3i",
    "S*4h", "3i8i+", "7h7i", "8i9i", "N*7c", "G*7g", "L*5i", "7g6h",
    "7i6h", "N*7f", "4g4f", "7f6h+", "5h4g", "8c7c", "N*5e", "6a7b",
    "4e3e", "G*4d", "3e2e", "S*3d", "2e1d", "8a2a", "P*3e", "N*2e",
    "3g3f", "6h5i", "3e3d", "5i4i", "S*8b", "7c8b", "4h3g", "6e5g+",
    "4g5g"
]

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(cmd): p.stdin.write(cmd + "\n"); p.stdin.flush()

send("usi")
send("isready")
while "readyok" not in p.stdout.readline(): pass

print("Checking Suisho5 (byoyomi 500ms) on all Sente turns...")
matched = True
actual_moves = []
for ply in range(1, len(moves_105) + 1):
    expected = moves_105[ply - 1]
    if ply % 2 == 1:
        # Suisho5's turn (Sente)
        send(f"position startpos moves {' '.join(actual_moves)}")
        send("go btime 0 wtime 0 byoyomi 500")
        bm, sc = None, None
        while True:
            l = p.stdout.readline().strip()
            if l.startswith("info") and "score" in l: sc = l
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        if bm != expected:
            print(f"DIVERGENCE at ply {ply}: Suisho5 played {bm}, expected {expected} | {sc}")
            matched = False
            actual_moves.append(bm)
            break
        else:
            print(f"Ply {ply:3d} [Suisho5]: {bm} MATCH")
            actual_moves.append(bm)
    else:
        # Morse's turn (Gote)
        actual_moves.append(expected)

send("quit")
if matched:
    print("ALL 53 SENTE MOVES MATCHED PERFECTLY!")
