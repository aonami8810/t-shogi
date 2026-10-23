import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

moves = ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "7a6b", "6i7h", "8c8d", "8h7g", "7c7d", "2g2f", "6b7c", "3g3f", "8d8e", "3i4h", "2b3c", "2f2e", "4a3b", "4h3g", "5a5b", "1g1f", "7c6d", "3g4f", "1c1d", "9g9f", "9c9d", "2i3g", "4c4d", "4i4h", "5b4b", "2h2f", "6a5b", "5i5h", "5b4c", "7g6f", "4b3a", "3f3e", "3d3e", "4f3e", "P*3f", "2f3f", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "P*3d", "3c2b", "3f2f", "B*5d", "2e2d", "2c2d", "3e2d", "P*2c", "2d3e", "8b8e", "2f2e", "5d7f", "6f7g", "7f5d", "7g8f", "8e8b", "2e2f", "P*3f", "3g2e", "2c2d", "2e3c+", "2a3c", "3d3c+", "2b3c", "B*6a", "5d4e", "N*5f", "8b6b"]

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8")
p.stdin.write("usi\n")
p.stdin.write("isready\n")
p.stdin.flush()
while True:
    if "readyok" in p.stdout.readline():
        break

for ply in range(75, 95):
    p.stdin.write(f"position startpos moves {' '.join(moves)}\n")
    p.stdin.write("go btime 0 wtime 0 byoyomi 800\n")
    p.stdin.flush()
    bestmove = None
    score = None
    while True:
        line = p.stdout.readline().strip()
        if line.startswith("info") and "score" in line and "pv" in line:
            parts = line.split()
            if "cp" in parts:
                score = parts[parts.index("cp") + 1]
            elif "mate" in parts:
                score = "mate " + parts[parts.index("mate") + 1]
        if line.startswith("bestmove"):
            bestmove = line.split()[1]
            break
    if bestmove in ["resign", "win", None]:
        break
    moves.append(bestmove)
    turn = "Sente" if ply % 2 == 1 else "Gote"
    print(f"Ply {ply:2d} ({turn}): {bestmove:6s} (eval: {score})")

print("\nFull line extended:")
print('["' + '", "'.join(moves) + '"]')
p.stdin.write("quit\n")
p.stdin.flush()
p.terminate()
