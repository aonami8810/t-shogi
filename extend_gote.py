import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

moves = ["2g2f", "8c8d", "7g7f", "3c3d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b", "2d3d", "2b3c", "5i5h", "5a5b", "3g3f", "8f7f", "8i7g", "3c5e", "P*2b", "2a3c", "2b2a+", "3a4b", "3d8d", "5e1i+", "8d8a+", "3c4e", "2a2b", "3b2b", "N*3d", "2b3b", "8a8e", "4e5g+", "5h5g", "L*5e", "5g4h", "7f3f", "N*4f", "P*8g", "7h8g", "P*8f", "8g8f", "1i2i", "8e5e", "2i3i", "4i3i", "S*3g", "4h4i", "P*3h", "3i2i", "P*8g", "8f8g", "N*3e", "4i5h", "3e4g+", "5h4g", "3g4f", "5e4f", "3f4f", "4g4f", "R*2f"]

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8")
p.stdin.write("usi\n")
p.stdin.write("isready\n")
p.stdin.flush()
while True:
    if "readyok" in p.stdout.readline():
        break

for ply in range(65, 85):
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
        print(f"Game over at ply {ply}: {bestmove}")
        break
    moves.append(bestmove)
    turn = "Sente" if ply % 2 == 1 else "Gote"
    print(f"Ply {ply:2d} ({turn}): {bestmove:6s} (eval: {score})")

print("\nFull Branch 2 extended:")
print('["' + '", "'.join(moves) + '"]')
p.stdin.write("quit\n")
p.stdin.flush()
p.terminate()
