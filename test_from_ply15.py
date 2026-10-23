import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

moves = ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '5i5h']

print("Moves at ply 15:", " ".join(moves))

# Check Gote's best reply at ply 16
send(f"position startpos moves {' '.join(moves)}")
send("go btime 0 wtime 0 byoyomi 500")
while True:
    l = p.stdout.readline().strip()
    if l.startswith("info") and "score" in l:
        sc = l
    if l.startswith("bestmove"):
        bm16 = l.split()[1]
        print(f"Ply 16 Gote bestmove: {bm16} | {sc}")
        break

moves.append(bm16)

# Check Suisho5's reply at ply 17
send(f"position startpos moves {' '.join(moves)}")
send("go btime 0 wtime 0 byoyomi 500")
while True:
    l = p.stdout.readline().strip()
    if l.startswith("info") and "score" in l:
        sc = l
    if l.startswith("bestmove"):
        bm17 = l.split()[1]
        print(f"Ply 17 Suisho5 bestmove: {bm17} | {sc}")
        break

send("quit")
