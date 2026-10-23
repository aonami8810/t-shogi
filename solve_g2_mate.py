import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

moves = ['7g7f', '8c8d', '8h7g', '3c3d', '7i7h', '7a6b', '3i4h', '2b7g+', '7h7g', '3a2b', '1g1f', '1c1d', '5i6h', '7c7d', '6i7h', '6c6d', '9g9f', '9c9d', '4g4f', '6b6c', '4h4g', '2b3c', '3g3f', '4a3b', '2i3g', '8a7c', '2h2i', '8b8a', '4i4h', '6a6b', '4g5f', '5a6a', '2g2f', '5c5d', '2f2e', '6a7b', '6g6f', '3c4d', '2e2d', '2c2d', '2i2d', '3d3e', '3f3e', 'P*2f', '9f9e', '9d9e', 'P*9c', '9a9c', '7f7e', '8a9a', 'P*9d', '9c9d', '7e7d', '6c7d', 'P*9c', '7d8c', '5f4e', 'B*1c', '2d1d', '3b2c', '1d1e', '2c2d', 'P*7d', '2d1e', '7d7c+', '7b7c', '1f1e', '4d5c', '4e3d', '5c4b', '6f6e', '2f2g+', 'B*8f', 'P*7e', '8f7e', '7c8b', '7e6d', 'P*7c', 'N*7e', 'P*6g', '6h6g', '2g3g', '7e8c+', '8b8c', 'S*9b', '9a9b', '9c9b+', '8c9b', 'P*9c', '9b9c', 'R*8a', 'S*9b', '8a9a+', 'R*3a', 'G*7a', '3a7a', '9a7a', 'G*7b', '7a9a', '3g4h', '9i9e', '9d9e', 'P*9d', '9c8c', 'R*9c', '8c7d', '6d4b+', 'N*7e', '4b7e', '7d7e', '7g7f', '7e7d', 'P*7e', '7d6c', 'S*6d', '6c5b', 'N*4d', '4c4d', '9c9b+', 'B*4i', '6g7g', '4i7f+', '7g7f', 'G*8e', '7f7g', 'S*7f', '7g8h', '9e9h+', '8h9h', 'L*9f', '9h8h', '9f9h+', '8h9h', 'N*8f', '8g8f', 'P*9g', '8i9g', '7f8g+', '7h8g']

send(f"position startpos moves {' '.join(moves)}")
send("go btime 0 wtime 0 byoyomi 5000")

while True:
    l = p.stdout.readline().strip()
    if l.startswith("info") and "mate" in l:
        print(l)
    if l.startswith("bestmove"):
        print("Bestmove at ply 140:", l)
        break

send("quit")
