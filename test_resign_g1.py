import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(cmd): p.stdin.write(cmd + "\n"); p.stdin.flush()

send("usi")
send("isready")
while True:
    if "readyok" in p.stdout.readline(): break

m = ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'P*2b', 'R*8c', 'P*8b', '8c8e+', '3c5a', '2d2c+', '3b2c', '8e3e', 'P*3d', '3e2f', '5a7c', 'P*2h', '2c3c', '3f3e', '4c4d', '3e3d', '3c4c', '7i6h', '7a6b', '2f7f', '5c5d', '7h7i', '3a4b', '7f2f', '4b3a', '1g1f', '6b5c', '1f1e', '7c6d', '3i4h', '7d7e', '9g9f', '5d5e', '7g8f', 'R*2c', '2f3g', '5e5f', '4g4f', '4d4e', '4i3i', '4e4f', '5g5f', '3a4b', '8f7g', '5c4d', '6h5g', '4d4e', '7g6h', 'P*3f', '3g3h', '7e7f', 'P*4g', '4f4g+', '3h4g', '4c3d', 'P*4f', '4e5d', '5f5e', '5d5e', '4g3f', 'P*3e', '3f4g', '4b4c', '5g5f', '5b4a', '5f6e', '6d4b', '5h6i', '6a5b', '6i7h', '8a7c', '6e7f', '4b3c', 'P*7d', 'P*7e', '7f7e', '7c6e', '7d7c+', 'P*4e', '4f4e', 'P*4f', '4g3h', '3d4e', '2i3g', '4e4d', '6g6f', '5e5f', '6f6e', '3e3f', '6h8f', '3f3g+', '3h3g', 'P*3f', '3g8g', '2c2e', '7e7d', '5b4b', '7c6c', '5f6e', '7d6e', '2e6e', '6c5c', 'N*7e', '5c4b', '3c4b', '8f7e', '6e7e', 'P*7f', '7e3e', 'N*2g', '3e2e', 'P*5c', 'B*4e', '7h8h', '4b5c', 'N*6e', '5c6d', 'S*5c', '6d5e', 'P*6f', '4a3b', 'P*5f', '4e5f', '8g5g', 'S*4e', '5c4d+', '5e4d', 'G*5c', '4c3d', 'P*3e', '5f6e', '3e3d', 'P*5f', '5g8g', 'N*7e', 'G*4c', '6e4c', '5c4c', '3b4c', '7f7e', '4d6f', 'B*7g', '6f7g+', '8i7g', 'P*7f', '8g7f', '5f5g+', '4h5g', 'B*3b', 'N*3e', '4c3d', 'S*4c', '3b4c', '3e4c+', '3d4c', 'B*1f', 'G*3d', '1f2e', '3d2e', 'P*4d']

for ply in range(186, 210):
    send(f"position startpos moves {' '.join(m)}")
    send("go btime 0 wtime 0 byoyomi 500")
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("info") and "score" in l:
            sc = l
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    turn = "Suisho5" if (ply % 2 == 0) else "Morse"
    print(f"Ply {ply:3d} [{turn}]: {bm} | {sc}")
    if bm == "resign":
        print(f"SUISHO5 RESIGNED AT PLY {ply}!")
        break
    m.append(bm)

send("quit")
