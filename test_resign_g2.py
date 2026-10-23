import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(cmd): p.stdin.write(cmd + "\n"); p.stdin.flush()

send("usi")
send("isready")
while True:
    if "readyok" in p.stdout.readline(): break

m = ['7g7f', '8c8d', '8h7g', '3c3d', '7i6h', '2b7g+', '6h7g', '3a2b', '3i3h', '2b3c', '3g3f', '7a6b', '5i6h', '4a3b', '4g4f', '1c1d', '1g1f', '9c9d', '9g9f', '6c6d', '3h4g', '7c7d', '6i7h', '6b6c', '2i3g', '8a7c', '4i4h', '6a6b', '2g2f', '5a4b', '2h2i', '4c4d', '6g6f', '8b8a', '6h7i', '6c5d', '4g5f', '4b3a', '2f2e', '8a4a', '7g6h', '8d8e', '8i7g', '4a8a', '4f4e', '8a4a', '4e4d', '4a4d', 'P*4e', '4d4a', 'B*4f', '6b6c', '6h6g', '4a8a', '7i6h', '8e8f', '8g8f', '8a8f', 'P*8g', '8f8a', '6h5h', '3a2b', '5h6h', 'P*8f', '8g8f', '8a8f', 'P*8g', '8f8a', '6h5h', '9d9e', '2e2d', '3c2d', '9f9e', 'P*9g', '4e4d', '8a4a', '9i9g', '4a4d', 'P*4e', '4d4a', '9e9d', '3d3e', 'P*2e', '2d3c']

for ply in range(85, 140):
    if ply % 2 == 1:
        # 先手 (水匠5) byoyomi 500ms
        send(f"position startpos moves {' '.join(m)}")
        send("go btime 0 wtime 0 byoyomi 500")
        while True:
            l = p.stdout.readline().strip()
            if l.startswith("info") and "score" in l: sc = l
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        print(f"Ply {ply:3d} [Suisho5]: {bm} | {sc}")
        if bm == "resign":
            print(f"SUISHO5 RESIGNED AT PLY {ply}!")
            break
        m.append(bm)
    else:
        # 後手 (Morse) depth 16
        send(f"position startpos moves {' '.join(m)}")
        send("go depth 16")
        while True:
            l = p.stdout.readline().strip()
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        print(f"Ply {ply:3d} [Morse  ]: {bm}")
        m.append(bm)

send("quit")
print("G2 ALL MOVES: " + repr(m))
