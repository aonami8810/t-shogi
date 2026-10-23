import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

def ask(moves):
    send(f"position startpos moves {' '.join(moves)}")
    send("go btime 0 wtime 0 byoyomi 500")
    bm, sc = None, ""
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("info") and "score" in l: sc = l
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    return bm, sc

# Actual moves up to ply 35 (Suisho played 7g3c+)
moves = ['2g2f', '8c8d', '7g7f', '3c3d', '6i7h', '8d8e', '2f2e', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'P*2b', 'R*8c', 'P*8b', '8c8e+', 'R*2g', '3i3h', '2g2d+', 'P*8c', '8a7c', '7g3c+']

print("Rollout from ply 36...")
for ply in range(36, 120):
    bm, sc = ask(moves)
    role = "Morse (Gote)" if ply % 2 == 0 else "Suisho (Sente)"
    print(f"Ply {ply:3d} [{role}]: {bm:6s} | {sc[:60]}")
    moves.append(bm)
    if bm == "resign":
        print(f"Resign at ply {ply}!")
        break

print("All moves:", repr(moves))
send("quit")
