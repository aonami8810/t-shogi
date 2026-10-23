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

moves = ['2g2f', '8c8d', '2f2e', '4a3b', '7g7f', '8d8e', '8h7g', '3c3d', '7i6h', '2b7g+', '6h7g', '3a2b', '6i7h', '2b3c', '4g4f', '7a6b']

print("Rollout from ply 17 after 7a6b...")
for ply in range(17, 130):
    bm, sc = ask(moves)
    role = "Morse (Sente)" if ply % 2 == 1 else "Suisho (Gote)"
    print(f"Ply {ply:3d} [{role:15s}]: {bm:6s} | {sc[:60]}")
    moves.append(bm)
    if bm == "resign":
        print(f"Resigned at ply {ply}!")
        break

print("\nALL_MOVES =", repr(moves))
send("quit")
