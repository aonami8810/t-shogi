import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

def ask(moves, byo=1000):
    send(f"position startpos moves {' '.join(moves)}")
    send(f"go btime 0 wtime 0 byoyomi {byo}")
    bm, sc = None, ""
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("info") and "score" in l: sc = l
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    return bm, sc

# Game 1: Suisho (Gote) played P*2f at ply 60
moves = ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'R*2f', '3i3h', 'P*2h', '7g3c+', '2a3c', 'B*3g', '2f2d', '3g9a+', '2h2i+', '9a8a', '2i2h', 'N*7c', '6a7b', '7c6a+', '3a4b', '3h3g', '2h3h', '4i3h', '2d2i+', 'R*3i', '2i2a', 'P*7c', '2a6a', '7c7b+', '7a7b', '8a8b', 'B*7c', 'P*8c', 'P*8a', '8b7c', '7b7c', '3i2i', 'N*4e', '3g4f', 'P*2f']

print("Rolling out Game 1 from ply 61 (Morse Sente turn after 60... P*2f)...")
for ply in range(61, 200):
    is_morse = (ply % 2 == 1) # Sente = Morse
    byo = 1500 if is_morse else 500
    bm, sc = ask(moves, byo=byo)
    role = "Morse (Sente)" if is_morse else "Suisho (Gote)"
    print(f"Ply {ply:3d} [{role:14s}]: {bm:6s} | {sc[:60]}", flush=True)
    moves.append(bm)
    if bm == "resign":
        winner = "Morse" if not is_morse else "Suisho"
        print(f"\n>>> TERMINATED! Winner: {winner} at ply {ply}! <<<", flush=True)
        break

print("\nG1_PLY60_P2F_WIN =", repr(moves))
with open("g1_ply60_p2f_win.txt", "w", encoding="utf-8") as f:
    f.write(repr(moves))

send("quit")
