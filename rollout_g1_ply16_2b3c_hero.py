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

# Game 1: Suisho (Gote) played 2b3c at ply 16 (after 14. 7a6b 15. 3i3h)
moves = ['2g2f', '8c8d', '7g7f', '8d8e', '2f2e', '4a3b', '8h7g', '3c3d', '7i8h', '2b7g+', '8h7g', '3a2b', '3g3f', '7a6b', '3i3h', '2b3c']

print("Rolling out Game 1 from ply 17 (Morse Sente turn after 16... 2b3c)...")
for ply in range(17, 200):
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

print("\nG1_PLY16_2B3C_HERO_WIN =", repr(moves))
with open("g1_ply16_2b3c_hero_win.txt", "w", encoding="utf-8") as f:
    f.write(repr(moves))

send("quit")
