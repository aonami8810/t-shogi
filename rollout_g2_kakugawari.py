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

# Game 2: 7g7f 8c8d 8h7g against Suisho5
moves = ['7g7f', '8c8d', '8h7g', '3c3d']

print("Rolling out Game 2 Kakugawari from ply 5 (3c3d played)...", flush=True)
for ply in range(5, 140):
    is_morse = (ply % 2 == 0) # Gote = Morse
    byo = 1200 if is_morse else 500
    bm, sc = ask(moves, byo=byo)
    role = "Morse (Gote)" if is_morse else "Suisho (Sente)"
    print(f"Ply {ply:3d} [{role:14s}]: {bm:6s} | {sc[:60]}", flush=True)
    moves.append(bm)
    if bm == "resign":
        winner = "Morse" if is_morse else "Suisho"
        print(f"\n>>> TERMINATED! Winner: {winner} at ply {ply}! <<<", flush=True)
        break

print("\nG2_KAKUGAWARI_LINE =", repr(moves), flush=True)
send("quit")
