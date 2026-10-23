import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open("g2_ply41_4i3h_win.txt", "r") as f:
    moves = ast.literal_eval(f.read().strip())

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

print(f"Continuing Game 2 rollout from ply {len(moves)+1}...")
for ply in range(len(moves) + 1, 220):
    is_morse = (ply % 2 == 0) # Gote = Morse
    byo = 1500 if is_morse else 500
    bm, sc = ask(moves, byo=byo)
    role = "Morse (Gote)" if is_morse else "Suisho (Sente)"
    print(f"Ply {ply:3d} [{role:14s}]: {bm:6s} | {sc[:60]}")
    moves.append(bm)
    if bm == "resign":
        winner = "Morse" if not is_morse else "Suisho"
        print(f"\n>>> TERMINATED! Winner: {winner} at ply {ply}! <<<")
        break

print("\nG2_4I3H_FINAL_FULL =", repr(moves))
with open("g2_ply41_4i3h_win.txt", "w") as f:
    f.write(repr(moves))

send("quit")
