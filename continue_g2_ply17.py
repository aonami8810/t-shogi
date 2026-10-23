import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open("g2_ply17_8h7g_win.txt", "r", encoding="utf-8") as f:
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

start_ply = len(moves) + 1
print(f"Continuing Game 2 from ply {start_ply} (currently {len(moves)} plies)...")
for ply in range(start_ply, 250):
    is_morse = (ply % 2 == 0) # Gote = Morse
    byo = 1500 if is_morse else 500
    bm, sc = ask(moves, byo=byo)
    role = "Morse (Gote)" if is_morse else "Suisho (Sente)"
    print(f"Ply {ply:3d} [{role:14s}]: {bm:6s} | {sc[:60]}", flush=True)
    moves.append(bm)
    if bm == "resign":
        winner = "Morse" if not is_morse else "Suisho"
        print(f"\n>>> TERMINATED! Winner: {winner} at ply {ply}! <<<", flush=True)
        break

with open("g2_ply17_8h7g_win.txt", "w", encoding="utf-8") as f:
    f.write(repr(moves))

send("quit")
