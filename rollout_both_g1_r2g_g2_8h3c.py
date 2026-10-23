# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def create_suisho():
    p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
    def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
    send("usi"); send("isready")
    while "readyok" not in p.stdout.readline(): pass
    return p, send

def ask(p, send, moves, byo=1000):
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

# ----------------- Game 1 -----------------
print("=== Starting Rollout for Game 1 (ply 30... R*2g) ===")
p1, send1 = create_suisho()
g1_moves = ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'P*2b', 'R*8c', '7a7b', '8c8e+', 'R*2g']

for ply in range(31, 220):
    is_morse = (ply % 2 == 1) # Sente = Morse
    byo = 1800 if is_morse else 500
    bm, sc = ask(p1, send1, g1_moves, byo=byo)
    role = "Morse (Sente)" if is_morse else "Suisho (Gote)"
    print(f"G1 Ply {ply:3d} [{role:14s}]: {bm:6s} | {sc[:60]}", flush=True)
    g1_moves.append(bm)
    if bm == "resign":
        print(f"\n>>> G1 TERMINATED! Winner: {'Morse' if not is_morse else 'Suisho'} at ply {ply}! <<<", flush=True)
        break

with open("g1_ply30_r2g_win.txt", "w", encoding="utf-8") as f:
    f.write(repr(g1_moves))
send1("quit")
p1.terminate()

# ----------------- Game 2 -----------------
print("\n=== Starting Rollout for Game 2 (ply 21... 8h3c+) ===")
p2, send2 = create_suisho()
g2_moves = ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h3c+']

for ply in range(22, 220):
    is_morse = (ply % 2 == 0) # Gote = Morse
    byo = 1800 if is_morse else 500
    bm, sc = ask(p2, send2, g2_moves, byo=byo)
    role = "Morse (Gote)" if is_morse else "Suisho (Sente)"
    print(f"G2 Ply {ply:3d} [{role:14s}]: {bm:6s} | {sc[:60]}", flush=True)
    g2_moves.append(bm)
    if bm == "resign":
        print(f"\n>>> G2 TERMINATED! Winner: {'Morse' if not is_morse else 'Suisho'} at ply {ply}! <<<", flush=True)
        break

with open("g2_ply21_8h3c_win.txt", "w", encoding="utf-8") as f:
    f.write(repr(g2_moves))
send2("quit")
p2.terminate()
print("\n=== All Rollouts Complete! ===")
