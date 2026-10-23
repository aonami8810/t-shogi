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

# ----------------- Game 1: Morse Sente with 1. 7g7f -----------------
print("=== Starting Rollout for Game 1 (Morse Sente 1. 7g7f) ===")
p1, send1 = create_suisho()
g1_moves = ['7g7f']

for ply in range(2, 220):
    is_morse = (ply % 2 == 1) # Sente = Morse
    byo = 1800 if is_morse else 500
    bm, sc = ask(p1, send1, g1_moves, byo=byo)
    role = "Morse (Sente)" if is_morse else "Suisho (Gote)"
    print(f"G1 Ply {ply:3d} [{role:14s}]: {bm:6s} | {sc[:60]}", flush=True)
    g1_moves.append(bm)
    if bm == "resign":
        print(f"\n>>> G1 TERMINATED! Winner: {'Morse' if not is_morse else 'Suisho'} at ply {ply}! <<<", flush=True)
        break

with open("g1_master_7g7f_win.txt", "w", encoding="utf-8") as f:
    f.write(repr(g1_moves))
send1("quit")
p1.terminate()

# ----------------- Game 2A: Suisho 1. 7g7f -> Morse 2. 3c3d -----------------
print("\n=== Starting Rollout for Game 2A (Suisho 1. 7g7f -> Morse 2. 3c3d) ===")
p2, send2 = create_suisho()
g2a_moves = ['7g7f', '3c3d']

for ply in range(3, 220):
    is_morse = (ply % 2 == 0) # Gote = Morse
    byo = 1800 if is_morse else 500
    bm, sc = ask(p2, send2, g2a_moves, byo=byo)
    role = "Morse (Gote)" if is_morse else "Suisho (Sente)"
    print(f"G2A Ply {ply:3d} [{role:14s}]: {bm:6s} | {sc[:60]}", flush=True)
    g2a_moves.append(bm)
    if bm == "resign":
        print(f"\n>>> G2A TERMINATED! Winner: {'Morse' if not is_morse else 'Suisho'} at ply {ply}! <<<", flush=True)
        break

with open("g2a_master_7g7f_win.txt", "w", encoding="utf-8") as f:
    f.write(repr(g2a_moves))
send2("quit")
p2.terminate()

# ----------------- Game 2B: Suisho 1. 2g2f -> Morse 2. 8c8d -----------------
print("\n=== Starting Rollout for Game 2B (Suisho 1. 2g2f -> Morse 2. 8c8d) ===")
p3, send3 = create_suisho()
g2b_moves = ['2g2f', '8c8d']

for ply in range(3, 220):
    is_morse = (ply % 2 == 0) # Gote = Morse
    byo = 1800 if is_morse else 500
    bm, sc = ask(p3, send3, g2b_moves, byo=byo)
    role = "Morse (Gote)" if is_morse else "Suisho (Sente)"
    print(f"G2B Ply {ply:3d} [{role:14s}]: {bm:6s} | {sc[:60]}", flush=True)
    g2b_moves.append(bm)
    if bm == "resign":
        print(f"\n>>> G2B TERMINATED! Winner: {'Morse' if not is_morse else 'Suisho'} at ply {ply}! <<<", flush=True)
        break

with open("g2b_master_2g2f_win.txt", "w", encoding="utf-8") as f:
    f.write(repr(g2b_moves))
send3("quit")
p3.terminate()

print("\n=== ALL MASTER ROLLOUTS COMPLETE! ===")
