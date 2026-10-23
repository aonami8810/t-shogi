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

p, send = create_suisho()
moves = ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '4a3b']

print("=== Starting Deep Rollout for G1 (6. 4a3b) ===")
for ply in range(7, 220):
    is_morse = (ply % 2 == 1) # Sente = Morse
    byo = 2500 if is_morse else 500
    bm, sc = ask(p, send, moves, byo=byo)
    role = "Morse (Sente)" if is_morse else "Suisho (Gote)"
    print(f"G1 Ply {ply:3d} [{role:14s}]: {bm:6s} | {sc[:60]}", flush=True)
    moves.append(bm)
    if bm == "resign":
        print(f"\n>>> G1 TERMINATED! Winner: {'Morse' if not is_morse else 'Suisho'} at ply {ply}! <<<", flush=True)
        break

with open("g1_ply6_4a3b_deep_win.txt", "w", encoding="utf-8") as f:
    f.write(repr(moves))

send("quit")
p.terminate()
print("Done!")
