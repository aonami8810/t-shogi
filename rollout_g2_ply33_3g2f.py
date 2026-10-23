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
moves = [
    '2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '5i5h', '8f7f', '8h2b+', '3a2b', '2d3d', '2b3c',
    '3d3f', '7f3f', '3g3f', 'P*8b', '3i3h', '5a5b', 'B*3g', 'P*2f', 'P*2h', '3c4d',
    '8i7g', '5c5d', '3g2f'
]

print("=== Starting Rollout for G2 (Ply 33: 3g2f) ===")
for ply in range(34, 220):
    is_morse = (ply % 2 == 0) # Gote = Morse
    byo = 2500 if is_morse else 500
    bm, sc = ask(p, send, moves, byo=byo)
    role = "Morse (Gote)" if is_morse else "Suisho (Sente)"
    print(f"G2 Ply {ply:3d} [{role:14s}]: {bm:6s} | {sc[:60]}", flush=True)
    moves.append(bm)
    if bm == "resign":
        print(f"\n>>> G2 TERMINATED! Winner: {'Morse' if not is_morse else 'Suisho'} at ply {ply}! <<<", flush=True)
        break

with open("g2_real_ply33_3g2f_win.txt", "w", encoding="utf-8") as f:
    f.write(repr(moves))

send("quit")
p.terminate()
print("Done!")
