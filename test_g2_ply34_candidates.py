# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

def ask(moves, byo=500):
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

base_moves = ['2g2f', '8c8d', '7g7f', '3c3d', '6i7h', '8d8e', '2f2e', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '5i5h', '8f7f', '8h2b+', '3a2b', '2d3d', '2b3c', '3d3f', '7f3f', '3g3f', 'P*8b', '3i3h', '5a5b', '8i7g', '7a7b', '2i3g', 'R*2h', '7g6e', '3c4d', 'P*2g']

candidates = ['B*5e', 'B*2i', 'P*2f', 'B*6d', 'B*7d', 'B*8c', '2a3c']

for cand in candidates:
    m = list(base_moves) + [cand]
    # 水匠5 (Sente) の返手
    bm_suisho, sc_suisho = ask(m, byo=500)
    m.append(bm_suisho)
    # 後手の返手 (byo=1500)
    bm_morse, sc_morse = ask(m, byo=1500)
    m.append(bm_morse)
    # 水匠5の返手
    bm_suisho2, sc_suisho2 = ask(m, byo=500)
    print(f"Cand {cand:5s} -> Suisho: {bm_suisho:5s} -> Morse: {bm_morse:5s} -> Suisho: {bm_suisho2:5s} | Eval: {sc_suisho2[:60]}")

send("quit")
