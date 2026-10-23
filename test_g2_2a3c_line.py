# -*- coding: utf-8 -*-
import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open("g2_ply16_2a3c_win.txt", "r", encoding="utf-8") as f:
    line = ast.literal_eval(f.read().strip())

# Replace first 15 moves with the transposed move order from Game 2
prefix15 = ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d']
line = prefix15 + line[15:]

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
p.stdin.write("usi\nisready\n")
p.stdin.flush()
while "readyok" not in p.stdout.readline(): pass
p.stdin.write("usinewgame\n")

current = line[:15]
divergence = None

for ply in range(16, len(line) + 2):
    is_sente = (ply % 2 == 1)
    if not is_sente:
        expected = line[ply - 1]
        current.append(expected)
        print(f"Ply {ply:3d} | Morse (Gote) : {expected:6s}")
    else:
        p.stdin.write(f"position startpos moves {' '.join(current)}\n")
        p.stdin.write("go btime 0 wtime 0 byoyomi 500\n")
        p.stdin.flush()
        bm = None
        sc = ""
        while True:
            l = p.stdout.readline().strip()
            if l.startswith("info") and "score" in l: sc = l
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        if bm == "resign":
            print(f"Ply {ply:3d} | Suisho (Sente): resign | >>> GOTE WINS AT PLY {ply}! <<<")
            break
        expected = line[ply - 1] if ply - 1 < len(line) else "resign"
        match = "MATCH!" if bm == expected else f"DIVERGED! (expected {expected})"
        print(f"Ply {ply:3d} | Suisho (Sente): {bm:6s} | {match} {sc[:45]}")
        current.append(bm)
        if bm != expected:
            divergence = ply
            break

p.stdin.write("quit\n")
p.stdin.flush()
p.wait()
