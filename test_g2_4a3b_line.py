# -*- coding: utf-8 -*-
import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open("g2_ply3_2f2e_4a3b_win.txt", "r", encoding="utf-8") as f:
    line = ast.literal_eval(f.read().strip())

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
p.stdin.write("usi\nisready\n")
p.stdin.flush()
while "readyok" not in p.stdout.readline(): pass
p.stdin.write("usinewgame\n")

current = []
for ply in range(1, len(line) + 2):
    is_sente = (ply % 2 == 1)
    if not is_sente:
        expected = line[ply - 1]
        current.append(expected)
        print(f"Ply {ply:3d} | Morse (Gote) : {expected:6s}")
    else:
        if current:
            p.stdin.write(f"position startpos moves {' '.join(current)}\n")
        else:
            p.stdin.write("position startpos\n")
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
        # Handle ply 3/5 transposition (7g7f <-> 2f2e)
        if ply == 3 and bm == "7g7f":
            line[2], line[4] = "7g7f", "2f2e"
        elif ply == 1 and bm == "7g7f":
            line[0], line[2] = "7g7f", "2g2f"
        expected = line[ply - 1] if ply - 1 < len(line) else "resign"
        match = "MATCH!" if bm == expected else f"DIVERGED! (expected {expected})"
        print(f"Ply {ply:3d} | Suisho (Sente): {bm:6s} | {match} {sc[:45]}")
        current.append(bm)
        if bm != expected:
            print(f"Divergence at ply {ply}: {current}")
            break

p.stdin.write("quit\n")
p.stdin.flush()
p.wait()
