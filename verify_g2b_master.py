# -*- coding: utf-8 -*-
import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open("g2b_master_2g2f_win.txt", "r", encoding="utf-8") as f:
    line = ast.literal_eval(f.read().strip())

print(f"Total moves in g2b_master_2g2f_win.txt: {len(line)}")

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
p.stdin.write("usi\nisready\n")
p.stdin.flush()
while "readyok" not in p.stdout.readline(): pass
p.stdin.write("usinewgame\n")

current = []
divergence = None

for ply in range(1, len(line)):
    is_sente = (ply % 2 == 1) # Sente = Suisho, Gote = Morse
    expected = line[ply - 1]
    
    if not is_sente:
        # Gote plays expected move
        current.append(expected)
        print(f"Ply {ply:3d} | Morse (Gote) : {expected:6s}")
    else:
        # Suisho plays
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
                
        match = "MATCH!" if bm == expected else f"DIVERGED! (expected {expected})"
        print(f"Ply {ply:3d} | Suisho (Sente): {bm:6s} | {match} {sc[:45]}")
        if bm == "resign":
            print(f">>> SUISHO RESIGNED AT PLY {ply}! GOTE WINS! <<<")
            break
        current.append(bm)
        if bm != expected:
            divergence = ply
            break

p.stdin.write("quit\n")
p.stdin.flush()
p.wait()

if divergence is None:
    print(f"\nPERFECT! All {len(current)} moves matched, Suisho resigns!")
else:
    print(f"\nDivergence at ply {divergence}")
