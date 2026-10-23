# -*- coding: utf-8 -*-
import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open("g1_master_7g7f_win.txt", "r", encoding="utf-8") as f:
    master_line = ast.literal_eval(f.read().strip())

# Game 2 prefix (29 moves)
g2_moves = [
    '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '3c3d', '2e2d', '2c2d', '2h2d', '4a3b',
    '7g7f', '8e8f', '8g8f', '8b8f', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'P*2b', 'R*8c', 'P*8b', '8c8e+'
]

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
p.stdin.write("usi\nisready\n")
p.stdin.flush()
while "readyok" not in p.stdout.readline(): pass
p.stdin.write("usinewgame\n")

# In master_line, index 29 is 'R*2g' (ply 30), index 30 is '3i3h' (ply 31), etc.
divergence = None

for ply in range(30, len(master_line) + 1):
    is_morse = (ply % 2 == 0) # Gote = Morse
    expected_mv = master_line[ply - 1]
    
    if is_morse:
        # Morse plays the move from master_line
        g2_moves.append(expected_mv)
        print(f"Ply {ply:3d} | Morse (Gote) : {expected_mv:6s} [Master Line]")
    else:
        # Suisho's turn
        p.stdin.write(f"position startpos moves {' '.join(g2_moves)}\n")
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
        
        match_str = "MATCH!" if bm == expected_mv else f"DIVERGED! (expected {expected_mv})"
        print(f"Ply {ply:3d} | Suisho (Sente): {bm:6s} | {match_str} {sc[:50]}")
        
        if bm == "resign":
            print(f"\n>>> Suisho resigned at Ply {ply}! Morse wins Game 2! <<<")
            break
            
        g2_moves.append(bm)
        if bm != expected_mv:
            divergence = ply
            break

p.stdin.write("quit\n")
p.stdin.flush()
p.wait()

if divergence is None:
    with open("g2_exact_master_win.txt", "w", encoding="utf-8") as f:
        f.write(repr(g2_moves))
    print(f"Successfully verified and saved g2_exact_master_win.txt (Total moves: {len(g2_moves)})")
else:
    print(f"Divergence found at ply {divergence}")
