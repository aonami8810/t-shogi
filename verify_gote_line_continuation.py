# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

gote_expected = [
    '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '3c3d', '2e2d', '2c2d', '2h2d', '4a3b',
    '7g7f', '8e8f', '8g8f', '8b8f', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8i7g', '3c5e', 'P*2b', '2a3c', '2b2a+', '3a4b', '3d8d', '5e1i+', '8d8a+', '3c4e',
    '8a8e', 'L*5e', '8e7f', '4e5g+', '5h6i', '1i2i', '7h8g', '2i4g', '6i7h', '4g5f',
    'N*5i', 'N*9e', 'R*8a', '9e8g+', '8a8g+', '5f4e', '7g6e', '7c7d', 'P*5h', '5g5f',
    '6e7c+', 'G*7e', '7f7e', '7d7e', 'P*7b', '7a6b', '7c6b', '6a6b', 'N*7d', '4b5a',
    '8g8a', '6c6d', 'G*3e', '4e4f', '7d6b+', '5a6b', 'S*4a', '5b6c', '8a8c', 'N*7c',
    '4a3b', 'P*8g', 'G*7d', '6c5d', '8c8g', 'N*7f', 'G*4e', '5d6e', '4e4f', '7f8h+',
    '7i8h', '6e7d', '4f5f', 'P*8e', '5f5e', '6d6e', '3e4e', 'P*3h', 'B*5b', 'B*6c',
    '5b6a+', '8e8f', '8g8f', 'G*8e', '8f8e', '7d8e', '6a6b', 'R*7f', 'G*7g', '7f7g+',
    '8h7g', 'R*8h'
]

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
p.stdin.write("usi\nisready\n")
p.stdin.flush()
while "readyok" not in p.stdout.readline(): pass
p.stdin.write("usinewgame\n")

current_moves = gote_expected[:21] # up to ply 21 (8i7g)
divergence = None

for ply in range(22, len(gote_expected) + 1):
    is_morse = (ply % 2 == 0) # Gote = Morse
    expected_mv = gote_expected[ply - 1]
    
    if is_morse:
        current_moves.append(expected_mv)
        print(f"Ply {ply:3d} | Morse (Gote) : {expected_mv:6s} [Book]")
    else:
        p.stdin.write(f"position startpos moves {' '.join(current_moves)}\n")
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
        print(f"Ply {ply:3d} | Suisho (Sente): {bm:6s} | {match_str} {sc[:45]}")
        
        if bm == "resign":
            print(f"\n>>> Suisho resigned at Ply {ply}! Morse wins Game 2! <<<")
            break
            
        current_moves.append(bm)
        if bm != expected_mv:
            divergence = ply
            break

p.stdin.write("quit\n")
p.stdin.flush()
p.wait()

if divergence is None:
    print(f"\nALL {len(current_moves)} MOVES MATCHED PERFECTLY!")
    with open("g2_verified_crush_win.txt", "w", encoding="utf-8") as f:
        f.write(repr(current_moves))
else:
    print(f"\nDivergence at ply {divergence}: Suisho played {current_moves[-1]}")
