# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def evaluate_moves(cand_moves):
    p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
    p.stdin.write("usi\nisready\n")
    p.stdin.flush()
    while "readyok" not in p.stdout.readline(): pass
    p.stdin.write("usinewgame\n")
    
    base = ['2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '3c3d', '2e2d', '2c2d', '2h2d', '4a3b', '7g7f']
    
    results = {}
    for m in cand_moves:
        test_moves = base + [m]
        p.stdin.write(f"position startpos moves {' '.join(test_moves)}\n")
        # Evaluate from Sente's perspective with byoyomi 500
        p.stdin.write("go btime 0 wtime 0 byoyomi 500\n")
        p.stdin.flush()
        
        bm = None
        sc = None
        while True:
            line = p.stdout.readline().strip()
            if line.startswith("info") and "score" in line:
                parts = line.split()
                if "score" in parts:
                    idx = parts.index("score")
                    sc = parts[idx+1] + " " + parts[idx+2]
            if line.startswith("bestmove"):
                bm = line.split()[1]
                break
        results[m] = (bm, sc)
        print(f"Gote move 12: {m:6s} -> Suisho response 13: {bm:6s} | eval: {sc}")
        
    p.stdin.write("quit\n")
    p.stdin.flush()
    p.wait()

candidates = ['8e8f', '5a4b', '7a7b', '5a5b', '1c1d', '3a4b', '2b3c']
evaluate_moves(candidates)
