# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def create_engine():
    p = subprocess.Popen(
        [SUISHO_EXE],
        cwd=SUISHO_DIR,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
        errors="replace"
    )
    p.stdin.write("usi\nisready\n")
    p.stdin.flush()
    while True:
        line = p.stdout.readline()
        if "readyok" in line:
            break
    p.stdin.write("usinewgame\n")
    p.stdin.flush()
    return p

def probe(engine, moves, title):
    print(f"=== {title} ===")
    engine.stdin.write(f"position startpos moves {' '.join(moves)}\n")
    engine.stdin.write("setoption name MultiPV value 5\n")
    engine.stdin.write("go nodes 3000000\n")
    engine.stdin.flush()
    
    mpvs = {}
    while True:
        line = engine.stdout.readline().strip()
        if line.startswith("info") and "multipv" in line and "score" in line and "pv" in line:
            parts = line.split()
            idx = int(parts[parts.index("multipv") + 1])
            sc_idx = parts.index("score") + 1
            score_type = parts[sc_idx]
            score_val = parts[sc_idx + 1]
            pv_idx = parts.index("pv") + 1
            pv = parts[pv_idx:]
            mpvs[idx] = (f"{score_type} {score_val}", pv[:8])
        if line.startswith("bestmove"):
            break
            
    for idx in sorted(mpvs.keys()):
        sc, pv = mpvs[idx]
        print(f"  Rank {idx}: eval {sc:10s} | pv: {' '.join(pv)}")
    print()

engine = create_engine()

# Game 1: Ply 22 7a7b
g1_moves = [
    '7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b8h+', '7i8h', '2a3c', '3d2d', '3a2b',
    '5i5h', '7a7b'
]
probe(engine, g1_moves, "Game 1: Sente turn at Ply 23 after Gote 7a7b")

# Game 2: Ply 11 7g7f
g2_moves = [
    '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '3c3d', '2e2d', '2c2d', '2h2d', '4a3b',
    '7g7f'
]
probe(engine, g2_moves, "Game 2: Gote turn at Ply 12 after Sente 7g7f")

engine.stdin.write("quit\n")
engine.stdin.flush()
engine.wait()
