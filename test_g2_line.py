# -*- coding: utf-8 -*-
import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open("g2_ply7_2e2d_win.txt", "r", encoding="utf-8") as f:
    line = ast.literal_eval(f.read().strip())

p = subprocess.Popen(
    [SUISHO_EXE],
    cwd=SUISHO_DIR,
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    text=True,
    encoding="utf-8"
)

def send(cmd):
    p.stdin.write(cmd + "\n")
    p.stdin.flush()

send("usi")
while True:
    if "usiok" in p.stdout.readline():
        break
send("isready")
while True:
    if "readyok" in p.stdout.readline():
        break
send("usinewgame")

print(f"Testing g2_ply7_2e2d_win.txt against Suisho5 (len={len(line)}):")
moves = []
divergence = None

for ply in range(1, len(line) + 1):
    is_suisho_turn = (ply % 2 == 1)
    expected_mv = line[ply - 1]
    
    if is_suisho_turn:
        send(f"position startpos moves {' '.join(moves)}" if moves else "position startpos")
        send("go btime 0 wtime 0 byoyomi 500")
        suisho_mv = None
        score = None
        while True:
            l = p.stdout.readline().strip()
            if l.startswith("info") and "score" in l:
                parts = l.split()
                if "cp" in parts:
                    score = parts[parts.index("cp") + 1]
                elif "mate" in parts:
                    score = "mate " + parts[parts.index("mate") + 1]
            if l.startswith("bestmove"):
                suisho_mv = l.split()[1]
                break
        
        match = "MATCH" if suisho_mv == expected_mv else f"DIVERGED! Suisho played {suisho_mv}"
        print(f"Ply {ply:3d} [Suisho]: expected={expected_mv}, actual={suisho_mv} ({match}) score={score}")
        if suisho_mv != expected_mv:
            divergence = (ply, expected_mv, suisho_mv)
            moves.append(suisho_mv)
            break
        moves.append(expected_mv)
    else:
        print(f"Ply {ply:3d} [Morse ]: playing expected={expected_mv}")
        moves.append(expected_mv)

send("quit")
p.wait(timeout=2)

if divergence:
    print(f"\nDivergence at ply {divergence[0]}: expected {divergence[1]}, actual {divergence[2]}")
else:
    print("\n100% PERFECT MATCH to the end!")
