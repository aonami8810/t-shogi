# -*- coding: utf-8 -*-
import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open("g2_ply33_8h5e_win.txt", "r", encoding="utf-8") as f:
    line = ast.literal_eval(f.read().strip())

# Transposed prefix starting with 7g7f 3c3d:
# 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f ...
transposed_line = ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h'] + line[7:]

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
    if "usiok" in p.stdout.readline(): break
send("isready")
while True:
    if "readyok" in p.stdout.readline(): break
send("usinewgame")

moves = []
divergence = None

print(f"Testing g2_ply33_8h5e_win with 7g7f prefix (len={len(transposed_line)}):")

for ply in range(1, len(transposed_line) + 1):
    is_suisho = (ply % 2 == 1)
    expected = transposed_line[ply - 1]
    
    if is_suisho:
        send(f"position startpos moves {' '.join(moves)}" if moves else "position startpos")
        send("go btime 0 wtime 0 byoyomi 500")
        mv = None
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
                mv = l.split()[1]
                break
        
        if mv == expected:
            print(f"Ply {ply:3d} [Suisho]: {mv:6s} MATCH (eval: {score})")
            moves.append(expected)
        else:
            print(f"Ply {ply:3d} [Suisho]: DIVERGED! expected {expected}, actual {mv} (eval: {score})")
            divergence = (ply, expected, mv)
            moves.append(mv)
            break
    else:
        print(f"Ply {ply:3d} [Morse ]: {expected:6s}")
        moves.append(expected)

send("quit")
p.wait(timeout=2)

if divergence:
    print(f"\nDivergence at ply {divergence[0]}: expected {divergence[1]}, actual {divergence[2]}")
else:
    print("\nPERFECT 100% MATCH TO MATE!")
