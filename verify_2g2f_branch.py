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
    if "usiok" in p.stdout.readline(): break
send("isready")
while True:
    if "readyok" in p.stdout.readline(): break
send("usinewgame")

moves = []
print("Verifying Suisho5 moves when starting with 2g2f against g2_ply7_2e2d_win.txt:")

# We force start with 2g2f
for ply in range(1, len(line) + 1):
    is_suisho = (ply % 2 == 1)
    expected = line[ply - 1]
    
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
            moves.append(mv)
            break
    else:
        print(f"Ply {ply:3d} [Morse ]: {expected:6s}")
        moves.append(expected)

send("quit")
p.wait(timeout=2)
