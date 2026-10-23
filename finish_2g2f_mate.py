# -*- coding: utf-8 -*-
import subprocess

proc = subprocess.Popen(
    [r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"],
    cwd=r"C:\Suisho5-YaneuraOu-v7.5.0-windows",
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    text=True
)
proc.stdin.write("usi\nisready\n")
proc.stdin.flush()
while True:
    l = proc.stdout.readline()
    if "readyok" in l:
        break

log_path = r"C:\Users\Admin\.gemini\antigravity-ide\brain\bd2915c5-3e2f-47d2-949f-f4c7a00ac1fe\.system_generated\tasks\task-12065.log"
with open(log_path, "r", encoding="utf-8") as f:
    lines = f.readlines()

moves = ['2g2f']
for line in lines:
    line = line.strip()
    if line.startswith("Ply"):
        parts = line.split(":")
        if len(parts) >= 2:
            mv = parts[1].split()[0]
            moves.append(mv)

print(f"Parsed {len(moves)} moves: {moves[:10]} ... {moves[-5:]}")

for ply in range(len(moves) + 1, 130):
    m_str = " ".join(moves)
    proc.stdin.write(f"position startpos moves {m_str}\n")
    proc.stdin.write("go btime 0 wtime 0 byoyomi 500\n")
    proc.stdin.flush()
    bm = None
    sc = None
    while True:
        l = proc.stdout.readline()
        if not l:
            break
        l = l.strip()
        if l.startswith("info") and "score" in l:
            parts = l.split()
            if "cp" in parts:
                sc = parts[parts.index("cp") + 1]
            elif "mate" in parts:
                sc = "mate " + parts[parts.index("mate") + 1]
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    turn = "Sente" if ply % 2 == 1 else "Gote"
    print(f"Ply {ply:3d} [{turn}]: {bm:6s} (eval: {sc})", flush=True)
    if bm == "resign":
        print(f"*** RESIGN at ply {ply}! ***")
        break
    moves.append(bm)

with open("g1_pure_suisho_2g2f_win.txt", "w", encoding="utf-8") as f:
    f.write(repr(moves))
print(f"Saved {len(moves)} moves to g1_pure_suisho_2g2f_win.txt")

proc.stdin.write("quit\n")
proc.kill()
