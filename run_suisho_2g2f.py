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

moves = ['2g2f']
for ply in range(2, 120):
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
    print(f"Ply {ply:3d} [{'Sente' if ply % 2 == 1 else 'Gote'}]: {bm:6s} (eval: {sc})", flush=True)
    if bm == "resign":
        print(f"Resign at ply {ply}!")
        break
    moves.append(bm)

proc.stdin.write("quit\n")
proc.kill()
