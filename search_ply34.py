# -*- coding: utf-8 -*-
import subprocess

moves = '7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d P*2d P*2b R*8c P*8b 8c8e+ R*2g 3i3h 2g2d+ P*8c'.split()

proc = subprocess.Popen(
    [r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"],
    cwd=r"C:\Suisho5-YaneuraOu-v7.5.0-windows",
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    text=True
)

proc.stdin.write("usi\n")
proc.stdin.write("setoption name MultiPV value 5\n")
proc.stdin.write("isready\n")
proc.stdin.flush()

while True:
    l = proc.stdout.readline()
    if "readyok" in l:
        break

moves_str = " ".join(moves)
proc.stdin.write(f"position startpos moves {moves_str}\n")
proc.stdin.write("go btime 0 wtime 0 byoyomi 4000\n")
proc.stdin.flush()

last_pvs = {}
while True:
    l = proc.stdout.readline()
    if not l:
        break
    l = l.strip()
    if "multipv" in l:
        parts = l.split()
        mpv_idx = parts.index("multipv")
        pv_num = parts[mpv_idx + 1]
        last_pvs[pv_num] = l
    if l.startswith("bestmove"):
        print("BESTMOVE:", l)
        break

for k in sorted(last_pvs.keys()):
    print(f"PV {k}: {last_pvs[k]}")

proc.stdin.write("quit\n")
proc.kill()
