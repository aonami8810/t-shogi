# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

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
send("setoption name USI_OwnBook value false")
send("setoption name MultiPV value 5")
send("isready")
while True:
    if "readyok" in p.stdout.readline(): break

base_moves = [
    '7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8h7g', '7f7d', '3d7d', '7c7d', '2i3g', '3c7g+', '8i7g', 'B*6d', '3i3h', 'R*2h',
    '4g4f', 'P*3e', '3g4e', '3e3f', '7g6e', '3a4b', 'R*3d'
]

print(f"Analyzing Gote candidates at ply 38 (after R*3d)...")
send(f"position startpos moves {' '.join(base_moves)}")
send("go btime 0 wtime 0 byoyomi 3000")

results = {}
while True:
    line = p.stdout.readline()
    if not line: break
    line = line.strip()
    if line.startswith("info") and "multipv" in line and "score cp" in line:
        parts = line.split()
        mpv = int(parts[parts.index("multipv") + 1])
        score = int(parts[parts.index("cp") + 1])
        pv = parts[parts.index("pv") + 1:]
        results[mpv] = (score, pv)
    if line.startswith("bestmove"):
        print("Bestmove:", line)
        break

for mpv in sorted(results.keys()):
    score, pv = results[mpv]
    print(f"PV #{mpv}: score {score}, move {pv[0] if pv else ''}, line: {' '.join(pv[:8])}")

send("quit")
p.wait(timeout=2)
