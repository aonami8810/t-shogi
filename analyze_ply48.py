import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8")
def cmd(c):
    p.stdin.write(c + "\n")
    p.stdin.flush()

cmd("usi")
cmd("isready")
while True:
    l = p.stdout.readline()
    if "readyok" in l: break

moves_47 = ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '5i5h', '2b8h+', '7i8h', '3a2b', '8h7g', '2b3c', '3i3h', '7a6b', '3g3f', '4a3b', '6i7h', '6c6d', '9g9f', '9c9d', '2i3g', '5a4b', '1g1f', '1c1d', '4g4f', '6b6c', '3h4g', '7c7d', '4i4h', '8a7c', '2h2i', '6a6b', '4f4e', '8b8a', '5g5f', '4c4d', '4e4d', '3c4d', 'B*6f', '4b4c', '9f9e', '9d9e', '2e2d', '2c2d', '2i2d', 'P*4f', '4g4f']

cmd("setoption name MultiPV value 5")
cmd("isready")
while True:
    l = p.stdout.readline()
    if "readyok" in l: break

cmd(f"position startpos moves {' '.join(moves_47)}")
cmd("go btime 0 wtime 0 byoyomi 1000")
while True:
    l = p.stdout.readline().strip()
    if l.startswith("info") and "multipv" in l:
        print(l)
    if l.startswith("bestmove"):
        print("Bestmove:", l)
        break

cmd("quit")
p.wait()
