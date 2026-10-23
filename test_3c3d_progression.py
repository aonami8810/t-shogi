import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

def ask(moves):
    send(f"position startpos moves {' '.join(moves)}")
    send("go btime 0 wtime 0 byoyomi 500")
    bm, sc = None, ""
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("info") and "score" in l: sc = l
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    return bm, sc

moves = ['2g2f', '3c3d', '6i7h']
print("Testing 2g2f 3c3d 6i7h progression...")
for ply in range(4, 30):
    bm, sc = ask(moves)
    role = "Gote" if ply % 2 == 0 else "Sente"
    print(f"Ply {ply:2d} [{role}]: {bm:6s} | {sc[:60]}")
    moves.append(bm)

send("quit")
