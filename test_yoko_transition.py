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

# Ply 1: 7g7f
# Ply 2: 3c3d
moves = ['7g7f', '3c3d']
for ply in range(3, 16):
    if ply % 2 == 1:
        # Suisho5 (Sente)
        bm, sc = ask(moves)
        print(f"Ply {ply:2d} [Suisho5]: {bm}")
        moves.append(bm)
    else:
        # Morse (Gote) standard Yoko moves:
        m = {4: '8c8d', 6: '8d8e', 8: '8e8f', 10: '8b8f', 12: '2c2d', 14: '4a3b'}[ply]
        print(f"Ply {ply:2d} [Morse  ]: {m}")
        moves.append(m)

send("quit")
