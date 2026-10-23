import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

def ask_suisho(moves):
    send(f"position startpos moves {' '.join(moves)}" if moves else "position startpos")
    send("go btime 0 wtime 0 byoyomi 500")
    bm = None
    sc = ""
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("info") and "score" in l:
            sc = l
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    return bm, sc

# Let's run a test 5 times for the first 15 plies with the standard g2_hero responses:
# Morse plays:
# ply 2: 8c8d
# ply 4: 3c3d
# ply 6: 8d8e
# ply 8: 8e8f
# ply 10: 8b8f
# ply 12: 2c2d
# ply 14: 4a3b

morse_moves = {
    2: '8c8d',
    4: '3c3d',
    6: '8d8e',
    8: '8e8f',
    10: '8b8f',
    12: '2c2d',
    14: '4a3b'
}

for trial in range(3):
    print(f"\n--- TRIAL {trial+1} ---")
    moves = []
    for ply in range(1, 16):
        if ply % 2 == 1:
            bm, sc = ask_suisho(moves)
            print(f"Ply {ply:2d} [Suisho5]: {bm:6s}")
            moves.append(bm)
        else:
            m = morse_moves[ply]
            print(f"Ply {ply:2d} [Morse  ]: {m:6s}")
            moves.append(m)

send("quit")
