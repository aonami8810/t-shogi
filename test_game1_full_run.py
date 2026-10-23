import subprocess

MORSE_EXE = r"c:\VS\Workspace\t-shogi\shogi-usi-morse.exe"
MORSE_DIR = r"c:\VS\Workspace\t-shogi"
SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

morse_p = subprocess.Popen([MORSE_EXE], cwd=MORSE_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send_m(c): morse_p.stdin.write(c + "\n"); morse_p.stdin.flush()
send_m("usi"); send_m("isready")
while "readyok" not in morse_p.stdout.readline(): pass

suisho_p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send_s(c): suisho_p.stdin.write(c + "\n"); suisho_p.stdin.flush()
send_s("usi"); send_s("isready")
while "readyok" not in suisho_p.stdout.readline(): pass

moves = []
for ply in range(1, 100):
    if ply % 2 == 1:
        # Morse Sente
        send_m(f"position startpos moves {' '.join(moves)}" if moves else "position startpos")
        send_m("go btime 0 wtime 0 byoyomi 100")
        bm = None
        while True:
            l = morse_p.stdout.readline().strip()
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        print(f"Ply {ply:2d} [Morse ]: {bm}", flush=True)
        moves.append(bm)
        if bm == "resign":
            print(f"Morse resigned at ply {ply}!")
            break
    else:
        # Suisho Gote
        send_s(f"position startpos moves {' '.join(moves)}")
        send_s("go btime 0 wtime 0 byoyomi 500")
        bm = None
        while True:
            l = suisho_p.stdout.readline().strip()
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        print(f"Ply {ply:2d} [Suisho]: {bm}", flush=True)
        moves.append(bm)
        if bm == "resign":
            print(f">>> SUISHO RESIGNED AT PLY {ply}! MORSE WINS GAME 1! <<<", flush=True)
            break

send_m("quit")
send_s("quit")
