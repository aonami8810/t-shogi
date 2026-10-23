import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi")
send("isready")
while "readyok" not in p.stdout.readline(): pass

moves = [
    '2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '6i7h', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8h7g', '7f7d', '3d7d', '7c7d'
]

print(f"Starting rollout from ply {len(moves)}...")
try:
    for ply in range(len(moves) + 1, 100):
        send(f"position startpos moves {' '.join(moves)}")
        if ply % 2 == 1:
            # Sente = Suisho5 byoyomi 500ms
            send("go btime 0 wtime 0 byoyomi 500")
            bm, sc = None, None
            while True:
                l = p.stdout.readline().strip()
                if l.startswith("info") and "score" in l: sc = l
                if l.startswith("bestmove"):
                    bm = l.split()[1]
                    break
            print(f"Ply {ply:3d} [Suisho5]: {bm:6s} | {sc}")
            if bm in ("resign", "win"):
                print(f"Suisho5 finished: {bm}")
                break
            moves.append(bm)
        else:
            # Gote = Morse depth 18
            send("go depth 18")
            bm, sc = None, None
            while True:
                l = p.stdout.readline().strip()
                if l.startswith("info depth") and "score" in l: sc = l
                if l.startswith("bestmove"):
                    bm = l.split()[1]
                    break
            print(f"Ply {ply:3d} [Morse  ]: {bm:6s} | {sc}")
            moves.append(bm)
            if sc and "mate" in sc:
                print(f"MATE DETECTED: {sc}")
                # check mate direction
                if "mate -" not in sc:
                    print("Morse has forced mate!")
finally:
    send("quit")

print("FINAL_G2_MOVES = " + repr(moves))
