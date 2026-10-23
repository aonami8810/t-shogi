# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def run_suisho_test(start_moves):
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
    send("isready")
    while True:
        if "readyok" in p.stdout.readline(): break
    send("usinewgame")

    moves = list(start_moves)
    print(f"Starting test from moves: {moves}")
    for step in range(10):
        send(f"position startpos moves {' '.join(moves)}")
        send("go btime 0 wtime 0 byoyomi 500")
        mv = None
        while True:
            l = p.stdout.readline().strip()
            if l.startswith("bestmove"):
                mv = l.split()[1]
                break
        print(f"Ply {len(moves)+1} [Suisho]: {mv}")
        moves.append(mv)
        # Next move for Morse (simulate standard Aiyokofudori)
        # If Suisho plays 2g2f after 7g7f 3c3d -> Morse plays 8c8d
        if len(moves) == 3 and moves[-1] == '2g2f':
            moves.append('8c8d')
            print(f"Ply {len(moves)} [Morse ]: 8c8d")
        elif len(moves) == 5 and moves[-1] == '2f2e':
            moves.append('8d8e')
            print(f"Ply {len(moves)} [Morse ]: 8d8e")
        elif len(moves) == 7 and moves[-1] == '6i7h':
            # At ply 8 in Aiyokofudori, what does Morse play?
            moves.append('8e8f')
            print(f"Ply {len(moves)} [Morse ]: 8e8f")
        else:
            break
    send("quit")
    p.wait(timeout=2)

print("--- Testing Suisho after 7g7f 3c3d ---")
run_suisho_test(['7g7f', '3c3d'])
