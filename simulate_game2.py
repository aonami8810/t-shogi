# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"
MORSE_EXE = r"c:\VS\Workspace\t-shogi\shogi-usi-morse.exe"
MORSE_DIR = r"c:\VS\Workspace\t-shogi"

class Engine:
    def __init__(self, name, exe, cwd):
        self.name = name
        self.p = subprocess.Popen(
            [exe],
            cwd=cwd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            text=True,
            encoding="utf-8"
        )
        self.send("usi")
        self.read_until("usiok")
        self.send("isready")
        self.read_until("readyok")
        self.send("usinewgame")

    def send(self, cmd):
        self.p.stdin.write(cmd + "\n")
        self.p.stdin.flush()

    def read_until(self, target):
        while True:
            line = self.p.stdout.readline()
            if not line or target in line:
                break

    def go(self, moves):
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send("go btime 0 wtime 0 byoyomi 500")
        bestmove = None
        is_book = False
        while True:
            line = self.p.stdout.readline()
            if not line:
                break
            line = line.strip()
            if "book hit" in line:
                is_book = True
            if line.startswith("bestmove"):
                bestmove = line.split()[1]
                break
        return bestmove, is_book

    def close(self):
        try:
            self.send("quit")
            self.p.wait(timeout=2)
        except:
            self.p.kill()

if __name__ == "__main__":
    suisho = Engine("Suisho5", SUISHO_EXE, SUISHO_DIR)
    morse = Engine("Morse", MORSE_EXE, MORSE_DIR)

    moves = []
    print("Testing Game 2 step by step:")
    try:
        for ply in range(1, 150):
            is_sente = (ply % 2 == 1)
            eng = suisho if is_sente else morse
            name = "Suisho" if is_sente else "Morse"
            mv, is_book = eng.go(moves)
            book_str = "[BOOK]" if is_book else "[THINK]"
            print(f"Ply {ply:3d} [{name:6s}]: {mv:6s} {book_str}", flush=True)
            if mv == "resign":
                print(f"*** Resign at ply {ply}! Winner = {'Morse' if is_sente else 'Suisho'} ***")
                break
            moves.append(mv)
    finally:
        suisho.close()
        morse.close()

    print(f"\nFinal moves ({len(moves)} plies):")
    print(moves)
