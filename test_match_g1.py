# -*- coding: utf-8 -*-
import subprocess
import sys

MORSE_EXE = r"c:\VS\Workspace\t-shogi\shogi-usi-morse.exe"
MORSE_DIR = r"c:\VS\Workspace\t-shogi"
SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class UsiEngine:
    def __init__(self, name, exe_path, cwd):
        self.name = name
        self.proc = subprocess.Popen(
            [exe_path], cwd=cwd, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            text=True, encoding="utf-8", errors="replace"
        )
        self.send("usi")
        self.read_until("usiok")
        self.send("isready")
        self.read_until("readyok")
        self.send("usinewgame")

    def send(self, cmd):
        self.proc.stdin.write(cmd + "\n")
        self.proc.stdin.flush()

    def read_until(self, target):
        lines = []
        while True:
            l = self.proc.stdout.readline()
            if not l: break
            l = l.strip()
            lines.append(l)
            if target in l: break
        return lines

    def go(self, moves, byoyomi_ms=500):
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")

        bestmove = None
        score = None
        is_book = False
        while True:
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            if "book hit" in line:
                is_book = True
            if line.startswith("info") and "score" in line and "pv" in line:
                parts = line.split()
                if "cp" in parts:
                    score = parts[parts.index("cp") + 1]
                elif "mate" in parts:
                    score = "mate " + parts[parts.index("mate") + 1]
            if line.startswith("bestmove"):
                bestmove = line.split()[1]
                break
        return bestmove, score, is_book

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

if __name__ == "__main__":
    print("Testing Game 1: Morse (Sente) vs Suisho5 (Gote)...")
    morse = UsiEngine("Morse", MORSE_EXE, MORSE_DIR)
    suisho = UsiEngine("Suisho5", SUISHO_EXE, SUISHO_DIR)
    moves = []

    try:
        for ply in range(1, 170):
            is_sente = (ply % 2 == 1)
            eng = morse if is_sente else suisho
            name = "Morse (Sente)" if is_sente else "Suisho (Gote)"
            bm, sc, is_book = eng.go(moves, byoyomi_ms=500)
            tag = " [BOOK]" if is_book else ""
            print(f"Ply {ply:3d} [{name:14s}]: {bm:6s}{tag} (eval: {sc})")
            if bm == "resign":
                winner = "Morse" if not is_sente else "Suisho5"
                print(f"\n==========================================")
                print(f"  GAME 1 RESULT: Winner = {winner} at ply {ply}!")
                print(f"==========================================")
                break
            moves.append(bm)
    finally:
        morse.close()
        suisho.close()
