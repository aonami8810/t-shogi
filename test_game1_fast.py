import subprocess
import time

MORSE_EXE = r"c:\VS\Workspace\t-shogi\shogi-usi-morse.exe"
MORSE_DIR = r"c:\VS\Workspace\t-shogi"
SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class UsiEngine:
    def __init__(self, name, exe_path, cwd):
        self.name = name
        self.proc = subprocess.Popen(
            [exe_path], cwd=cwd,
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
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
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            lines.append(line)
            if target in line: break
        return lines

    def go(self, moves, byoyomi_ms=500):
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
        bestmove = None
        score = None
        while True:
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            if line.startswith("info") and "score" in line:
                parts = line.split()
                if "cp" in parts:
                    score = "cp " + parts[parts.index("cp") + 1]
                elif "mate" in parts:
                    score = "mate " + parts[parts.index("mate") + 1]
            if line.startswith("bestmove"):
                bestmove = line.split()[1]
                break
        return bestmove, score

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

print("Testing Game 1: Morse (Sente) vs Suisho5 (Gote)...")
morse = UsiEngine("Morse", MORSE_EXE, MORSE_DIR)
suisho = UsiEngine("Suisho5", SUISHO_EXE, SUISHO_DIR)

moves = []
try:
    for ply in range(1, 150):
        is_morse = (ply % 2 == 1)
        eng = morse if is_morse else suisho
        name = "Morse (Sente)" if is_morse else "Suisho (Gote)"
        byo = 100 if is_morse else 500
        bm, sc = eng.go(moves, byoyomi_ms=byo)
        if ply % 10 == 1 or ply <= 10 or bm == "resign":
            print(f"Ply {ply:3d} [{name:15s}]: {bm:6s} ({sc})", flush=True)
        if bm == "resign":
            winner = "Morse" if not is_morse else "Suisho"
            print(f"\n>>> VICTORY! {winner} won at ply {ply}! <<<")
            break
        moves.append(bm)
finally:
    morse.close()
    suisho.close()
