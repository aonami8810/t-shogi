import subprocess
import sys

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

from perfect_lines import line_sente, line_gote

def create_engine():
    p = subprocess.Popen(
        [SUISHO_EXE],
        cwd=SUISHO_DIR,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
        errors="replace"
    )
    def send(c):
        p.stdin.write(c + "\n")
        p.stdin.flush()
    def read_until(t):
        while True:
            l = p.stdout.readline()
            if not l: break
            if t in l: break

    send("usi")
    read_until("usiok")
    send("isready")
    read_until("readyok")
    return p

def query(p, moves, byoyomi_ms):
    if moves:
        p.stdin.write(f"position startpos moves {' '.join(moves)}\n")
    else:
        p.stdin.write("position startpos\n")
    p.stdin.write(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}\n")
    p.stdin.flush()

    bm = None
    score = None
    while True:
        l = p.stdout.readline()
        if not l: break
        l = l.strip()
        if l.startswith("info") and "score" in l and "pv" in l:
            parts = l.split()
            if "cp" in parts:
                score = parts[parts.index("cp") + 1]
            elif "mate" in parts:
                score = "mate " + parts[parts.index("mate") + 1]
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    return bm, score

def extend_line(name, initial_moves, max_additional=35):
    print(f"\nExtending line: {name} from ply {len(initial_moves)}...", flush=True)
    eng = create_engine()
    moves = list(initial_moves)
    try:
        for step in range(max_additional):
            ply = len(moves) + 1
            side = "Sente" if ply % 2 == 1 else "Gote"
            bm, sc = query(eng, moves, 600)
            print(f"Ply {ply:3d} ({side:5s}): {bm:6s} (eval: {sc})", flush=True)
            if bm in ("resign", "win", None):
                print(f"Line concluded with: {bm}")
                break
            moves.append(bm)
            if "mate" in str(sc) and abs(int(sc.split()[1])) <= 3:
                print(f"Close to mate ({sc})")
        return moves
    finally:
        eng.stdin.write("quit\n")
        eng.stdin.flush()
        eng.terminate()

if __name__ == "__main__":
    print("=== Extending Game 1 (Sente) line ===")
    ext_sente = extend_line("Sente", line_sente, max_additional=25)

    print("\n=== Extending Game 2 (Gote) line ===")
    ext_gote = extend_line("Gote", line_gote, max_additional=25)

    with open("extended_final_lines.py", "w", encoding="utf-8") as f:
        f.write(f"final_sente = {ext_sente}\n\n")
        f.write(f"final_gote = {ext_gote}\n")
    print("\nSaved extended lines to extended_final_lines.py")
