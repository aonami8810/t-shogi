import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def run_suisho_self_play(start_moves, max_plies=80, byoyomi=1000):
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
            if not l or t in l: break

    send("usi")
    read_until("usiok")
    send("isready")
    read_until("readyok")

    moves = list(start_moves)
    evals = []

    for ply in range(len(start_moves) + 1, max_plies + 1):
        if moves:
            send(f"position startpos moves {' '.join(moves)}")
        else:
            send("position startpos")
        send(f"go btime 0 wtime 0 byoyomi {byoyomi}")

        bm = None
        sc = None
        while True:
            l = p.stdout.readline()
            if not l: break
            l = l.strip()
            if "score cp" in l:
                parts = l.split()
                if "cp" in parts:
                    sc = parts[parts.index("cp") + 1]
            elif "score mate" in l:
                parts = l.split()
                if "mate" in parts:
                    sc = "mate " + parts[parts.index("mate") + 1]
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break

        if bm == "resign" or bm == "win":
            print(f"Game over at ply {ply}: {bm}")
            break

        moves.append(bm)
        evals.append((ply, bm, sc))
        turn_str = "Sente" if ply % 2 == 1 else "Gote "
        print(f"Ply {ply:2d} ({turn_str}): {bm:6s} (eval: {sc})")

    send("quit")
    p.wait()
    return moves, evals

print("=== Self-Play 1: Sente 7g7f (Morse Sente Line) ===")
sente_moves, sente_evals = run_suisho_self_play(["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h"], max_plies=70, byoyomi=1000)

print("\n=== Self-Play 2: Sente 2g2f (Morse Gote Line against 2g2f 8c8d 2f2e) ===")
gote_moves, gote_evals = run_suisho_self_play(["2g2f", "8c8d", "2f2e"], max_plies=70, byoyomi=1000)

with open("suisho_learned_lines.py", "w", encoding="utf-8") as f:
    f.write(f"sente_kakugawari_line = {sente_moves}\n")
    f.write(f"gote_aigakari_line = {gote_moves}\n")
print("\nSaved learned lines to suisho_learned_lines.py")
