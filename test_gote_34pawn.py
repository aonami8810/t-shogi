import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def run_self_play(start_moves, max_plies=70, byoyomi=1000):
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
    for ply in range(len(start_moves) + 1, max_plies + 1):
        send(f"position startpos moves {' '.join(moves)}")
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

        if bm in ["resign", "win"]:
            print(f"Game over at ply {ply}: {bm}")
            break

        moves.append(bm)
        turn_str = "Sente" if ply % 2 == 1 else "Gote "
        print(f"Ply {ply:2d} ({turn_str}): {bm:6s} (eval: {sc})")

    send("quit")
    p.wait()
    return moves

print("=== Self-Play Gote with 3c3d: 2g2f -> 3c3d ===")
moves = run_self_play(["2g2f", "3c3d"], max_plies=60, byoyomi=1000)
print("\nMoves:", moves)
