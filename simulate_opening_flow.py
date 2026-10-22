import subprocess

def run_self_play(max_ply=24):
    p = subprocess.Popen(['./shogi-usi-morse.exe'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, encoding='utf-8')
    p.stdin.write("usi\nisready\n")
    p.stdin.flush()
    while True:
        line = p.stdout.readline()
        if not line or "readyok" in line:
            break

    played_moves = []
    print(f"--- Simulating Self-Play (First {max_ply} moves) ---")
    for ply in range(1, max_ply + 1):
        if played_moves:
            p.stdin.write(f"position startpos moves {' '.join(played_moves)}\n")
        else:
            p.stdin.write("position startpos\n")
        p.stdin.flush()

        p.stdin.write("go movetime 100\n")
        p.stdin.flush()

        bm = None
        while True:
            line = p.stdout.readline()
            if not line:
                break
            line = line.strip()
            if line.startswith("bestmove"):
                parts = line.split()
                if len(parts) >= 2:
                    bm = parts[1]
                break

        if not bm or bm in ["resign", "draw"]:
            print(f"Game terminated at ply {ply}: {bm}")
            break

        played_moves.append(bm)
        turn_str = "先手" if ply % 2 == 1 else "後手"
        print(f"Move {ply:2d} ({turn_str}): {bm}")

    p.stdin.write("quit\n")
    p.stdin.flush()
    p.terminate()
    return played_moves

moves = run_self_play(24)
print("\nFinal moves string:")
print(" ".join(moves))
