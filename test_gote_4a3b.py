import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def run_match_simulation(start_moves, gote_byo=2500, sente_byo=500, max_plies=75):
    proc = subprocess.Popen(
        [SUISHO_EXE],
        cwd=SUISHO_DIR,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
        errors="replace"
    )
    def send(cmd):
        proc.stdin.write(cmd + "\n")
        proc.stdin.flush()
    def read_until(target):
        while True:
            line = proc.stdout.readline()
            if not line or target in line:
                break

    send("usi")
    read_until("usiok")
    send("isready")
    read_until("readyok")

    moves = list(start_moves)
    for ply in range(len(start_moves) + 1, max_plies + 1):
        is_sente = (ply % 2 != 0)
        byo = sente_byo if is_sente else gote_byo
        send(f"position startpos moves {' '.join(moves)}")
        send(f"go btime 0 wtime 0 byoyomi {byo}")
        
        bestmove = None
        score = None
        while True:
            line = proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            if line.startswith("info") and "score" in line and "pv" in line:
                parts = line.split()
                if "cp" in parts:
                    score = parts[parts.index("cp") + 1]
                elif "mate" in parts:
                    score = "mate " + parts[parts.index("mate") + 1]
            if line.startswith("bestmove"):
                bestmove = line.split()[1]
                break
        if bestmove in ["resign", "win"]:
            moves.append(bestmove)
            print(f"Ply {ply:2d}: {bestmove} (GAME OVER, eval={score})")
            break
        moves.append(bestmove)
        player_name = "Sente(Suisho)" if is_sente else "Gote(Morse)"
        print(f"Ply {ply:2d} ({player_name}): {bestmove:6s} (eval={score})", flush=True)

    try:
        send("quit")
        proc.wait(timeout=2)
    except:
        proc.kill()
    return moves

if __name__ == "__main__":
    print("=== Testing Candidate 1: Gote plays 4a3b at ply 4 ===")
    start1 = ["2g2f", "8c8d", "7g7f", "4a3b"]
    m1 = run_match_simulation(start1, gote_byo=2500, sente_byo=500, max_plies=75)
    print("Moves 1:", repr(m1))
