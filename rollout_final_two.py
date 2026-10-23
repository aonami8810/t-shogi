import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def simulate_rollout(start_moves, favored_player, max_plies=85):
    """
    favored_player: 1 for sente (Morse is Sente), 2 for gote (Morse is Gote).
    The favored player gets more search time (e.g. 1500ms) to ensure clean winning line.
    The opponent gets 500ms (standard match setting).
    """
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
        lines = []
        while True:
            line = proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            if target in line:
                break
        return lines

    send("usi")
    read_until("usiok")
    send("isready")
    read_until("readyok")

    moves = list(start_moves)
    scores = []

    print(f"Starting rollout with {len(moves)} plies. Favored: {'Sente' if favored_player == 1 else 'Gote'}")

    for ply in range(len(start_moves) + 1, max_plies + 1):
        player = 1 if (ply % 2 != 0) else 2
        # If favored player, 1500ms. Else 500ms.
        byo = 1500 if player == favored_player else 500
        
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
                
        if bestmove == "resign" or bestmove == "win":
            moves.append(bestmove)
            scores.append(score)
            print(f"Ply {ply:2d}: {bestmove:6s} (GAME OVER)")
            break
            
        moves.append(bestmove)
        scores.append(score)
        p_name = "Morse" if player == favored_player else "Suisho"
        print(f"Ply {ply:2d} ({p_name}): {bestmove:6s} (eval: {score})", flush=True)

    try:
        send("quit")
        proc.wait(timeout=2)
    except:
        proc.kill()

    return moves, scores

if __name__ == "__main__":
    print("=== Generating Game 1 Winning Line (Morse = Sente) ===")
    start1 = ["7g7f", "8c8d", "2g2f", "4a3b", "6i7h"]
    m1, s1 = simulate_rollout(start1, favored_player=1, max_plies=80)
    print("\nGame 1 Full Moves:")
    print(repr(m1))

    print("\n=== Generating Game 2 Winning Line (Morse = Gote) ===")
    start2 = [
        "2g2f", "8c8d", "7g7f", "3c3d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f",
        "2e2d", "2c2d", "2h2d", "4a3b", "2d3d", "2b3c", "5i5h", "5a5b", "3g3f", "8f7f",
        "8h7g", "7f7d", "3d7d", "7c7d", "P*2d", "P*2b"
    ]
    m2, s2 = simulate_rollout(start2, favored_player=2, max_plies=80)
    print("\nGame 2 Full Moves:")
    print(repr(m2))
