import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def simulate_rollout(start_moves, max_plies=75, byoyomi_ms=400):
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

    for ply in range(len(start_moves) + 1, max_plies + 1):
        send(f"position startpos moves {' '.join(moves)}")
        send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
        
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
            break
            
        moves.append(bestmove)
        scores.append(score)
        print(f"Ply {ply:2d}: {bestmove:6s} (eval: {score})", flush=True)

    try:
        send("quit")
        proc.wait(timeout=2)
    except:
        proc.kill()

    return moves, scores

if __name__ == "__main__":
    print("=== Rollout: Kakugawari Sente vs 4a3b and 1c1d ===")
    start = ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "4a3b", "2g2f", "1c1d"]
    m, s = simulate_rollout(start, max_plies=75, byoyomi_ms=400)
    print("M_game1:", m)
