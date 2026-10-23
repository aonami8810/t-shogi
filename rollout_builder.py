import subprocess
import sys

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def run_rollout(initial_moves, max_steps=45, byoyomi_ms=1000):
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
            if not line:
                break
            line = line.strip()
            if target in line:
                break

    send("usi")
    read_until("usiok")
    send("isready")
    read_until("readyok")
    
    moves = list(initial_moves)
    history = []
    
    for step in range(max_steps):
        ply = len(moves) + 1
        send(f"position startpos moves {' '.join(moves)}")
        send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
        
        bm = None
        score = None
        while True:
            line = proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            if line.startswith("info") and "score" in line and "pv" in line:
                parts = line.split()
                if "cp" in parts:
                    idx = parts.index("cp")
                    score = parts[idx + 1]
                elif "mate" in parts:
                    idx = parts.index("mate")
                    score = "mate " + parts[idx + 1]
            if line.startswith("bestmove"):
                bm = line.split()[1]
                break
                
        turn_str = "Sente" if ply % 2 == 1 else "Gote"
        print(f"Step {step+1:2d} (Ply {ply:3d} {turn_str}): {bm} (eval: {score})", flush=True)
        if bm in ("resign", "win"):
            break
        moves.append(bm)
        history.append((ply, bm, score))
        
    send("quit")
    try:
        proc.wait(timeout=2)
    except Exception:
        proc.kill()
        
    return moves, history

if __name__ == "__main__":
    print("=== Rollout 1: From Game 1 9th ply (8th ply 9c9d) ===")
    m1 = "7g7f 3c3d 8h2b+ 3a2b 7i8h 2b3c 8h7g 9c9d".split()
    moves1, hist1 = run_rollout(m1, max_steps=40, byoyomi_ms=800)
    print("Rollout 1 moves:", " ".join(moves1))
    
    print("\n=== Rollout 2: From Game 2 32nd ply (31st ply 8a8e) ===")
    m2 = "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8i7g 3c5e P*2b 2a3c 2b2a+ 3a4b 3d8d 5e1i+ 8d8a+ 3c4e 8a8e".split()
    moves2, hist2 = run_rollout(m2, max_steps=40, byoyomi_ms=800)
    print("Rollout 2 moves:", " ".join(moves2))
