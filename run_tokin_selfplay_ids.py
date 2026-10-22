import subprocess

def run_step(moves_str):
    p = subprocess.Popen(
        ["c:\\VS\\Workspace\\t-shogi\\shogi-usi-morse.exe"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8"
    )
    def send(c):
        p.stdin.write(c + "\n")
        p.stdin.flush()
    def read(t):
        lines = []
        while True:
            l = p.stdout.readline()
            if not l: break
            lines.append(l.strip())
            if t in l: break
        return lines
    
    send("usi")
    read("usiok")
    send("isready")
    read("readyok")
    send(f"position startpos moves {moves_str}")
    send("go depth 3")
    lines = read("bestmove")
    send("quit")
    p.wait()
    
    bm = None
    score = None
    for l in lines:
        if "bestmove" in l:
            bm = l.split()[1]
        elif "info depth" in l:
            parts = l.split()
            if "cp" in parts:
                idx = parts.index("cp")
                score = parts[idx + 1]
            elif "mate" in parts:
                idx = parts.index("mate")
                score = f"mate {parts[idx + 1]}"
    return bm, score

def self_play(initial_moves_str, max_steps=16):
    current_moves = initial_moves_str.split()
    print(f"=== Starting IDS Self-Play from {len(current_moves)} plies ===")
    for step in range(max_steps):
        moves_line = " ".join(current_moves)
        bm, sc = run_step(moves_line)
        if not bm or bm == "resign" or bm == "win":
            print(f"Terminal reached at step {step}: {bm}")
            break
        current_moves.append(bm)
        print(f"Ply {len(current_moves)}: {bm} (Score: {sc})")
        if sc and ("mate" in str(sc) or (str(sc).lstrip("-").isdigit() and abs(int(sc)) >= 9000)):
            print("Checkmate or Oracle Mate Ideal reached!")
            # 詰みまであと数手進行させる
            if "mate 1" in str(sc):
                break
    return current_moves

# 1. 先手中飛車の 5三歩成からの自己対局
init_sente = "7g7f 3c3d 5g5f 8c8d 2h5h 8d8e 5f5e 6c6d 5e5d 7a6b 5d5c+ 4a3b"
full_sente = self_play(init_sente, max_steps=12)
print("\nFinal Sente Nakabisha V-Path:")
print(full_sente)

# 2. 後手中飛車の 5七歩成からの自己対局
init_gote = "2g2f 5c5d 2f2e 8b5b 3g3f 5d5e 3i4h 5e5f 6i7h 5f5g+ 4i3h"
full_gote = self_play(init_gote, max_steps=12)
print("\nFinal Gote Gokigen V-Path:")
print(full_gote)
