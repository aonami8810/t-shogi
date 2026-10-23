import subprocess
import sys

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

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
                score = int(parts[parts.index("cp") + 1])
            elif "mate" in parts:
                mate_str = parts[parts.index("mate") + 1]
                score = 30000 if not mate_str.startswith("-") else -30000
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    return bm, score

def find_line(name, initial_moves, winning_side, max_plies=75):
    print(f"\n=======================================================", flush=True)
    print(f"Finding Line: {name} (Winner: {winning_side})", flush=True)
    print(f"=======================================================", flush=True)
    
    eng = create_engine()
    moves = list(initial_moves)
    
    try:
        while len(moves) < max_plies:
            ply = len(moves) + 1
            is_sente = (ply % 2 == 1)
            side = "Sente" if is_sente else "Gote"
            
            if side == winning_side:
                # 勝たせたい側は深層思考（2000ms）
                bm, sc = query(eng, moves, 2000)
            else:
                # 相手（水匠5実戦）は 500ms
                bm, sc = query(eng, moves, 500)
                
            eval_from_winner = sc if side == winning_side else (-sc if sc is not None else 0)
            print(f"Ply {ply:2d} ({side:5s}): {bm:6s} | Eval(Winner): {eval_from_winner}", flush=True)
            
            if bm in ("resign", "win"):
                print(f"Game finished at ply {ply}: {bm}", flush=True)
                break
                
            moves.append(bm)
            
        return moves
    finally:
        eng.stdin.write("quit\n")
        eng.stdin.flush()
        eng.terminate()

if __name__ == "__main__":
    # 1. Game 1: 先手番で後手8手目 9c9d に対する 75手勝勢ライン
    init1 = ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "2b3c", "8h7g", "9c9d"]
    line1 = find_line("Kakugawari_Suisho_Conquest_9c9d", init1, winning_side="Sente", max_plies=75)
    
    # 2. Game 2: 後手番で先手31手目 8a8e に対する 75手勝勢ライン
    init2 = ["2g2f", "8c8d", "7g7f", "3c3d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f", 
             "2e2d", "2c2d", "2h2d", "4a3b", "2d3d", "2b3c", "5i5h", "5a5b", "3g3f", "8f7f", 
             "8i7g", "3c5e", "P*2b", "2a3c", "2b2a+", "3a4b", "3d8d", "5e1i+", "8d8a+", "3c4e", "8a8e"]
    line2 = find_line("Gote_Aigakari_Suisho_Conquest_8a8e", init2, winning_side="Gote", max_plies=75)

    with open("perfect_lines.py", "w", encoding="utf-8") as f:
        f.write(f"line_sente = {line1}\n\n")
        f.write(f"line_gote = {line2}\n")
    print("\nSaved both lines to perfect_lines.py", flush=True)
