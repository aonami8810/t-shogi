import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def trace(start_moves, max_plies=75, byoyomi=1200):
    p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8", errors="replace")
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
                if "cp" in parts: sc = parts[parts.index("cp")+1]
            elif "score mate" in l:
                parts = l.split()
                if "mate" in parts: sc = "mate " + parts[parts.index("mate")+1]
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        if bm in ["resign", "win"]: break
        moves.append(bm)
        turn_str = "Sente" if ply % 2 == 1 else "Gote "
        print(f"Ply {ply:2d} ({turn_str}): {bm:6s} (eval: {sc})")
    send("quit")
    p.wait()
    return moves

print("=== Tracing Sente Conquest against 7a6b (62 Silver) ===")
branch = ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "7a6b"]
res = trace(branch, max_plies=75, byoyomi=1200)
print("\nFinal Line:", res)
with open("sente_62silver_line.py", "w") as f:
    f.write(f"sente_62silver_line = {res}\n")
