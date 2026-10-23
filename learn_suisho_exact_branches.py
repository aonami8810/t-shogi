import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def query_pv(moves, byoyomi=1500):
    p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8", errors="replace")
    p.stdin.write("usi\nisready\n")
    p.stdin.write(f"position startpos moves {' '.join(moves)}\n")
    p.stdin.write(f"go btime 0 wtime 0 byoyomi {byoyomi}\n")
    p.stdin.flush()
    bm, sc, pv = None, None, []
    while True:
        l = p.stdout.readline()
        if not l: break
        l = l.strip()
        if "score cp" in l and "pv" in l:
            parts = l.split()
            if "cp" in parts:
                sc = parts[parts.index("cp") + 1]
            if "pv" in parts:
                pv = parts[parts.index("pv") + 1:]
        elif "score mate" in l and "pv" in l:
            parts = l.split()
            if "mate" in parts:
                sc = "mate " + parts[parts.index("mate") + 1]
            if "pv" in parts:
                pv = parts[parts.index("pv") + 1:]
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    p.stdin.write("quit\n")
    p.stdin.flush()
    p.wait()
    return bm, sc, pv

def trace_game(start_moves, max_plies=70, byoyomi=1500):
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

print("=== 1. 先手番: 水匠5の 6手目 7c7d に対する先手最善手順の学習 ===")
sente_branch1 = ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "7c7d"]
sente_line = trace_game(sente_branch1, max_plies=70, byoyomi=1200)

print("\n=== 2. 後手番: 水匠5の 5手目 7g7f に対する後手最善手順の学習 ===")
gote_branch1 = ["2g2f", "8c8d", "2f2e", "8d8e", "7g7f"]
gote_line = trace_game(gote_branch1, max_plies=70, byoyomi=1200)

with open("suisho_exact_branches.py", "w", encoding="utf-8") as f:
    f.write(f"sente_7c7d_line = {sente_line}\n")
    f.write(f"gote_7g7f_line = {gote_line}\n")
print("\nSaved lines to suisho_exact_branches.py")
