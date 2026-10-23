import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def query_suisho(moves, byoyomi_ms=1000):
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
    def read_until(target):
        lines = []
        while True:
            l = p.stdout.readline()
            if not l: break
            lines.append(l.strip())
            if target in l: break
        return lines

    send("usi")
    read_until("usiok")
    send("isready")
    read_until("readyok")
    if moves:
        send(f"position startpos moves {' '.join(moves)}")
    else:
        send("position startpos")
    send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
    
    bm = None
    score = None
    while True:
        line = p.stdout.readline()
        if not line: break
        line = line.strip()
        if "score cp" in line:
            parts = line.split()
            if "cp" in parts:
                score = parts[parts.index("cp") + 1]
        if line.startswith("bestmove"):
            bm = line.split()[1]
            break
    send("quit")
    p.wait()
    return bm, score

moves = ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "8c8d"]
print(f"Base moves: {' '.join(moves)}")

# 先手・後手双方で水匠5に最善手を聞いて20手進める
for i in range(10):
    # 先手
    bm_sente, s_sente = query_suisho(moves, byoyomi_ms=1000)
    moves.append(bm_sente)
    ply = len(moves)
    print(f"Ply {ply:2d} (Sente): {bm_sente} (eval: {s_sente})")
    
    # 後手
    bm_gote, s_gote = query_suisho(moves, byoyomi_ms=1000)
    moves.append(bm_gote)
    ply = len(moves)
    print(f"Ply {ply:2d} (Gote ): {bm_gote} (eval: {s_gote})")
