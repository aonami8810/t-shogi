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

# 1. Game 1: 8手目 5a4b に対する先手の最善手順（水匠5から学ぶ）
print("=== Learning Sente Kakugawari Line against 5a4b ===")
moves_g1 = ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "8c8d", "8h7g", "5a4b"]
for i in range(15):
    # 先手
    bm_s, s_s = query_suisho(moves_g1, byoyomi_ms=1000)
    moves_g1.append(bm_s)
    print(f"Ply {len(moves_g1):2d} (Sente): {bm_s} (eval: {s_s})")
    # 後手
    bm_g, s_g = query_suisho(moves_g1, byoyomi_ms=1000)
    moves_g1.append(bm_g)
    print(f"Ply {len(moves_g1):2d} (Gote ): {bm_g} (eval: {s_g})")

print("\nFinal Sente Line:", moves_g1)

# 2. Game 2: 13手目 2h2d に対する後手の最善手順（水匠5から学ぶ）
print("\n=== Learning Gote Aigakari Line after 2h2d ===")
moves_g2 = ["2g2f", "8c8d", "7g7f", "3c3d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d"]
for i in range(15):
    # 後手
    bm_g, s_g = query_suisho(moves_g2, byoyomi_ms=1000)
    moves_g2.append(bm_g)
    print(f"Ply {len(moves_g2):2d} (Gote ): {bm_g} (eval: {s_g})")
    # 先手
    bm_s, s_s = query_suisho(moves_g2, byoyomi_ms=1000)
    moves_g2.append(bm_s)
    print(f"Ply {len(moves_g2):2d} (Sente): {bm_s} (eval: {s_s})")

print("\nFinal Gote Line:", moves_g2)
