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

# 1. 先手相掛かりラインで水匠5の後手の手を調査
print("=== Testing Sente Aigakari Line against Suisho5 ===")
base_moves = ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b"]

current_moves = list(base_moves)
print(f"Base moves (16 plies): {' '.join(current_moves)}")

# 17手目（先手の手番）：先手は何を指すべきか？ 3八銀(3i3h) か 7八金(6i7h)
current_moves.append("3i3h")
print(f"Ply 17 (Sente): 3i3h")

for i in range(10):
    bm, score = query_suisho(current_moves, byoyomi_ms=1000)
    ply = len(current_moves) + 1
    current_moves.append(bm)
    print(f"Ply {ply:2d} (Suisho5): {bm} (eval: {score})")
    
    # 次の先手番の手を水匠5に聞く（最善手探索）
    bm_sente, score_sente = query_suisho(current_moves, byoyomi_ms=1000)
    ply = len(current_moves) + 1
    current_moves.append(bm_sente)
    print(f"Ply {ply:2d} (Sente best): {bm_sente} (eval: {score_sente})")
