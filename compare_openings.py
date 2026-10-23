import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def play_opening(start_moves, max_plies=40, byoyomi=1000):
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
    scores = []
    for ply in range(len(start_moves) + 1, max_plies + 1):
        send(f"position startpos moves {' '.join(moves)}")
        send(f"go btime 0 wtime 0 byoyomi {byoyomi}")

        bm = None
        sc = 0
        while True:
            l = p.stdout.readline()
            if not l: break
            l = l.strip()
            if "score cp" in l:
                parts = l.split()
                if "cp" in parts:
                    sc = int(parts[parts.index("cp") + 1])
            elif "score mate" in l:
                parts = l.split()
                if "mate" in parts:
                    m_val = parts[parts.index("mate") + 1]
                    sc = 30000 if int(m_val) > 0 else -30000
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break

        if bm in ["resign", "win"]:
            break
        moves.append(bm)
        # 後手番からの視点スコア（後手有利ならマイナス先手スコア = プラス後手スコア）
        gote_sc = -sc if ply % 2 == 1 else sc # 先手手番の評価値scは先手視点、後手手番時の評価値scは後手視点(水匠は手番視点ではなく先手視点か？)
        scores.append((ply, bm, sc))

    send("quit")
    p.wait()
    return moves, scores

# 候補となる後手の戦型
# 1. 2g2f に対する 8c8d (相掛かり)
# 2. 2g2f に対する 3c3d (横歩取り・角換わり)
# 3. 2g2f に対する 5c5d (ゴキゲン中飛車)
# 4. 2g2f に対する 4c4d (四間飛車)
print("Testing openings...")
m1, s1 = play_opening(["2g2f", "8c8d"], 30)
print(f"1. Aigakari final score: {s1[-1]}")

m2, s2 = play_opening(["2g2f", "3c3d", "7g7f", "8c8d"], 30)
print(f"2. Yokofudori/Kakugawari final score: {s2[-1]}")

m3, s3 = play_opening(["2g2f", "5c5d", "7g7f", "5d5e"], 30)
print(f"3. Central Rook final score: {s3[-1]}")
