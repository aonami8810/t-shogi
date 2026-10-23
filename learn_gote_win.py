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
    send(f"position startpos moves {' '.join(moves)}")
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
        elif "score mate" in line:
            parts = line.split()
            if "mate" in parts:
                score = "mate " + parts[parts.index("mate") + 1]
        if line.startswith("bestmove"):
            bm = line.split()[1]
            break
    send("quit")
    p.wait()
    return bm, score

# 後手相掛かり（2手目 8c8d からの横歩取り・3三角型）
# 22手目 3c5e (5五角) 局面から先手を圧倒する手順
gote_line = ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8i7g', '3c5e']
print("Initial Gote Line (Ply 22):", gote_line)

for i in range(15):
    # 先手
    bm_s, s_s = query_suisho(gote_line, byoyomi_ms=1000)
    gote_line.append(bm_s)
    ply = len(gote_line)
    print(f"Ply {ply:2d} (Sente): {bm_s} (eval: {s_s})")
    
    # 後手
    bm_g, s_g = query_suisho(gote_line, byoyomi_ms=1000)
    gote_line.append(bm_g)
    ply = len(gote_line)
    print(f"Ply {ply:2d} (Gote ): {bm_g} (eval: {s_g})")
    if "mate" in str(s_g) and int(s_g.split()[1]) < 0:
        print("Gote has mate!")

print("\nFinal Gote Line:", gote_line)
