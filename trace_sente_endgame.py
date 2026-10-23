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
        if line.startswith("bestmove"):
            bm = line.split()[1]
            break
    send("quit")
    p.wait()
    return bm, score

# 先手角換わりの続き（38手目以降）
sente_line = ['7g7f', '3c3d', '8h2b+', '3a2b', '7i8h', '8c8d', '8h7g', '5a4b', '3i3h', '4a3b', '1g1f', '7c7d', '4g4f', '1c1d', '9g9f', '9c9d', '3g3f', '7a7b', '2i3g', '8a7c', '4i4h', '8d8e', '6g6f', '6c6d', '2h2i', '6d6e', '6i7h', '6e6f', '5i5h', '8e8f', '8g8f', '3d3e', '3h4g', 'P*8h', '8i9g', '3e3f', '4g3f', 'P*3d']

for i in range(12):
    # 先手
    bm_s, s_s = query_suisho(sente_line, byoyomi_ms=1000)
    sente_line.append(bm_s)
    print(f"Ply {len(sente_line):2d} (Sente): {bm_s} (eval: {s_s})")
    # 後手
    bm_g, s_g = query_suisho(sente_line, byoyomi_ms=1000)
    sente_line.append(bm_g)
    print(f"Ply {len(sente_line):2d} (Gote ): {bm_g} (eval: {s_g})")

print("\nExtended Sente Line:", sente_line)
