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

moves = ['7g7f', '3c3d', '8h2b+', '3a2b', '7i8h', '8c8d', '8h7g', '5a4b', '3i3h', '4a3b', '1g1f', '7c7d', '4g4f', '1c1d', '9g9f', '9c9d', '3g3f', '7a7b', '2i3g', '8a7c', '4i4h', '8d8e', '6g6f', '6c6d', '2h2i', '6d6e', '6i7h', '6e6f', '5i5h', '8e8f', '8g8f', '3d3e', '3h4g', 'P*8h', '8i9g', '3e3f', '4g3f', 'P*3d', '9g8e', '7c8e', '7g6f', '2b3c', '3f4e', '8e9g+', '9i9g', '8b8f', 'B*6d', 'B*8g', '6d8f', '8g7h+', 'P*6b', '6a7a', '4e5d', 'N*4a', '3g4e', '3c4d', 'N*6e', 'G*6c', '6e5c+', '4a5c', '4e5c+', '6c5c']

for i in range(15):
    # 先手
    bm_s, s_s = query_suisho(moves, byoyomi_ms=1000)
    moves.append(bm_s)
    ply = len(moves)
    print(f"Ply {ply:2d} (Sente): {bm_s} (eval: {s_s})")
    if "mate" in str(s_s) and int(s_s.split()[1]) > 0:
        print("Sente has mate!")
    # 後手
    bm_g, s_g = query_suisho(moves, byoyomi_ms=1000)
    moves.append(bm_g)
    ply = len(moves)
    print(f"Ply {ply:2d} (Gote ): {bm_g} (eval: {s_g})")

print("\nFinal Complete Sente Conquest:", moves)
