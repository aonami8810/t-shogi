import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def run_query(moves, byoyomi=1000):
    p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8", errors="replace")
    p.stdin.write("usi\nisready\n")
    p.stdin.write(f"position startpos moves {' '.join(moves)}\n")
    p.stdin.write(f"go btime 0 wtime 0 byoyomi {byoyomi}\n")
    p.stdin.flush()
    bm, sc = None, 0
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
    p.stdin.write("quit\n")
    p.stdin.flush()
    p.wait()
    return bm, sc

curr = ['2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '4a3b', '3i3h', '7a7b', '2e2d', '2c2d', '2h2d', 'P*2c', '2d2h', '5a5b', '7g7f', '8e8f', '8g8f', '8b8f', '4g4f', '7c7d', '3h4g', '8a7c', '9g9f', '8f8a', 'P*8g', '6c6d', '4g3f', '3c3d', '7f7e', '7b6c', '7e7d', '6c7d', '3f4e', 'P*7g', '8h7g', '2b7g+', '8i7g', 'P*7f', '7i8h', '7f7g+', '8h7g', '7c6e', '7g6f', '6a6b', '4e3d', '3a2b', 'B*4e', 'B*5d', '4e5f', 'N*7e', '5i4h', '7e8g+', '7h6h', '8g7h', '6h7h', '8a8i+', 'P*7i', 'P*7g', '6f7g', '8i9i', 'N*6f', '5d6c', '6f7d']

print("Continuing Gote line from ply 64...")
for _ in range(15):
    # 後手
    bm_g, sc_g = run_query(curr, byoyomi=1000)
    curr.append(bm_g)
    ply = len(curr)
    print(f"Ply {ply:2d} (Gote ): {bm_g:6s} (eval: {sc_g})")
    if sc_g >= 1500 or sc_g <= -1500:
        print("Decisive victory reached!")
    # 先手
    bm_s, sc_s = run_query(curr, byoyomi=1000)
    curr.append(bm_s)
    ply = len(curr)
    print(f"Ply {ply:2d} (Sente): {bm_s:6s} (eval: {sc_s})")

print("\nComplete Gote Victory Line:", curr)
