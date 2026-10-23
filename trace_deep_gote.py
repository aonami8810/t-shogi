import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def run_deep_query(moves, byoyomi=1500):
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

# 13手目までの相掛かり
base = ['2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '4a3b', '3i3h', '7a7b', '2e2d', '2c2d', '2h2d', 'P*2c', '2d2h']

# 後手番からの展開（深い読みで後手の最善手順を追う）
curr = list(base)
print("=== Tracing Gote Line ===")
for step in range(25):
    # 後手の手番
    bm_g, sc_g = run_deep_query(curr, byoyomi=2000)
    curr.append(bm_g)
    ply = len(curr)
    print(f"Ply {ply:2d} (Gote ): {bm_g:6s} (eval: {sc_g})")
    
    # 先手（水匠5）の応手
    bm_s, sc_s = run_deep_query(curr, byoyomi=1000)
    curr.append(bm_s)
    ply = len(curr)
    print(f"Ply {ply:2d} (Sente): {bm_s:6s} (eval: {sc_s})")

print("\nFinal Gote Line:", curr)
