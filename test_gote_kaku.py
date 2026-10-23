import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def query_pv(moves):
    p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8")
    p.stdin.write("usi\nisready\n")
    p.stdin.flush()
    while True:
        if "readyok" in p.stdout.readline(): break
    p.stdin.write(f"position startpos moves {' '.join(moves)}\ngo btime 0 wtime 0 byoyomi 1000\n")
    p.stdin.flush()
    bm = None
    sc = None
    pv = None
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("info") and "score" in l and "pv" in l:
            pv = l
            parts = l.split()
            if "cp" in parts: sc = parts[parts.index("cp")+1]
            elif "mate" in parts: sc = "mate " + parts[parts.index("mate")+1]
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    p.stdin.write("quit\n")
    p.stdin.flush()
    p.wait()
    return bm, sc, pv

print("=== Gote 6th move after 2g2f 8c8d 7g7f 4a3b 6i7h ===")
# 6手目の選択肢
moves_base = ["2g2f", "8c8d", "7g7f", "4a3b", "6i7h"]
bm, sc, pv = query_pv(moves_base)
print("6th move best:", bm, sc)
print(pv)

# もし後手が 3c3d と突いて、先手が 2f2e と突いたときの後手8手目
moves_8 = ["2g2f", "8c8d", "7g7f", "4a3b", "6i7h", "3c3d", "2f2e"]
bm8, sc8, pv8 = query_pv(moves_8)
print("\n8th move best:", bm8, sc8)
print(pv8)
