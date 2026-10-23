import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def query(moves):
    p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8")
    p.stdin.write("usi\nisready\n")
    p.stdin.flush()
    while True:
        l = p.stdout.readline()
        if "readyok" in l: break
    p.stdin.write(f"position startpos moves {' '.join(moves)}\ngo btime 0 wtime 0 byoyomi 500\n")
    p.stdin.flush()
    bm = None
    score = None
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("info") and "score" in l:
            parts = l.split()
            if "cp" in parts:
                score = parts[parts.index("cp")+1]
            elif "mate" in parts:
                score = "mate " + parts[parts.index("mate")+1]
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    p.stdin.write("quit\n")
    p.stdin.flush()
    p.wait()
    return bm, score

print("=== Sente 3rd move 8h2b+ after 7g7f 8c8d ===")
bm4, sc4 = query(["7g7f", "8c8d", "8h2b+"])
print("Ply 4 (Gote):", bm4, sc4)

bm5, sc5 = query(["7g7f", "8c8d", "8h2b+", bm4, "7i8h"])
print("Ply 6 (Gote):", bm5, sc5)

bm7, sc7 = query(["7g7f", "8c8d", "8h2b+", bm4, "7i8h", bm5, "2g2f"])
print("Ply 8 (Gote):", bm7, sc7)
