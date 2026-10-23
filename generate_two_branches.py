import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def get_engine():
    p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8")
    p.stdin.write("usi\n")
    p.stdin.write("isready\n")
    p.stdin.flush()
    while True:
        line = p.stdout.readline()
        if "readyok" in line:
            break
    return p

def query_move(p, moves, byoyomi_ms):
    if moves:
        p.stdin.write(f"position startpos moves {' '.join(moves)}\n")
    else:
        p.stdin.write("position startpos\n")
    p.stdin.write(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}\n")
    p.stdin.flush()
    bestmove = None
    score = None
    while True:
        line = p.stdout.readline().strip()
        if line.startswith("info") and "score" in line and "pv" in line:
            parts = line.split()
            if "cp" in parts:
                score = parts[parts.index("cp") + 1]
            elif "mate" in parts:
                score = "mate " + parts[parts.index("mate") + 1]
        if line.startswith("bestmove"):
            bestmove = line.split()[1]
            break
    return bestmove, score

# Branch A: Game 1 (7g7f 3c3d 8h2b+ 3a2b 7i8h 8c8d)
# 先手は深めの探索（800ms）で最強手を選択
print("=== Generating Branch A: Sente vs Suisho5 (8c8d at ply 6) ===")
sente_eng = get_engine()
gote_eng = get_engine()
moves_a = ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "8c8d"]
for ply in range(7, 75):
    is_sente = (ply % 2 == 1)
    eng = sente_eng if is_sente else gote_eng
    btime = 800 if is_sente else 300
    mv, sc = query_move(eng, moves_a, btime)
    if mv in ["resign", "win", None]:
        print(f"Game over at ply {ply}: {mv}")
        break
    moves_a.append(mv)
    print(f"Ply {ply:2d} ({'Sente' if is_sente else 'Gote'}): {mv:6s} (eval: {sc})")

print("\nFinal Branch A Moves:")
print('["' + '", "'.join(moves_a) + '"]')
sente_eng.stdin.write("quit\n")
gote_eng.stdin.write("quit\n")
sente_eng.terminate()
gote_eng.terminate()

# Branch B: Game 2 (2g2f 8c8d 2f2e 8d8e 6i7h 4a3b 3i3h 7a7b 9g9f)
# 後手（Morse）は深めの探索（800ms）で最強手を選択
print("\n=== Generating Branch B: Suisho5 vs Gote (9g9f at ply 9) ===")
sente_eng2 = get_engine()
gote_eng2 = get_engine()
moves_b = ["2g2f", "8c8d", "2f2e", "8d8e", "6i7h", "4a3b", "3i3h", "7a7b", "9g9f"]
for ply in range(10, 80):
    is_sente = (ply % 2 == 1)
    eng = sente_eng2 if is_sente else gote_eng2
    btime = 300 if is_sente else 800
    mv, sc = query_move(eng, moves_b, btime)
    if mv in ["resign", "win", None]:
        print(f"Game over at ply {ply}: {mv}")
        break
    moves_b.append(mv)
    print(f"Ply {ply:2d} ({'Sente' if is_sente else 'Gote'}): {mv:6s} (eval: {sc})")

print("\nFinal Branch B Moves:")
print('["' + '", "'.join(moves_b) + '"]')
sente_eng2.stdin.write("quit\n")
gote_eng2.stdin.write("quit\n")
sente_eng2.terminate()
gote_eng2.terminate()
