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

sente_eng = get_engine()
gote_eng = get_engine()

# 14手目 4a3b（3二金）
moves = [
    "2g2f", "8c8d", "2f2e", "8d8e", "7g7f", "3c3d", "6i7h",
    "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b"
]

for ply in range(15, 80):
    is_sente = (ply % 2 == 1)
    eng = sente_eng if is_sente else gote_eng
    btime = 300 if is_sente else 800
    mv, sc = query_move(eng, moves, btime)
    if mv in ["resign", "win", None]:
        print(f"Game over at ply {ply}: {mv}")
        break
    moves.append(mv)
    print(f"Ply {ply:2d} ({'Sente' if is_sente else 'Gote'}): {mv:6s} (eval: {sc})")

print("\nFinal Generated Conquest Line for Gote (Transposition 2f2e...4a3b):")
print('["' + '", "'.join(moves) + '"]')

sente_eng.stdin.write("quit\n")
gote_eng.stdin.write("quit\n")
sente_eng.terminate()
gote_eng.terminate()
