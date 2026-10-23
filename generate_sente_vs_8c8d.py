import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def get_engine():
    p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8")
    p.stdin.write("usi\n")
    p.stdin.write("isready\n")
    p.stdin.flush()
    while True:
        if "readyok" in p.stdout.readline():
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

# 先手 7g7f, 後手 8c8d, 先手 2g2f
moves = ["7g7f", "8c8d", "2g2f"]

for ply in range(4, 75):
    is_sente = (ply % 2 == 1)
    eng = sente_eng if is_sente else gote_eng
    btime = 1000 if is_sente else 300
    mv, sc = query_move(eng, moves, btime)
    if mv in ["resign", "win", None]:
        print(f"Game over at ply {ply}: {mv}")
        break
    moves.append(mv)
    print(f"Ply {ply:2d} ({'Sente' if is_sente else 'Gote'}): {mv:6s} (eval: {sc})")

print("\nFinal Generated Conquest Line for Sente vs 8c8d:")
print('["' + '", "'.join(moves) + '"]')

sente_eng.stdin.write("quit\n")
gote_eng.stdin.write("quit\n")
sente_eng.terminate()
gote_eng.terminate()
