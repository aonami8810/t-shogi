import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

from extend_to_mate import create_engine, query

def run_sente():
    eng = create_engine()
    moves = ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "2b3c", "8h7g", "9c9d", "6i7h"]
    try:
        for ply in range(10, 85):
            is_sente = (ply % 2 == 1)
            side = "Sente" if is_sente else "Gote"
            # 先手（我々）は 2000ms で最善手
            # 後手（水匠5）は 500ms
            byo = 2000 if is_sente else 500
            bm, sc = query(eng, moves, byo)
            try:
                sc_int = int(sc)
            except Exception:
                sc_int = 0
            sente_score = sc_int if is_sente else -sc_int
            print(f"Ply {ply:2d} ({side:5s}): {bm:6s} (eval: {sc}, SenteScore: {sente_score})", flush=True)
            if bm in ("resign", "win", None):
                break
            moves.append(bm)
        return moves
    finally:
        eng.stdin.write("quit\n")
        eng.terminate()

if __name__ == "__main__":
    line = run_sente()
    print("\nFinal Sente Line moves:", len(line))
    with open("sente_6i7h_line.py", "w") as f:
        f.write(f"sente_6i7h_line = {line}\n")
