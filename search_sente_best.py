import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

from extend_to_mate import create_engine, query

def test_sente_move(m9):
    eng = create_engine()
    moves = ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "2b3c", "8h7g", "9c9d", m9]
    try:
        # 後手（水匠5実戦 500ms）と先手（我々 1500ms）で 20手進行
        for ply in range(10, 35):
            is_sente = (ply % 2 == 1)
            byo = 1500 if is_sente else 500
            bm, sc = query(eng, moves, byo)
            try:
                sc_int = int(sc)
            except Exception:
                sc_int = 0
            sente_score = sc_int if is_sente else -sc_int
            moves.append(bm)
        print(f"Move {m9:5s} -> Final ply {len(moves)} eval(Sente): {sente_score}")
        return moves, sente_score
    finally:
        eng.stdin.write("quit\n")
        eng.terminate()

if __name__ == "__main__":
    for m in ["2g2f", "3g3f", "1g1f", "3i4h", "6i7h"]:
        print(f"\n--- Testing 9th move: {m} ---")
        moves, sc = test_sente_move(m)
        print(f"Result for {m}: Sente eval = {sc}")
