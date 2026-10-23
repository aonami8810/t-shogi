import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

from extend_to_mate import create_engine, query

def run():
    eng = create_engine()
    # 4手目に 8d8e と突く角換わりルート
    moves = ["2g2f", "8c8d", "7g7f", "8d8e"]
    try:
        for ply in range(5, 76):
            is_sente = (ply % 2 == 1)
            side = "Sente" if is_sente else "Gote"
            # 先手（水匠5）は実戦の 500ms
            # 後手（我々）は 1500ms で最善手を引く
            byo = 500 if is_sente else 1500
            bm, sc = query(eng, moves, byo)
            try:
                sc_int = int(sc) if sc is not None else 0
            except Exception:
                sc_int = 0
            eval_gote = -sc_int if is_sente else sc_int
            print(f"Ply {ply:2d} ({side:5s}): {bm:6s} (eval: {sc}, GoteEval: {eval_gote})", flush=True)
            if bm in ("resign", "win", None):
                break
            moves.append(bm)
        return moves
    finally:
        eng.stdin.write("quit\n")
        eng.terminate()

if __name__ == "__main__":
    line = run()
    print("Final moves count:", len(line))
    with open("gote_kaku_line.py", "w") as f:
        f.write(f"gote_kaku_line = {line}\n")
