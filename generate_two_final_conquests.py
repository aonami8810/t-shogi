import subprocess
import sys

from extend_to_mate import create_engine, query

def build_conquest_line(name, initial_moves, winning_side, max_plies=75):
    print(f"\n=======================================================", flush=True)
    print(f"Building Conquest Line: {name} (Winner: {winning_side})", flush=True)
    print(f"=======================================================", flush=True)
    eng = create_engine()
    moves = list(initial_moves)
    try:
        while len(moves) < max_plies:
            ply = len(moves) + 1
            is_sente = (ply % 2 == 1)
            side = "Sente" if is_sente else "Gote"
            # 勝たせたい側は 2000ms（深層思考）
            # 相手（水匠5）は実戦の 500ms
            byo = 2000 if side == winning_side else 500
            bm, sc = query(eng, moves, byo)
            try:
                sc_int = int(sc)
            except Exception:
                sc_int = 0
            win_score = sc_int if side == winning_side else -sc_int
            print(f"Ply {ply:2d} ({side:5s}): {bm:6s} | Score(Winner): {win_score}", flush=True)
            if bm in ("resign", "win", None):
                print(f"Game finished with {bm} at ply {ply}")
                break
            moves.append(bm)
        return moves
    finally:
        eng.stdin.write("quit\n")
        eng.terminate()

if __name__ == "__main__":
    # 1. Game 1: 8手目 8c8d に対する先手必勝ライン
    init1 = ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "7c7d", "3g3f", "8c8d", "8h7g"]
    line1 = build_conquest_line("Kakugawari_Suisho_Conquest_7c7d_8c8d_8h7g", init1, winning_side="Sente", max_plies=75)

    # 2. Game 2: 15手目 5i5h に対する後手必勝ライン (8f7f)
    init2 = ["2g2f", "8c8d", "7g7f", "3c3d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f", 
             "2e2d", "2c2d", "2h2d", "4a3b", "5i5h", "8f7f"]
    line2 = build_conquest_line("Gote_Aigakari_Suisho_Conquest_5i5h_8f7f", init2, winning_side="Gote", max_plies=75)

    with open("final_conquest_branch_lines.py", "w", encoding="utf-8") as f:
        f.write(f"line_sente_branch = {line1}\n\n")
        f.write(f"line_gote_branch = {line2}\n")
    print("\nSaved both lines to final_conquest_branch_lines.py")
