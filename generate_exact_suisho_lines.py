from extend_to_mate import create_engine, query

def build_line(name, init_moves, win_side, max_p=75):
    print(f"\nBuilding line: {name} (Winner: {win_side})", flush=True)
    eng = create_engine()
    moves = list(init_moves)
    try:
        while len(moves) < max_p:
            ply = len(moves) + 1
            is_sente = (ply % 2 == 1)
            side = "Sente" if is_sente else "Gote"
            byo = 2000 if side == win_side else 500
            bm, sc = query(eng, moves, byo)
            try:
                sc_int = int(sc)
            except Exception:
                sc_int = 0
            win_score = sc_int if side == win_side else -sc_int
            print(f"Ply {ply:2d} ({side:5s}): {bm:6s} | Score: {win_score}", flush=True)
            if bm in ("resign", "win", None):
                break
            moves.append(bm)
        return moves
    finally:
        eng.stdin.write("quit\n")
        eng.terminate()

if __name__ == "__main__":
    # 1. Game 1: 8手目 7a6b に対する先手 3i4h からのライン
    init1 = ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "8c8d", "2g2f", "7a6b", "3i4h"]
    l1 = build_line("Kakugawari_Suisho_Conquest_8c8d_7a6b_3i4h", init1, "Sente", 75)

    # 2. Game 2: 17手目 3g3f に対する後手 8f7f からのライン
    init2 = ["2g2f", "8c8d", "7g7f", "3c3d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f", 
             "2e2d", "2c2d", "2h2d", "4a3b", "2d3d", "2b3c", "3g3f", "8f7f"]
    l2 = build_line("Gote_Aigakari_Suisho_Conquest_3g3f_8f7f", init2, "Gote", 75)

    with open("exact_two_lines.py", "w", encoding="utf-8") as f:
        f.write(f"line_sente_exact = {l1}\n\n")
        f.write(f"line_gote_exact = {l2}\n")
    print("\nSaved both lines to exact_two_lines.py")
