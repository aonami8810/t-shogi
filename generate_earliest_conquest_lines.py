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
    # 1. Game 1: 4手目 8d8e に対する先手 8h7g からのライン
    init1 = ["7g7f", "8c8d", "2g2f", "8d8e", "8h7g"]
    l1 = build_line("Kakugawari_Suisho_Conquest_8d8e_8h7g", init1, "Sente", 75)

    # 2. Game 2: 5手目 6i7h に対する後手 8d8e からのライン
    init2 = ["2g2f", "8c8d", "7g7f", "3c3d", "6i7h", "8d8e"]
    l2 = build_line("Gote_Aigakari_Suisho_Conquest_6i7h_8d8e", init2, "Gote", 75)

    with open("earliest_two_lines.py", "w", encoding="utf-8") as f:
        f.write(f"line_sente_early = {l1}\n\n")
        f.write(f"line_gote_early = {l2}\n")
    print("\nSaved both lines to earliest_two_lines.py")
