import subprocess

def evaluate_all_moves(moves_str):
    p = subprocess.Popen(['./shogi-usi-morse.exe'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, encoding='utf-8')
    p.stdin.write("usi\n")
    p.stdin.flush()
    while True:
        line = p.stdout.readline()
        if not line or "usiok" in line:
            break
    
    p.stdin.write("isready\n")
    p.stdin.flush()
    while True:
        line = p.stdout.readline()
        if not line or "readyok" in line:
            break

    # position moves
    # 各手を指した後の局面を評価するために、各合法手を調べてみる
    # まず合法手一覧を知るために go movetime 10 を投げる
    p.stdin.write(f"position startpos moves {moves_str}\n")
    p.stdin.flush()
    p.stdin.write("go movetime 10\n")
    p.stdin.flush()
    while True:
        line = p.stdout.readline()
        if not line or line.startswith("bestmove"):
            break

    # ここで各手について、position startpos moves {moves_str} {mv} で深さ1の評価を見る
    candidate_moves = ["2h7h", "2g2f", "6i7h", "7h7f", "3i3h", "3h2g", "5g4f", "5g5f", "5i6i"]
    results = {}
    for mv in candidate_moves:
        p.stdin.write(f"position startpos moves {moves_str} {mv}\n")
        p.stdin.flush()
        p.stdin.write("go movetime 50\n")
        p.stdin.flush()
        bm = None
        sc = None
        while True:
            line = p.stdout.readline()
            if not line:
                break
            line = line.strip()
            if "score cp " in line:
                try:
                    parts = line.split("score cp ")
                    sc = int(parts[1].split()[0])
                except:
                    pass
            if line.startswith("bestmove"):
                break
        if sc is not None:
            results[mv] = -sc # 相手手番の評価値なので反転

    p.stdin.write("quit\n")
    p.stdin.flush()
    p.terminate()
    return results

print("--- Test 2 candidates (after 7g7f 3c3d 7f7e 8c8d) ---")
res2 = evaluate_all_moves("7g7f 3c3d 7f7e 8c8d")
for mv, sc in sorted(res2.items(), key=lambda x: -x[1]):
    print(f"Move {mv}: score {sc}")

print("\n--- Test 2 floating rook candidates (after 7g7f 3c3d 7f7e 8c8d 2h7h 8d8e) ---")
res2f = evaluate_all_moves("7g7f 3c3d 7f7e 8c8d 2h7h 8d8e")
for mv, sc in sorted(res2f.items(), key=lambda x: -x[1]):
    print(f"Move {mv}: score {sc}")

print("\n--- Test 1 climbing silver candidates (after 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 4a32 3i3h 7a6b) ---")
res1 = evaluate_all_moves("7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 4a32 3i3h 7a6b")
for mv, sc in sorted(res1.items(), key=lambda x: -x[1]):
    print(f"Move {mv}: score {sc}")
