import subprocess

def test_pos(title, moves_str, expected_moves):
    print("=" * 80)
    print(f"TEST: {title}")
    print("=" * 80)
    p = subprocess.Popen(
        ["c:\\VS\\Workspace\\t-shogi\\shogi-usi-morse.exe"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8"
    )
    def send(c):
        p.stdin.write(c + "\n")
        p.stdin.flush()
    def read(t):
        lines = []
        while True:
            l = p.stdout.readline()
            if not l: break
            lines.append(l.strip())
            if t in l: break
        return lines
    
    send("usi")
    read("usiok")
    send("isready")
    read("readyok")
    send(f"position startpos moves {moves_str}")
    send("go depth 3")
    lines = read("bestmove")
    send("quit")
    p.wait()
    
    bm = None
    book_hit = False
    pv = None
    for l in lines:
        if "book hit" in l:
            book_hit = True
            print("  [Book Hit]:", l)
        elif "bestmove" in l:
            bm = l.split()[1]
        elif "info depth" in l:
            pv = l
            
    print(f"Moves: {moves_str}")
    print(f"PV: {pv}")
    print(f"Bestmove: {bm}")
    success = bm in expected_moves
    print(f"Expected one of {expected_moves}: {'PASS' if success else 'FAIL'}")
    print()
    return success

# 1. 先手中飛車：5三歩成直後（相手居玉 5一玉） -> 定跡ヒット or オラクル急所手 5c4b
test_pos("1. 先手中飛車 5三歩成直後（相手居玉）の急所追撃",
         "7g7f 3c3d 5g5f 8c8d 2h5h 8d8e 5f5e 6c6d 5e5d 7a6b 5d5c+ 4a3b",
         ["5c4b"])

# 2. 後手中飛車：5七歩成直後（相手居玉 5九玉） -> 定跡ヒット or オラクル急所手 5g5h
test_pos("2. 後手中飛車 5七歩成直後（相手居玉）の急所追撃",
         "2g2f 5c5d 2f2e 8b5b 3g3f 5d5e 3i4h 5e5f 6i7h 5f5g+ 4i3h",
         ["5g5h"])

# 3. 先手 5三歩成から 4二と（金剥がし）直後、後手が 5a4b（同玉）と取った局面 -> 5i4h（美濃囲い）または急所攻め
test_pos("3. 先手中飛車 金剥がし直後の後手同玉に対する着手",
         "7g7f 3c3d 5g5f 8c8d 2h5h 8d8e 5f5e 6c6d 5e5d 7a6b 5d5c+ 4a3b 5c4b 5a4b",
         ["5i4h", "B*5c", "G*5b", "R*5a"])

# 4. 前回の居飛車飛車先受けテスト（リグレッション防止）
test_pos("4. リグレッション確認：飛車先歩突き (8六歩) に対する同歩 (8g8f)",
         "7g7f 8c8d 2g2f 8d8e 2f2e 8e8f",
         ["8g8f"])

test_pos("5. リグレッション確認：飛車先突破 (8六同飛) に対する歩打ち受け (P*8g)",
         "7g7f 8c8d 2g2f 8d8e 2f2e 8e8f 8g8f 8b8f",
         ["P*8g"])
