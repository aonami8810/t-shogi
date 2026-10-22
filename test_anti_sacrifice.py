import subprocess

def test_no_major_sacrifice():
    proc = subprocess.Popen(
        ['./shogi-usi-morse.exe'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1
    )
    def send(c):
        proc.stdin.write(c + "\n")
        proc.stdin.flush()
    def read_until(token):
        while True:
            l = proc.stdout.readline()
            if not l: break
            if token in l: break

    send("usi")
    read_until("usiok")
    send("isready")
    read_until("readyok")

    # Test 1: 相手の陣地（2八など）に紐なしで飛車や竜を突っ込ませる成り捨て局面
    # 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 3a3b 2e2d 2c2d 2h2d P*2c 2d2h
    # 相手の2三には歩、3二には金。もし飛車が 2h2c+ (2三飛成) などと突っ込むと、
    # 相手の3二金でタダで取られて自滅する！
    moves_test1 = [
        "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b",
        "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "8e8f", "8g8f", "8b8f",
        "P*8g", "8f8b"
    ]
    send(f"position startpos moves {' '.join(moves_test1)}")
    send("go byoyomi 150")
    best1 = None
    while True:
        l = proc.stdout.readline()
        if not l: break
        if l.startswith("bestmove"):
            best1 = l.split()[1]
            break

    print("--- Test 1: Rook Sacrifice Check ---")
    print("Engine chose:", best1)
    if best1 in ["2h2c+", "2h2a+", "2h2b+"]:
        print("FAIL: Engine sacrificed Rook blindly!")
    else:
        print("SUCCESS: Engine did NOT sacrifice Rook. Chose healthy move:", best1)

    # Test 2: 角を持っている局面で、相手の駒が利いているマス（例: B*5c や B*4c や B*3b など）にタダで捨てるか
    # 角換わり中盤
    moves_test2 = [
        "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b",
        "8h2b+", "3a2b", "7i8h", "7a6b", "3i4h", "6b7b", "4h3g", "4a3b"
    ]
    send(f"position startpos moves {' '.join(moves_test2)}")
    send("go byoyomi 150")
    best2 = None
    while True:
        l = proc.stdout.readline()
        if not l: break
        if l.startswith("bestmove"):
            best2 = l.split()[1]
            break

    print("\n--- Test 2: Bishop Drop Suicide Check ---")
    print("Engine chose:", best2)
    # Suicidal bishop drops would be dropping onto squares guarded by enemy (e.g. 5c guarded by pawn/silver, 3b, 4b, 6c)
    if best2 in ["B*5c", "B*4b", "B*3b", "B*6c", "B*7c"]:
        print("FAIL: Engine made suicidal Bishop drop!")
    else:
        print("SUCCESS: Engine played safe constructive move:", best2)

    # Test 3: 中飛車中盤での4枚の攻め（銀・桂の進出）
    # 20 plies of Sente Nakabisha
    moves_test3 = [
        "7g7f", "3c3d", "5g5f", "8c8d", "2h5h", "8d8e", "7i7h", "4a3b",
        "5f5e", "5a4b", "3i3h", "7a6b", "5i4h", "6c6d", "4h3i", "3a4a",
        "3i2h", "4b3a", "4i3i", "6b6c", "6i6h", "8e8f", "8g8f", "8b8f",
        "P*8g", "8f8b"
    ]
    send(f"position startpos moves {' '.join(moves_test3)}")
    send("go byoyomi 150")
    best3 = None
    while True:
        l = proc.stdout.readline()
        if not l: break
        if l.startswith("bestmove"):
            best3 = l.split()[1]
            break

    print("\n--- Test 3: Four-Piece Sustained Attack Progression ---")
    print("Engine chose at ply 27:", best3)
    if best3 in ["5h5a+", "5h5b+", "8h2b+"]:
        print("FAIL: Blind major piece suicide!")
    else:
        print("SUCCESS: Proper sustained attack move:", best3)

    send("quit")
    proc.terminate()

if __name__ == "__main__":
    test_no_major_sacrifice()
