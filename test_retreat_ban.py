import subprocess

def test_gold_retreat_ban():
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

    # 局面：先手中飛車で金が5八にいる。
    # 7g7f 3c3d 5g5f 8c8d 2h5h 8d8e 7i7h 8e8f 8g8f 8b8f P*8g 8f8b 6i5h
    # 7a6b 5i4h 4a3b 4h3i 5a4b 3i2h 6b5c 4i3i 3a2b
    # ここで先手の番。金は 5八 と 3九 にいる。
    # もし金が 5八 -> 4九 や 5八 -> 6九 に戻ったらアウト！
    test_moves = [
        "7g7f", "3c3d", "5g5f", "8c8d", "2h5h", "8d8e", "7i7h", "8e8f",
        "8g8f", "8b8f", "P*8g", "8f8b", "6i5h", "7a6b", "5i4h", "4a3b",
        "4h3i", "5a4b", "3i2h", "6b5c", "4i3i", "3a2b"
    ]
    send(f"position startpos moves {' '.join(test_moves)}")
    send("go byoyomi 150")
    best = None
    while True:
        l = proc.stdout.readline()
        if not l: break
        if l.startswith("bestmove"):
            best = l.split()[1]
            break
    send("quit")
    proc.terminate()

    print(f"Position test: Gold at 5h, 3i.")
    print(f"Engine played: {best}")
    if best in ["5h4i", "5h6i", "3i4i"]:
        print("FAIL: Engine retreated Gold to home square!")
    else:
        print("SUCCESS: Engine did NOT retreat Gold! It played a constructive move:", best)

if __name__ == "__main__":
    test_gold_retreat_ban()
