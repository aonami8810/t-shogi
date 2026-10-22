import subprocess

def test_kobin_bishop_safety():
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

    # 局面：角交換後、先手番。相手(Gote)の持ち駒にBishopがある。
    # 先手の金は 5八、玉は 2八(美濃囲い)。
    # 7g7f 3c3d 5g5f 8c8d 2h5h 8d8e 7i7h 8e8f 8g8f 8b8f P*8g 8f8b
    # 8h2b+ 3a2b (角交換！) 6i6h 7a6b 5i4h 5a4b 3i3h 4a3b 4h3i 4b3a 3i2h 5c5d
    # 4i3i 6b5c (26手進んだ中盤の入り口)
    test_moves = [
        "7g7f", "3c3d", "5g5f", "8c8d", "2h5h", "8d8e", "7i7h", "8e8f",
        "8g8f", "8b8f", "P*8g", "8f8b", "8h2b+", "3a2b", "6i6h", "7a6b",
        "5i4h", "5a4b", "3i3h", "4a3b", "4h3i", "4b3a", "3i2h", "5c5d",
        "4i3i", "6b5c"
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

    print(f"Position test: Bishop exchange Nakabisha midgame (Ply 27).")
    print(f"Engine played: {best}")
    if best in ["6h6i", "6h5i", "3i4i", "3i4h"]:
        print("FAIL: Engine made blunder retreating Gold and exposing Kobin!")
    else:
        print("SUCCESS: Engine safely maintained defense or attacked:", best)

if __name__ == "__main__":
    test_kobin_bishop_safety()
