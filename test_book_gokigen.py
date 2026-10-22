import subprocess

def test_book_gokigen():
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

    gote_gokigen = [
        "7g7f", "3c3d", "2g2f", "5c5d", "2f2e", "8b5b", "3g3f", "7a6b",
        "3i3h", "6b5c", "3h3g", "5c4d", "3g4f", "4a3b", "2e2d", "2c2d",
        "2h2d", "P*2c", "2d2h", "5a6b", "6i7h", "6b7b", "5i6h", "7b8b",
        "4i4h", "6a7b", "1g1f", "1c1d", "2i3g", "4d5e", "4f5e", "5b5e",
        "P*5f", "5e5a", "8h2b+", "3a2b", "B*5c", "5a5b", "5c4b+", "3b4b",
        "2h2a+", "2b3a", "2a2c", "4b3b", "2c3b", "3a3b", "G*4a"
    ]

    print("=== Testing Gote Gokigen Book Guidance (up to 47 plies) ===")
    for length in range(20, len(gote_gokigen)):
        prefix = gote_gokigen[:length]
        expected = gote_gokigen[length]
        send(f"position startpos moves {' '.join(prefix)}")
        send("go byoyomi 80")
        best = None
        while True:
            l = proc.stdout.readline()
            if not l: break
            if l.startswith("bestmove"):
                best = l.split()[1]
                break
        status = "MATCH!" if best == expected else f"Differs (Got {best}, expected {expected})"
        print(f"  Ply {length+1:2d}: Expected {expected}, Engine played {best} -> {status}")

    send("quit")
    proc.terminate()

if __name__ == "__main__":
    test_book_gokigen()
