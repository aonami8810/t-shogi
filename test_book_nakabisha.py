import subprocess

def test_book_nakabisha():
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

    sente_nakabisha = [
        "7g7f", "3c3d", "5g5f", "8c8d", "2h5h", "8d8e", "7i7h", "4a3b",
        "5f5e", "5a4b", "3i3h", "7a6b", "5i4h", "6c6d", "4h3i", "3a4a",
        "3i2h", "4b3a", "4i3i", "6b6c", "6i6h", "8e8f", "8g8f", "8b8f",
        "P*8g", "8f8b", "5h5f", "7c7d", "6h5h", "8a7c", "1g1f", "1c1d",
        "5h5g", "7c6e", "7h6h", "6e7g+", "6h7g", "8b8d", "5f4f", "6c5d",
        "5e5d", "6d6e", "7g6f"
    ]

    print("=== Testing Sente Nakabisha Book Guidance (up to 43 plies) ===")
    for length in range(20, len(sente_nakabisha)):
        prefix = sente_nakabisha[:length]
        expected = sente_nakabisha[length]
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
    test_book_nakabisha()
