import subprocess

def test_sustained_attack_simulation():
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

    # Start from ply 26
    current = [
        "7g7f", "3c3d", "5g5f", "8c8d", "2h5h", "8d8e", "7i7h", "4a3b",
        "5f5e", "5a4b", "3i3h", "7a6b", "5i4h", "6c6d", "4h3i", "3a4a",
        "3i2h", "4b3a", "4i3i", "6b6c", "6i6h", "8e8f", "8g8f", "8b8f",
        "P*8g", "8f8b"
    ]

    print("=== Continuous Midgame Self-Play Test (Plies 27 to 42) ===")
    has_suicide = False
    for step in range(16):
        pos_cmd = "position startpos moves " + " ".join(current)
        send(pos_cmd)
        send("go byoyomi 80")
        best = None
        while True:
            l = proc.stdout.readline()
            if not l: break
            if l.startswith("bestmove"):
                best = l.split()[1]
                break
        if not best or best in ["resign", "win"]:
            break
        current.append(best)
        turn_str = "Sente" if len(current) % 2 == 1 else "Gote"
        print(f"  Ply {len(current):2d} ({turn_str}): {best}")

    send("quit")
    proc.terminate()

if __name__ == "__main__":
    test_sustained_attack_simulation()
