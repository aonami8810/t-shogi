import subprocess

def probe_moves(moves):
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

    current = list(moves)
    print(f"Starting from {len(current)} plies:")
    for step in range(12):
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
        print(f"  Ply {len(current)} ({('Gote' if len(current)%2==0 else 'Sente')}): {best}")

    send("quit")
    proc.terminate()

# 24 plies: Sente with Gold at 5h, Gote with in-hand Bishop or open diagonal
# 7g7f 3c3d 5g5f 8c8d 2h5h 8d8e 7i7h 8e8f 8g8f 8b8f P*8g 8f8b 4i5h (Gold to 5h!)
# 7a6b 5i4h 5a4b 3i3h 4b3a 4h3i 3a2b (Mino for both)
# 8h2b+ 3a2b (Bishop exchange!)
# Now both have Bishop in hand!
moves_kakuexchange = [
    "7g7f", "3c3d", "5g5f", "8c8d", "2h5h", "8d8e", "7i7h", "8e8f",
    "8g8f", "8b8f", "P*8g", "8f8b", "4i5h", "7a6b", "5i4h", "5a4b",
    "3i3h", "4a3b", "4h3i", "3b2b", "8h2b+", "3a2b", "6i5i", "6b5c"
]

print("=== KAKU EXCHANGE NAKABISHA FROM PLY 24 ===")
probe_moves(moves_kakuexchange)
