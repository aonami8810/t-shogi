import subprocess
import time

def test_desperado():
    proc = subprocess.Popen(
        ['.\\shogi-usi-morse.exe'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1
    )

    def send_cmd(cmd):
        print(f">> {cmd}")
        proc.stdin.write(cmd + "\n")
        proc.stdin.flush()

    def read_until(stop_pattern, timeout=10):
        start = time.time()
        lines = []
        while time.time() - start < timeout:
            line = proc.stdout.readline()
            if line:
                line_str = line.strip()
                lines.append(line_str)
                print(f"<< {line_str}")
                if any(p in line_str for p in stop_pattern):
                    return lines
            else:
                time.sleep(0.05)
        return lines

    send_cmd("usi")
    read_until(["usiok"])
    send_cmd("isready")
    read_until(["readyok"])

    # Test 1: Critical midgame tactical choice
    # Bishop at 8h has choices in Aigakari / Yokofudori variation:
    print("\n--- Test 1: Midgame Attack Piece Critical Decision ---")
    pos1 = "position startpos moves 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 3a3b 2e2d 2c2d 2h2d 8e8f 8g8f 8b8f P*8g 8f8b 2d3d 4a4b 3d3f 8b8d"
    send_cmd(pos1)
    send_cmd("go btime 0 wtime 0 byoyomi 3000")
    lines1 = read_until(["bestmove"], timeout=8)
    for l in lines1:
        if "bestmove" in l:
            print(f"VERIFICATION: Chosen move = {l}")
            # Ensure it chooses 8h2b+ (capturing Silver) and NOT a low-value pawn move
            assert "8h2b+" in l or "8h2b" in l, f"Unexpected move: {l}"
            print("SUCCESS: Bishop chooses high-value Silver capture!")

    # Test 2: Gote trapped piece situation in GoteGokigen
    print("\n--- Test 2: Gokigen Iron Defense position ---")
    pos2 = "position startpos moves 7g7f 3c3d 2g2f 5c5d 2f2e 8b5b 3g3f 7a6b 3i3h 6b5c 3h3g 5c4d 3g4f 4a3b 2e2d 2c2d 2h2d P*2c 2d2h 5a6b 6i7h 6b7b 5i6h 7b8b 4i4h 6a7b 1g1f 1c1d 2i3g 4d5e 4f5e 5b5e P*5f 5e5a 8h2b+ 3a2b B*5c 5a5b 5c4b+ 3b4b"
    send_cmd(pos2)
    send_cmd("go btime 0 wtime 0 byoyomi 2000")
    lines2 = read_until(["bestmove"], timeout=6)

    send_cmd("quit")
    proc.terminate()

if __name__ == '__main__':
    test_desperado()
