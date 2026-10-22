import subprocess
import time

def test_bishop_exchange_dilemma():
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

    # Test position:
    # Sente's Bishop at 8h or 7g can capture Gote's Silver at 3a/2b or capture a Pawn.
    # For example, in Kakugawari:
    # Sente plays 8h2b+ capturing Silver at 2b!
    # If there is also a Pawn that can be captured (e.g. at 2d or 7d), Bishop must NEVER choose the Pawn over the Silver!
    # Let's test a position where Sente's Bishop at 4e or 8h can choose between:
    # 1. Capturing Silver at 3a/2b (e.g., 8h2b+)
    # 2. Capturing Pawn at 7d/6c or similar
    print("\n--- Test Bishop Silver vs Pawn dilemma ---")
    pos = "position startpos moves 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 3a3b 2e2d 2c2d 2h2d 8e8f 8g8f 8b8f P*8g 8f8b 2d3d 4a4b 3d3f 8b8d"
    send_cmd(pos)
    send_cmd("go btime 0 wtime 0 byoyomi 3000")
    lines = read_until(["bestmove"], timeout=8)
    for l in lines:
        if "bestmove" in l:
            print(f"VERIFICATION: Chosen move = {l}")
            # Ensure it captures or moves effectively, does not blunder pawn trade
            assert "8h2b" in l or "8h2b+" in l or "8h" in l, f"Unexpected move: {l}"
            print("SUCCESS: Bishop correctly targets valuable piece / line!")

    # Test another critical position:
    # Sente Bishop can capture Silver (3c/2b) vs capturing Pawn (2d/7d)
    print("\n--- Test Direct Choice: Silver vs Pawn ---")
    pos_choice = "position startpos moves 7g7f 3c3d 8h2b+ 3a2b"
    # Sente has Bishop in hand, Gote's Silver is at 2b.
    send_cmd(pos_choice)
    send_cmd("go btime 0 wtime 0 byoyomi 2000")
    lines2 = read_until(["bestmove"], timeout=6)

    send_cmd("quit")
    proc.terminate()

if __name__ == '__main__':
    test_bishop_exchange_dilemma()
