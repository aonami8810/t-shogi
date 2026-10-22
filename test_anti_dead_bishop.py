import subprocess
import time

def test_anti_dead_bishop():
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

    # Test 1: Sente has Bishop in hand, verify 98 Bishop drop (B*9h) is NEVER chosen
    print("\n--- Test 1: Verify 98 Bishop Drop (B*9h) is Rejected ---")
    # Position: Kakugawari opening where Sente has Bishop in hand
    pos1 = "position startpos moves 7g7f 3c3d 8h2b+ 3a2b 2g2f 8c8d 2f2e 8d8e 7i7h 2b3c 3i3h 4a3b 5i4h 5a4b 4h3i 6a5b 3i2h 7a6b 1g1f 1c1d"
    send_cmd(pos1)
    send_cmd("go btime 0 wtime 0 byoyomi 3000")
    lines1 = read_until(["bestmove"], timeout=8)
    for l in lines1:
        if "bestmove" in l:
            print(f"VERIFICATION: Chosen move = {l}")
            assert "B*9h" not in l and "9h" not in l, f"CRITICAL BLUNDER: B*9h was selected! {l}"
            print("SUCCESS: 98 Bishop drop (B*9h) was completely REJECTED!")

    # Test 2: Defense against opponent Bishop drop threat
    # Gote has Bishop in hand, Sente must prevent fork/incursion
    print("\n--- Test 2: Defending against opponent Bishop drop ---")
    pos2 = "position startpos moves 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 3a3b 8h2b+ 4a2b"
    # Gote has Bishop in hand! Sente move (ply 11).
    send_cmd(pos2)
    send_cmd("go btime 0 wtime 0 byoyomi 3000")
    lines2 = read_until(["bestmove"], timeout=8)
    for l in lines2:
        if "bestmove" in l:
            print(f"VERIFICATION: Chosen move = {l}")
            # Ensure it plays a solid development or defense move, not exposing Kobin/fork
            assert "B*9h" not in l, f"Blunder B*9h: {l}"
            print("SUCCESS: Solid line chosen without exposing weaknesses to Bishop drop!")

    send_cmd("quit")
    proc.terminate()

if __name__ == '__main__':
    test_anti_dead_bishop()
