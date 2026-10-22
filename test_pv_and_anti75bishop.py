import subprocess
import time

def test_engine_pv_and_75bishop():
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

    def read_until(stop_pattern, timeout=12):
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

    # 1. Test PV display on startpos
    print("\n--- Test 1: Startpos Iterative Deepening with full PV ---")
    send_cmd("position startpos")
    send_cmd("go btime 0 wtime 0 byoyomi 1500")
    read_until(["bestmove"], timeout=8)

    # 2. Test 21st move (20 plies played, Sente has Bishop in hand or on board)
    # Standard Kakugawari opening reaching 20 plies:
    # 7g7f 3c3d 8h2b+ 3a2b 2g2f 8c8d 2f2e 8d8e 7i7h 2b3c 3i3h 4a3b 5i4h 5a4b 4h3i 6a5b 3i2h 7a6b 1g1f 1c1d
    # Ply 20: Gote just played 1c1d. Sente turn (Ply 21, 21手目).
    # Sente has Bishop in hand!
    # Does Sente drop Bishop at 7e (B*7e) which is covered by 7c pawn?
    print("\n--- Test 2: Ply 21 (21手目) Kakugawari Bishop drop check ---")
    kakugawari_pos = "position startpos moves 7g7f 3c3d 8h2b+ 3a2b 2g2f 8c8d 2f2e 8d8e 7i7h 2b3c 3i3h 4a3b 5i4h 5a4b 4h3i 6a5b 3i2h 7a6b 1g1f 1c1d"
    send_cmd(kakugawari_pos)
    send_cmd("go btime 0 wtime 0 byoyomi 2000")
    lines = read_until(["bestmove"], timeout=10)
    for l in lines:
        if "bestmove" in l:
            print(f"VERIFICATION: Chosen move = {l}")
            assert "B*7e" not in l, "CRITICAL ERROR: B*7e was selected!"
            print("SUCCESS: B*7e was NOT selected!")

    # 3. Test another 21st move position:
    # 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 3a3b 2e2d 2c2d 2h2d 8e8f 8g8f 8b8f P*8g 8f8b 2d3d 4a4b 3d3f 8b8d
    print("\n--- Test 3: Ply 21 Aigakari / Yokofudori variation ---")
    pos_21 = "position startpos moves 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 3a3b 2e2d 2c2d 2h2d 8e8f 8g8f 8b8f P*8g 8f8b 2d3d 4a4b 3d3f 8b8d"
    send_cmd(pos_21)
    send_cmd("go btime 0 wtime 0 byoyomi 2000")
    lines = read_until(["bestmove"], timeout=10)
    for l in lines:
        if "bestmove" in l:
            print(f"VERIFICATION: Chosen move = {l}")
            assert "B*7e" not in l and "7e" not in l, "ERROR: 75角 was selected!"
            print("SUCCESS: 75角 was NOT selected!")

    send_cmd("quit")
    proc.terminate()

if __name__ == '__main__':
    test_engine_pv_and_75bishop()
