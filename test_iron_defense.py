import subprocess
import time

def run_engine_test():
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

    # 1. Initialize
    send_cmd("usi")
    read_until(["usiok"])

    send_cmd("isready")
    read_until(["readyok"])

    # 2. Test Midgame Position: Sente Nakabisha with Iron Mino vs 4-piece attack
    # 7g7f 3c3d 5g5f 8c8d 2h5h 8d8e 7i7h 4a3b 5f5e 5a4b 3i3h 7a6b 5i4h 6c6d 4h3i 3a4a 3i2h 4b3a 4i3i 6b6c 6i6h 8e8f 8g8f 8b8f P*8g 8f8b
    # Let's test move 27 (Sente's move after 8f8b):
    nakabisha_pos = "position startpos moves 7g7f 3c3d 5g5f 8c8d 2h5h 8d8e 7i7h 4a3b 5f5e 5a4b 3i3h 7a6b 5i4h 6c6d 4h3i 3a4a 3i2h 4b3a 4i3i 6b6c 6i6h 8e8f 8g8f 8b8f P*8g 8f8b"
    send_cmd(nakabisha_pos)
    send_cmd("go btime 0 wtime 0 byoyomi 1000")
    lines = read_until(["bestmove"], timeout=8)

    # 3. Test Defense against 4-piece assault:
    # 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 3a3b 2e2d 2c2d 2h2d 8e8f 8g8f 8b8f 2d3d 8f8h+ 7i8h B*4e
    # Gote drops Bishop at 4e attacking Rook and King. Sente must defend with Rook or Interpose!
    yokofu_pos = "position startpos moves 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 3a3b 2e2d 2c2d 2h2d 8e8f 8g8f 8b8f 2d3d 8f8h+ 7i8h B*4e"
    send_cmd(yokofu_pos)
    send_cmd("go btime 0 wtime 0 byoyomi 1000")
    lines = read_until(["bestmove"], timeout=8)

    # 4. Test 1-Tempo Win (Speed Race Advantage):
    # King safe, can press opponent king with check or threat
    # Gote Gokigen Iron Defense position:
    gokigen_pos = "position startpos moves 7g7f 3c3d 2g2f 5c5d 2f2e 8b5b 3g3f 7a6b 3i3h 6b5c 3h3g 5c4d 3g4f 4a3b 2e2d 2c2d 2h2d P*2c 2d2h 5a6b 6i7h 6b7b 5i6h 7b8b 4i4h 6a7b 1g1f 1c1d 2i3g 4d5e 4f5e 5b5e P*5f 5e5a 8h2b+ 3a2b B*5c 5a5b 5c4b+ 3b4b 2h2a+ 2b3a 2a2c 4b3b 2c3b 3a3b"
    send_cmd(gokigen_pos)
    send_cmd("go btime 0 wtime 0 byoyomi 1000")
    lines = read_until(["bestmove"], timeout=8)

    send_cmd("quit")
    proc.terminate()

if __name__ == '__main__':
    run_engine_test()
