import subprocess
import time

def run_search_test():
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

    # Test out-of-book position:
    # 25 plies: Gote tries a sudden aggressive piece sacrifice / attack on 2筋
    # Sente has 4 pieces in defense (Gold, Silver, Knight, Lance)
    pos = "position startpos moves 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 3a3b 2e2d 2c2d 2h2d 8e8f 8g8f 8b8f P*8g 8f8b 2d3d 4a4b 3d3f 8b8d 3f2f 8d2d P*2e 2d2e"
    send_cmd(pos)
    send_cmd("go btime 0 wtime 0 byoyomi 1500")
    lines = read_until(["bestmove"], timeout=10)

    # Test 1-tempo win race (speed calculation):
    # Position where Sente king is safe, and opponent king can be pressured
    # Sente has rook in hand or on board, can take initiative
    pos_speed = "position startpos moves 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 3a3b 8h2b+ 4a2b B*4e 2b3a 4e6c+ 8e8f 8g8f 8b8f P*8g 8f8d 6c5b 8d8f"
    send_cmd(pos_speed)
    send_cmd("go btime 0 wtime 0 byoyomi 1500")
    lines = read_until(["bestmove"], timeout=10)

    send_cmd("quit")
    proc.terminate()

if __name__ == '__main__':
    run_search_test()
