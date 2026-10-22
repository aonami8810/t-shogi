import subprocess
import time

def test_deep_pv():
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

    # Test out-of-book deep search:
    pos = "position startpos moves 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 3a3b 2e2d 2c2d 2h2d 8e8f 8g8f 8b8f P*8g 8f8b 2d3d 4a4b 3d3f 8b8d"
    send_cmd(pos)
    send_cmd("go btime 0 wtime 0 byoyomi 4000")
    lines = read_until(["bestmove"], timeout=8)

    send_cmd("quit")
    proc.terminate()

if __name__ == '__main__':
    test_deep_pv()
