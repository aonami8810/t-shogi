import subprocess
import time

def test_search_depth_quiet():
    proc = subprocess.Popen(
        ['./shogi-usi-morse.exe'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1
    )

    def send(cmd):
        proc.stdin.write(cmd + "\n")
        proc.stdin.flush()

    def read_until(token, timeout=5.0):
        start = time.time()
        lines = []
        while time.time() - start < timeout:
            line = proc.stdout.readline()
            if not line:
                break
            lines.append(line.strip())
            if token in line:
                break
        return lines

    send("usi")
    read_until("usiok")
    send("isready")
    read_until("readyok")

    # 平穏な中盤局面（矢倉の駒組み途上）
    send("position startpos moves 7g7f 8c8d 6g6f 3c3d 7i6h 6c6d 6h7g 7a6b 5g5f 5a4b 4i5h 3a3b 5i6h 4b3c 6h7h 5c5d 7h8h 4c4d 8h7h 8d8e 7h8h 6b5c 2g2f 7c7d 2f2e 8e8f 8g8f 8b8f P*8g 8f8b 4g4f 3b4c 3i4h 4c3b 4h4g 6a5b 2e2d 4a4b")
    # 思考時間 1500ms
    send("go byoyomi 1500")
    
    start_time = time.time()
    depth_logs = []
    bestmove = None
    while time.time() - start_time < 3.0:
        line = proc.stdout.readline()
        if not line:
            break
        line = line.strip()
        if line.startswith("info depth"):
            depth_logs.append(line)
        if line.startswith("bestmove"):
            bestmove = line
            break

    print("=== Quiet Midgame Depth Trace ===")
    for log in depth_logs[:5]:
        print("  ", log)
    if len(depth_logs) > 10:
        print("   ...")
    for log in depth_logs[-5:]:
        print("  ", log)
    print("Total Depths Reached:", len(depth_logs))
    if depth_logs:
        print("Max Depth Reached:", depth_logs[-1].split()[2])
    print("Chosen Bestmove:", bestmove)

    send("quit")
    proc.terminate()

if __name__ == "__main__":
    test_search_depth_quiet()
