import subprocess
import time

def test_search_depth():
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

    # 定跡外の中盤複雑局面での探索深さをテスト
    # 局面: 平手から定跡外の変化（相掛かりの激戦中盤）
    send("position startpos moves 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 4a3b 2e2d 2c2d 2h2d 8e8f 8g8f 8b8f 2d3d 8f8h+ 7i8h 2b3c 3d3f")
    # 思考時間 1000ms で探索
    send("go btime 10000 wtime 10000 byoyomi 1000")
    
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

    print("=== Search Depth Trace ===")
    for log in depth_logs[:5]:
        print("  ", log)
    if len(depth_logs) > 10:
        print("   ...")
    for log in depth_logs[-5:]:
        print("  ", log)
    print("Total Depths Reached:", len(depth_logs))
    if depth_logs:
        print("Max Depth:", depth_logs[-1].split()[2])
    print("Chosen Bestmove:", bestmove)

    send("quit")
    proc.terminate()

if __name__ == "__main__":
    test_search_depth()
