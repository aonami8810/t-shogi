import subprocess
import time

def test_search_depth_offbook():
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

    # 定跡外の奇手（1六歩、9四歩、1五歩、9五歩...）で本筋から完全に逸脱した中盤
    send("position startpos moves 1g1f 9c9d 1f1e 9d9e 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 4a3b")
    # 思考時間 1000ms
    send("go byoyomi 1000")
    
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

    print("=== Off-Book Depth Trace ===")
    for log in depth_logs:
        print("  ", log)
    print("Total Depths Reached:", len(depth_logs))
    if depth_logs:
        print("Max Depth Reached:", depth_logs[-1].split()[2])
    print("Chosen Bestmove:", bestmove)

    send("quit")
    proc.terminate()

if __name__ == "__main__":
    test_search_depth_offbook()
