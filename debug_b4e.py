import subprocess
import time

def debug_eval_at_b4e():
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
            if not line: break
            lines.append(line.strip())
            if token in line: break
        return lines

    send("usi")
    read_until("usiok")
    send("isready")
    read_until("readyok")

    # 5手目 筋違い角 B*4e 直後（後手の手番）
    send("position startpos moves 7g7f 3c3d 8h2b+ 4a3b B*4e")
    send("go byoyomi 1000")
    
    t0 = time.time()
    while time.time() - t0 < 2.0:
        line = proc.stdout.readline()
        if not line: break
        line = line.strip()
        print(line)
        if line.startswith("bestmove"):
            break

    send("quit")
    proc.terminate()

if __name__ == "__main__":
    debug_eval_at_b4e()
