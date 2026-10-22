import subprocess
import time

def test_usi_tsumero():
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

    def read_until(token, timeout=3.0):
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
    lines = read_until("usiok")
    print("USI Init:", lines[-1] if lines else "None")

    send("isready")
    lines = read_until("readyok")
    print("Ready:", lines[-1] if lines else "None")

    # 1. 初期局面から相掛かり手順
    send("position startpos moves 2g2f 8c8d 2f2e 8d8e")
    send("go btime 1000 wtime 1000 byoyomi 0")
    lines = read_until("bestmove")
    print("Opening (Aigakari 5th ply):", [l for l in lines if l.startswith("bestmove")])

    # 2. 中盤局面（桂打ちフォーク・詰めろ）
    # 例: 先手玉 5i, 後手玉 5a, 持ち駒 G N
    send("position startpos moves 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 4a3b 2e2d 2c2d 2h2d 8e8f 8g8f 8b8f 2d3d")
    send("go btime 1000 wtime 1000 byoyomi 0")
    lines = read_until("bestmove")
    print("Midgame Deviation/Tsumero Test:", [l for l in lines if l.startswith("bestmove")])

    send("quit")
    proc.terminate()

if __name__ == "__main__":
    test_usi_tsumero()
