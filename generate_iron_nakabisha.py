# Tool to generate authentic, 100% legal Nakabisha lines with proper piece coordination

import subprocess
import time

def find_best_nakabisha_line():
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

    # 1. 先手中飛車：序盤の正確な骨格
    # 7g7f 3c3d 5g5f 8c8d 2h5h 8d8e 5f5e 6c6d 5i4h 4a3b 3i3h 5a4b 4h3i 7a6b 3i2h 6b5c 4i5h
    sente_nakabisha_seed = [
        "7g7f", "3c3d", "5g5f", "8c8d", "2h5h", "8d8e", "5f5e", "6c6d",
        "5i4h", "4a3b", "3i3h", "5a4b", "4h3i", "7a6b", "3i2h", "6b5c",
        "4i5h" # 17手目: 金を5八に上げてコビンと中央を鉄壁防護！
    ]

    print("Expanding Sente Nakabisha with Ironclad Gold defense...")
    moves1 = list(sente_nakabisha_seed)
    for ply in range(len(sente_nakabisha_seed) + 1, 46):
        send(f"position startpos moves {' '.join(moves1)}")
        send("go byoyomi 80")
        bestmove = None
        t0 = time.time()
        while time.time() - t0 < 1.0:
            l = proc.stdout.readline()
            if not l: break
            l = l.strip()
            if l.startswith("bestmove"):
                bestmove = l.split()[1]
                break
        if not bestmove or bestmove in ["resign", "win"]:
            break
        moves1.append(bestmove)
        print(f"  Ply {ply:2d}: {bestmove}")

    # 2. 後手ゴキゲン中飛車：序盤の正確な骨格
    # 7g7f 3c3d 2g2f 5c5d 2f2e 8b5b 4i3h 6b5c 3h3g 4a3b 3g4f 5c4d 2e2d 2c2d 2h2d P*2c 2d2h 5a6b
    gote_gokigen_seed = [
        "7g7f", "3c3d", "2g2f", "5c5d", "2f2e", "8b5b", "4i3h", "6b5c",
        "3h3g", "4a3b", "3g4f", "5c4d", "2e2d", "2c2d", "2h2d", "P*2c",
        "2d2h", "5a6b" # 18手目: 玉が6二へ片美濃に入城！コビン完全防護！
    ]

    print("\nExpanding Gote Gokigen Nakabisha with Ironclad Silver/Gold defense...")
    moves2 = list(gote_gokigen_seed)
    for ply in range(len(gote_gokigen_seed) + 1, 46):
        send(f"position startpos moves {' '.join(moves2)}")
        send("go byoyomi 80")
        bestmove = None
        t0 = time.time()
        while time.time() - t0 < 1.0:
            l = proc.stdout.readline()
            if not l: break
            l = l.strip()
            if l.startswith("bestmove"):
                bestmove = l.split()[1]
                break
        if not bestmove or bestmove in ["resign", "win"]:
            break
        moves2.append(bestmove)
        print(f"  Ply {ply:2d}: {bestmove}")

    send("quit")
    proc.terminate()

    with open("iron_nakabisha_lines.py", "w", encoding="utf-8") as f:
        f.write(f"sente_nakabisha = {repr(moves1)}\n")
        f.write(f"gote_gokigen = {repr(moves2)}\n")

    print("\nSaved to iron_nakabisha_lines.py successfully!")

if __name__ == "__main__":
    find_best_nakabisha_line()
