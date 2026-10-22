import subprocess
import time

def verify_all_openings_continuity():
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

    # 主要5戦型の骨格手順
    openings = {
        "Kakugawari (角換わり)": [
            "7g7f", "3c3d", "8h2b+", "3a2b", "B*4e", "B*7d", "4e7d", "7c7d", "2g2f", "8c8d",
            "2f2e", "8d8e", "6i7h", "4a3b", "5i6h", "5c5d", "4i5h", "5a4b", "3i4h", "6a5b",
            "7i6h", "7a6b", "5g5f", "8a7c", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "8e8f",
            "8g8f", "8b8f", "P*8g", "8f8b", "4g4f", "4b3c", "3g3f", "3b3a", "6h7g", "3c2d",
            "7g6f", "2d3e", "6f5e", "3e4f", "5e4d", "4f4g+", "4d3c+", "4g5h", "3c2c", "B*7a"
        ],
        "Yagura (矢倉)": [
            "7g7f", "8c8d", "6g6f", "3c3d", "7i6h", "6c6d", "6h7g", "7a6b", "5g5f", "5a4b",
            "4i5h", "5c5d", "3i4h", "3a3b", "5i6h", "4a3a", "2g2f", "8d8e", "8h7g", "4c4d",
            "6i7h", "6b5c", "7g8h", "3b4c", "2f2e", "8a7c", "3g3f", "7c8e", "8h7g", "8e7g+",
            "7h7g", "8b8f", "8g8f", "8e8f", "P*8g", "8f8b", "4g4f", "4c3b", "4h4g", "6a5b",
            "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "B*5e", "2h2e", "5e2h+", "2e2h", "G*6i"
        ],
        "Shikenbisha (四間飛車)": [
            "7g7f", "3c3d", "2g2f", "4c4d", "2f2e", "4a3b", "3i4h", "5a4b", "5i6h", "4b3a",
            "4h3g", "3a2b", "6h7h", "8b4b", "7i6h", "7a6b", "6i7i", "6b7b", "7i8h", "7b8b",
            "8h7g", "8a7c", "5g5f", "4d4e", "3g4e", "4b4e", "B*5e", "4e4d", "5e8b+", "7c8b",
            "B*6a", "3b2c", "6a7b+", "4d4h+", "7b8b", "4h5h", "8b9a", "5h6h", "9a9b", "6h7h",
            "9b8a", "7h8h", "8a7a", "8h7g", "7a6a", "7g6g", "6a5a", "6g5g", "5a4a", "5g4g"
        ]
    }

    for name, moves in openings.items():
        print(f"\n=======================================================")
        print(f" Testing Opening: {name}")
        print(f" Full Move Sequence: {len(moves)} plies")
        print(f"=======================================================")
        
        last_book_ply = 0
        hits = 0
        for ply in range(1, len(moves) + 1):
            sub_moves = moves[:ply]
            send(f"position startpos moves {' '.join(sub_moves)}")
            send("go btime 100 wtime 100")
            
            line_out = ""
            is_book = False
            is_egtb = False
            bestmove = ""
            start_t = time.time()
            while time.time() - start_t < 1.0:
                l = proc.stdout.readline()
                if not l: break
                l = l.strip()
                if "string book hit" in l:
                    is_book = True
                if "string quotient egtb hit" in l:
                    is_egtb = True
                if l.startswith("bestmove"):
                    bestmove = l
                    break
            
            if is_book:
                last_book_ply = ply
                hits += 1
            if ply in [10, 20, 30, 35, 40, 45, 50] or is_egtb:
                status = "BOOK HIT" if is_book else ("QEGTB HIT" if is_egtb else "SEARCH")
                print(f" Ply {ply:2d}: {sub_moves[-1]} -> {bestmove:15s} [{status}]")

        print(f" Total Book Hits in Branch: {hits} / {len(moves)} plies")
        print(f" Deepest Book Reach: Ply {last_book_ply}")

    send("quit")
    proc.terminate()

if __name__ == "__main__":
    verify_all_openings_continuity()
