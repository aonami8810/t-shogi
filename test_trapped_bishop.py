import subprocess
import time

def test_en_prise_bishop_silver_vs_pawn():
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

    def read_until(stop_pattern, timeout=8):
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

    # Create a position where Sente Bishop is at 3c (trapped), can capture:
    # 1. Silver at 3b (3c3b+) -> Silver capture (角銀交換)
    # 2. Pawn at 2d or 4d (3c2d or 3c4d) -> Pawn capture (角歩交換)
    # Both 3b, 2d, 4d are defended by Gote's King at 4a or Gold at 5a.
    # Therefore Bishop WILL BE CAPTURED in any case.
    # The question is: Does Sente capture the Silver at 3b, or the Pawn?
    # Sfen:
    # 9 squares per rank, rank 1: lnsgkgsnl, rank 2: 1r5b1, etc.
    # Let's craft SFEN:
    # Rank 1: l n s g k g s n l
    # Rank 2: . r . . . . . b .
    # Rank 3: p p . p p p p p p  (pawn at 3c missing, replaced by B!)
    # But simpler: use moves from startpos:
    # 7g7f 3c3d 8h3c+ (Bishop takes 3c, attacking 3b silver and surrounded by pawns!)
    # In that position, Gote has silver at 3a, king at 5a, pawns at 2c, 4c...
    # Let's test a position where Bishop is already at 4d attacking 3c (silver) and 5c (pawn):
    pos_trapped = "position startpos moves 7g7f 3c3d 2g2f 8c8d 8h4d"
    send_cmd(pos_trapped)
    # From 4d, Bishop attacks:
    # 3c (pawn), 5c (pawn), 3a (silver via ray), etc.
    # Let's see how engine searches:
    send_cmd("go btime 0 wtime 0 byoyomi 2000")
    lines = read_until(["bestmove"], timeout=6)

    send_cmd("quit")
    proc.terminate()

if __name__ == '__main__':
    test_en_prise_bishop_silver_vs_pawn()
