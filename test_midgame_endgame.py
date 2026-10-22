import subprocess
import time

def test_usi_midgame_to_endgame():
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

    # 矢倉本筋（40手超）の手順を抽出してテスト
    # GenerateStratifiedCompleteBook.hs の 矢倉本筋:
    # 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 4a3b 5i6h 5c5d 4i5h 5a4b 3i4h 3a3b 7i6h 3c3d 5g5f 7a6b 6h7g 6a5b 2h2f 8b8d 2f3f 4c4d 3g3f 4d3e 3f3e 8d8e 3e2d 2c2d 2e2d P*2c 2d2c+ 2b2c P*2d 2c2b R*2a ...
    
    # 実際の CompiledMorseBook の本筋から手順を取得
    import re
    longest_moves = []
    with open('CompiledMorseBook.hs', 'r', encoding='cp932', errors='replace') as f:
        text = f.read()
    for e in text.split('('):
        if '),' in e or ')]' in e:
            moves = re.findall(r'"([0-9a-i+*]{4,5})"', e)
            if len(moves) > len(longest_moves):
                longest_moves = moves

    print(f"Longest sequence found in book: {len(longest_moves)} plies")
    print(f"Sample moves: {' '.join(longest_moves[:10])} ... {' '.join(longest_moves[-5:])}")
    test_plies = [10, 20, 30, 35, len(longest_moves) - 2, len(longest_moves) - 1]
    moves_full = longest_moves

    for ply in test_plies:
        if ply > len(moves_full):
            continue
        sub_moves = moves_full[:ply]
        send(f"position startpos moves {' '.join(sub_moves)}")
        send("go btime 1000 wtime 1000")
        
        t0 = time.time()
        bestmove = None
        info_lines = []
        while time.time() - t0 < 2.0:
            line = proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            if line.startswith("info"):
                info_lines.append(line)
            if line.startswith("bestmove"):
                bestmove = line
                elapsed = time.time() - t0
                break

        print(f"\n[Ply {ply:2d}] Moves so far: {' '.join(sub_moves[-3:]) if len(sub_moves)>=3 else ' '.join(sub_moves)}")
        print(f"         Result: {bestmove} (response time: {elapsed*1000:.1f}ms)")
        if any("string Book hit" in l for l in info_lines):
            print("         Status: [BOOK HIT - 100% Pre-compiled Nearest Ideal Path]")
        else:
            print("         Status: [SEARCH ENGINE - MC-VDR & Inline QEGTB]")
            for il in info_lines[-2:]:
                print(f"                 {il}")

    send("quit")
    proc.terminate()

if __name__ == "__main__":
    test_usi_midgame_to_endgame()
