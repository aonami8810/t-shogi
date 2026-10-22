import subprocess
import time

def verify_exact_stratified_book():
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

    # GenerateStratifiedCompleteBook.hs の baseBookMoves + 延伸手
    import re
    with open('CompiledMorseBook.hs', 'r', encoding='cp932', errors='replace') as f:
        text = f.read()

    # Yagura の最長手順を抽出
    yagura_prefix = ["7g7f", "8c8d", "6g6f", "3c3d", "7i6h", "6c6d", "6h7g", "7a6b", "5g5f", "5a4b"]
    kakugawari_prefix = ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "8h2b+", "3b2b"]
    shikenbisha_prefix = ["7g7f", "3c3d", "6g6f", "8c8d", "2h6h", "8d8e", "8h7g", "6b6c", "4i4h", "7a6b"]

    tests = [
        ("Yagura (矢倉)", yagura_prefix),
        ("Kakugawari (角換わり)", kakugawari_prefix),
        ("Shikenbisha (四間飛車)", shikenbisha_prefix)
    ]

    for name, prefix in tests:
        # Find the longest path matching this prefix in CompiledMorseBook.hs
        longest_seq = []
        for e in text.split('('):
            if '),' in e or ')]' in e:
                moves = re.findall(r'"([0-9a-i+*]{4,5})"', e)
                if len(moves) >= len(prefix) and moves[:len(prefix)] == prefix:
                    if len(moves) > len(longest_seq):
                        longest_seq = moves

        print(f"\n=======================================================")
        print(f" 戦型: {name} (事前コンパイル最長深度: {len(longest_seq)}手)")
        print(f"=======================================================")
        
        # Test plies: 10, 20, 25, 30, 35, 40, 45, 50
        sample_plies = [10, 20, 25, 30, 35, 40, 45, len(longest_seq)-1]
        for ply in sample_plies:
            if ply > len(longest_seq):
                continue
            sub_moves = longest_seq[:ply]
            send(f"position startpos moves {' '.join(sub_moves)}")
            send("go btime 100 wtime 100")
            
            t0 = time.time()
            bestmove = ""
            is_book = False
            is_egtb = False
            score_info = ""
            while time.time() - t0 < 1.0:
                l = proc.stdout.readline()
                if not l: break
                l = l.strip()
                if "string book hit" in l:
                    is_book = True
                if "string quotient egtb hit" in l:
                    is_egtb = True
                if "score" in l:
                    score_info = l
                if l.startswith("bestmove"):
                    bestmove = l
                    elapsed = time.time() - t0
                    break

            status = "★ BOOK HIT (事前コンパイル最近傍イデアル)" if is_book else (
                "◆ QEGTB HIT (商空間終盤即詰み)" if is_egtb else "▲ ENGINE SEARCH"
            )
            print(f" [Ply {ply:2d}] {sub_moves[-1]} -> {bestmove:18s} ({elapsed*1000:5.1f}ms) | {status}")

    send("quit")
    proc.terminate()

if __name__ == "__main__":
    verify_exact_stratified_book()
