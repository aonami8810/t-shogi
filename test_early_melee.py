import subprocess
import time

def test_early_deviations():
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

    # 5手目自ら離脱パターン例:
    # 1. 7g7f 2. 3c3d 3. 2g2f 4. 8c8d 5. 7g7f... ではなく 5. 8h7g (角頭歩や筋違い角や早仕掛けなど)
    # または 1. 7g7f 2. 3c3d 3. 8h2b+ 4. 4a3b (筋違い角: 5. B*4e)
    # または 1. 7g7f 2. 3c3d 3. 2g2f 4. 8c8d 5. 2f2e 6. 8d8e 7. 6i7h 8. 4a3b 9. 5i6h ...
    # ユーザーの言う「5手目で先手自ら定跡を離脱」
    test_deviations = [
        ("Sujichigai Bishop (筋違い角)", ["7g7f", "3c3d", "8h2b+", "4a3b", "B*4e"]),
        ("Early Fighting 3五歩 (早仕掛け急戦)", ["7g7f", "3c3d", "2g2f", "8c8d", "3g3f"]),
        ("Aggressive Center (原始中飛車/急戦中飛車)", ["7g7f", "3c3d", "5g5f", "8c8d", "5h5f"]),
        ("Demon Killer / Surprise (鬼殺し/奇襲急戦)", ["7g7f", "3c3d", "2g2f", "8c8d", "7g7f", "8d8e", "7i7h"])
    ]

    for name, init_moves in test_deviations:
        print(f"\n=======================================================")
        print(f" Testing: {name}")
        print(f" Initial Moves: {' '.join(init_moves)}")
        print(f"=======================================================")
        
        curr_moves = list(init_moves)
        for ply in range(len(init_moves) + 1, 35):
            cmd = f"position startpos moves {' '.join(curr_moves)}"
            send(cmd)
            send("go btime 100 wtime 100")
            
            t0 = time.time()
            bestmove = ""
            is_book = False
            is_egtb = False
            score_line = ""
            while time.time() - t0 < 1.0:
                l = proc.stdout.readline()
                if not l: break
                l = l.strip()
                if "string book hit" in l: is_book = True
                if "string quotient egtb hit" in l: is_egtb = True
                if "score" in l: score_line = l
                if l.startswith("bestmove"):
                    bestmove = l.split()[1]
                    break
            
            tag = "[BOOK]" if is_book else ("[QEGTB]" if is_egtb else "[SEARCH]")
            print(f"  Ply {ply:2d} ({'Sente' if ply%2!=0 else 'Gote'}): {bestmove:6s} {tag:8s} | {score_line}")
            if not bestmove or bestmove in ["resign", "win"]:
                print(f"  ---> Finished at ply {ply}")
                break
            curr_moves.append(bestmove)

    send("quit")
    proc.terminate()

if __name__ == "__main__":
    test_early_deviations()
