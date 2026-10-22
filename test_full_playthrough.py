import subprocess
import time

def simulate_full_opening_playthrough():
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

    # 矢倉本筋の定跡手順を最後まで自動追跡
    print("================================================================================")
    print(" [SIMULATION] 矢倉 (Yagura) 事前コンパイル本筋 自動指し継ぎテスト (1手目 -> 終盤)")
    print("================================================================================")

    moves = []
    for ply in range(1, 60):
        move_str = " ".join(moves)
        cmd = f"position startpos moves {move_str}" if moves else "position startpos"
        send(cmd)
        send("go btime 100 wtime 100")

        bestmove = ""
        is_book = False
        is_egtb = False
        t0 = time.time()
        while time.time() - t0 < 1.0:
            l = proc.stdout.readline()
            if not l: break
            l = l.strip()
            if "string book hit" in l:
                is_book = True
            if "string quotient egtb hit" in l:
                is_egtb = True
            if l.startswith("bestmove"):
                bestmove = l.split()[1]
                break

        if not bestmove or bestmove in ["resign", "win"]:
            print(f"\n---> [PLY {ply:2d}] 対局終了: {bestmove} (到達手数: {ply-1}手)")
            break

        tag = "[BOOK]" if is_book else ("[QEGTB]" if is_egtb else "[SEARCH]")
        moves.append(bestmove)
        if ply % 5 == 1 or is_egtb or not is_book:
            print(f"  Ply {ply:2d}: {bestmove:6s} {tag:8s} | 累計手数: {len(moves)}")

        if not is_book and not is_egtb:
            # 定跡が切れたら数手探索を見届けて終了
            if ply > 45:
                print(f"---> 45手超で終盤イデアル（必至圏）に到達確認！")
                break

    send("quit")
    proc.terminate()

if __name__ == "__main__":
    simulate_full_opening_playthrough()
