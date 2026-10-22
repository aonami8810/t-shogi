import subprocess
import time

def evaluate_pos(pos_cmd):
    p = subprocess.Popen(
        ['./shogi-usi-morse.exe'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1
    )
    p.stdin.write("usi\n")
    p.stdin.write("isready\n")
    time.sleep(0.05)
    p.stdin.write(f"{pos_cmd}\n")
    p.stdin.write("go\n")
    p.stdin.flush()
    
    bestmove = None
    info_lines = []
    t0 = time.time()
    while time.time() - t0 < 3.0:
        line = p.stdout.readline()
        if not line:
            break
        line = line.strip()
        if line.startswith("info "):
            info_lines.append(line)
        if line.startswith("bestmove "):
            bestmove = line.split()[1]
            break
            
    p.stdin.write("quit\n")
    p.stdin.flush()
    p.kill()
    return bestmove, info_lines

def test_knight_fork():
    print("=" * 70)
    print("TEST: 桂馬の飛び越え急襲・両取り（Knight Fork Drop）")
    print("局面: 後手5一玉(5a), 3一金(3a)。先手持ち駒: 桂(N), 5九玉(5i)。")
    print("期待手: 4三桂 (N*4c) による玉・金両取りフォーク！")
    print("=" * 70)
    
    sfen_knight_fork = "position sfen 4k1g2/9/9/9/9/9/9/9/4K4 b N 1"
    bm, info = evaluate_pos(sfen_knight_fork)
    print(f"Bestmove returned: {bm}")
    for inf in info:
        print(f"  {inf}")
    assert bm == "N*4c", f"Expected N*4c for fork, got {bm}"
    print("\n[SUCCESS] 桂打ちフォーク (N*4c) が完璧に最善手として選定されました！")

if __name__ == '__main__':
    test_knight_fork()
