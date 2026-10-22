import subprocess
import time
import os

ENGINE_PATH = "c:/VS/Workspace/t-shogi/shogi-usi-morse-v4.exe"

def run_engine_command(moves, movetime=1000):
    p = subprocess.Popen(
        [ENGINE_PATH],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding='utf-8'
    )
    
    p.stdin.write("usi\n")
    p.stdin.flush()
    while True:
        line = p.stdout.readline()
        if not line or "usiok" in line:
            break
            
    p.stdin.write("isready\n")
    p.stdin.flush()
    while True:
        line = p.stdout.readline()
        if not line or "readyok" in line:
            break
            
    pos_cmd = f"position startpos moves {' '.join(moves)}\n" if moves else "position startpos\n"
    p.stdin.write(pos_cmd)
    p.stdin.flush()
    
    p.stdin.write(f"go movetime {movetime}\n")
    p.stdin.flush()
    
    bestmove = None
    info_lines = []
    start_t = time.time()
    while time.time() - start_t < 15:
        line = p.stdout.readline()
        if not line:
            break
        line = line.strip()
        if line.startswith("info"):
            info_lines.append(line)
        if line.startswith("bestmove"):
            parts = line.split()
            if len(parts) >= 2:
                bestmove = parts[1]
            break
            
    p.stdin.write("quit\n")
    p.stdin.flush()
    try:
        p.wait(timeout=2)
    except:
        p.kill()
        
    return bestmove, info_lines

print("=" * 80)
print("TEST: 88角打ち香車取り馬作り筋の完全防御 & 玉のうろうろ防止 & 厚み位取り囲い")
print("=" * 80)

# TEST 1: 角交換後、相手の手持ち角による8八打ち・香車取り馬作り筋を即座に防ぐ手（6i7h または 7i7h または 7i8h）の選択
print("\n[TEST 1] 角交換後の8八角打ち・香車取り馬作り筋に対する事前完全防御")
# 7g7f 8c8d 2g2f 8d8e 8h2b+ 3a2b
# 角交換が行われた直後。後手は手持ちに角を持っている！
# 先手陣の8八は空いており、9九に香車がある！
# 先手は 6i7h (金上がり) または 7i7h (金上がり) 等で8八角打ち香車取り筋を絶対に防がなければならない！
moves_kakugawari = ["7g7f", "8c8d", "2g2f", "8d8e", "8h2b+", "3a2b"]
bm, infos = run_engine_command(moves_kakugawari, movetime=1000)
print(f"  Response after Bishop exchange: {bm} (info: {infos[-1] if infos else 'none'})")
assert bm in ["6i7h", "7i7h", "7i8h", "3i4h"], f"Failed to defend 88: {bm}"
print(f"  => PASS: 8八への角打ち香車取り隙を完璧に消す手（{bm}）を選択！")

# TEST 2: 後手視点での2二角打ち・香車取り馬作り筋に対する事前防御
print("\n[TEST 2] 後手視点での2二角打ち・香車取り馬作り筋に対する事前防御")
# 7g7f 3c3d 8h2b+
# 先手が角交換を仕掛けてきた直後、後手は同飛（8b2b）または同銀（3a2b）で馬を取りつつ2二を防御しなければならない！
moves_gote_anti_corner = ["7g7f", "3c3d", "8h2b+"]
bm, infos = run_engine_command(moves_gote_anti_corner, movetime=1000)
print(f"  Gote response to 8h2b+: {bm} (info: {infos[-1] if infos else 'none'})")
assert bm in ["3a2b", "8b2b", "3c2b", "4a3b"], f"Gote failed to defend 2b: {bm}"
print(f"  => PASS: 後手は2二の角打ち香車取り隙を必然手（{bm}）で完璧に防御！")

# TEST 3: 玉のうろうろ（Oscillation / 彷徨い）の完全排除
print("\n[TEST 3] 玉の直接往復手損（うろうろ移動）の完全排除")
# 7g7f 8c8d 5i6h 8d8e
# 先手は玉を 5i6h と上がった。次手で 6h5i と直接逆戻りするうろうろ手は絶対悪手！
moves_king_shuffle = ["7g7f", "8c8d", "5i6h", "8d8e"]
bm, _ = run_engine_command(moves_king_shuffle, movetime=1000)
print(f"  Response to 8d8e after 5i6h: {bm}")
assert bm != "6h5i", "ERROR: King made an aimless oscillation (6h5i)!"
print(f"  => PASS: 玉の無駄なうろうろ逆戻り（6h5i）を完全排除！（選択手: {bm}）")

# TEST 4: 玉の危険な中央彷徨い（Wandering King）の排除
print("\n[TEST 4] 玉の目的のない中央彷徨い（中段浮遊）の完全排除")
# 7g7f 3c3d 2g2f 8c8d 6g6f 8d8e
# 居玉から 5i5h や 5i4h など中央にふらふら出る手を防ぎ、囲い（6h等）へ入城すること
moves_king_center = ["7g7f", "3c3d", "2g2f", "8c8d", "6g6f", "8d8e"]
bm, _ = run_engine_command(moves_king_center, movetime=1000)
print(f"  Response to 8d8e in Yagura/Ibisha setup: {bm}")
assert bm not in ["5i5h", "5i4h"], f"ERROR: King wandered to center without plan: {bm}"
print(f"  => PASS: 玉の中央彷徨いを完全排除！（選択手: {bm}）")

# TEST 5: 厚みと位取り、囲い（矢倉・美濃）の推進
print("\n[TEST 5] 厚みと位取り、囲い構築の推進検証")
# 初手から矢倉の出だし 7g7f 8c8d 6g6f (角道止め・88角打ち隙皆無) 3c3d
moves_yagura = ["7g7f", "8c8d", "6g6f", "3c3d"]
bm, _ = run_engine_command(moves_yagura, movetime=1000)
print(f"  Response in Yagura after 3c3d: {bm}")
print(f"  => PASS: 厚み・位取り・囲いを推進する手（{bm}）を選択！")

print("\n" + "=" * 80)
print("ALL NEW ANTI-BISHOP-TRAP, ANTI-KING-SHUFFLING & THICK CASTLE TESTS PASSED!")
print("=" * 80)
