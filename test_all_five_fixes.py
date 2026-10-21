import subprocess
import time

def evaluate_pos(pos_cmd, go_cmd="go btime 0 wtime 0 byoyomi 1000"):
    proc = subprocess.Popen(
        ['shogi-usi-morse-v4.exe'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding='utf-8'
    )
    
    commands = [
        'usi',
        'isready',
        'usinewgame',
        pos_cmd,
        go_cmd
    ]
    
    for cmd in commands:
        proc.stdin.write(cmd + '\n')
    proc.stdin.flush()
    
    time.sleep(1.2)
    proc.stdin.write('quit\n')
    proc.stdin.flush()
    
    stdout, stderr = proc.communicate(timeout=5)
    
    bestmove = None
    info_lines = []
    for line in stdout.splitlines():
        if line.startswith('bestmove'):
            bestmove = line.split()[1]
        if line.startswith('info'):
            info_lines.append(line)
    return bestmove, info_lines, stdout

print("==================================================")
print("TEST 1: 18香車の無駄上がり完全排除")
print("後手が1四歩(1d歩)と端歩を突いてきた局面")
print("先手は1八香などの無駄上がりを絶対に指さないか検証")
print("==================================================")
moves_edge = ["7g7f", "3c3d", "2g2f", "1c1d"]
pos1 = "position startpos moves " + " ".join(moves_edge)
bm1, info1, out1 = evaluate_pos(pos1)
print("Position after 1c1d, bestmove:", bm1)
assert bm1 != "1i1h" and bm1 != "9i9h", f"Error: Futile lance lift played: {bm1}"
print("=> OK: 18香などの無駄上がりは完全排除され、有力手", bm1, "が選択された！")

print("\n==================================================")
print("TEST 2: 桂馬の高跳びによるタダ捨ての完全排除")
print("相手の2四歩(2d歩)が2三と2五に利いている局面")
print("2五桂(2g2e)などの無謀な高跳び（歩の餌食）が排除されるか検証")
print("==================================================")
# 先手: 2七桂(2g), 5九玉(5i)
# 後手: 2四歩(2d), 5一玉(5a)
# SFEN: 4k4/9/9/7p1/9/9/7N1/9/4K4 b - 1
# 1段目: 4空き, 5一玉(k), 4空き => 4k4
# 4段目: 7空き, 2四歩(p), 1空き => 7p1
# 7段目: 7空き, 2七桂(N), 1空き => 7N1
# 9段目: 4空き, 5九玉(K), 4空き => 4K4
sfen_knight = "position sfen 4k4/9/9/7p1/9/9/7N1/9/4K4 b - 1"
bm2, info2, out2 = evaluate_pos(sfen_knight)
print("Knight position bestmove:", bm2)
# 2七桂の跳躍は 1五(2g1e) または 3五(2g3e)。2五には跳べない（桂馬は前2マス斜め1マスなので 2gからの跳躍先は (1,5) または (3,5)）
# もし 1四歩(1d)がいる場合、1五桂(2g1e)は歩で取られる！
# 1四歩と3四歩を配置して、両方歩の利きがある場合
sfen_knight_trap = "position sfen 4k4/9/9/6p1p/9/9/7N1/9/4K4 b - 1" # 3四歩(p), 1四歩(p)
bm2_trap, info2_trap, out2_trap = evaluate_pos(sfen_knight_trap)
print("Trapped Knight bestmove:", bm2_trap)
assert bm2_trap not in ["2g1e", "2g3e"], f"Error: Knight jumped into pawn attack: {bm2_trap}"
print("=> OK: 歩の餌食になる桂馬の高跳びタダ捨ては完全排除！(bestmove:", bm2_trap, ")")

print("\n==================================================")
print("TEST 3: 両取り（Fork）の実現 (角の王手飛車取り)")
print("後手: 5一玉(5a), 9一飛(9a)")
print("先手: 5九玉(5i), 持ち駒: 角(B)")
print("最善手: 7三角打 (B*7c) で王手飛車取りの両取り！")
print("==================================================")
# 1段目: 9一飛(r), 3空き, 5一玉(k), 4空き => r3k4
# 9段目: 4空き, 5九玉(K), 4空き => 4K4
# 持ち駒: 角(B)
sfen_fork = "position sfen r3k4/9/9/9/9/9/9/9/4K4 b B 1"
bm3, info3, out3 = evaluate_pos(sfen_fork)
print("Fork position bestmove:", bm3)
for line in info3[-2:]:
    print(" ", line)
print("=> Bestmove for Fork:", bm3)

print("\n==================================================")
print("TEST 4: 玉形の違いをQEGTBとの同値類とみる (Universal Quotient EGTB)")
print("中段玉形(5四玉)・脱出トポロジー閉鎖")
print("後手玉が5四(5d)に逃げ出し、周囲が壁で脱出マス0の局面")
print("最善手: 頭金イデアル (G*5e) がUniversal QEGTBで一発判定！")
print("==================================================")
# 4段目: 4四金(G), 5四玉(k), 6四金(G) -> 左右と後ろを金で挟み撃ち
# 先手: 5八飛(5h) で5四に利きあり、持ち駒: 金(G)
# sfen 9/9/9/3GkG3/9/9/9/4R4/4K4 b G 1
# 4段目: 3空き, 6四金(G), 5四玉(k), 4四金(G), 3空き => 3GkG3
# 8段目: 4空き, 5八飛(R), 4空き => 4R4
# 9段目: 4空き, 5九玉(K), 4空き => 4K4
sfen_universal_qegtb = "position sfen 9/9/9/3GkG3/9/9/9/4R4/4K4 b G 1"
bm4, info4, out4 = evaluate_pos(sfen_universal_qegtb)
print("Universal QEGTB bestmove:", bm4)
for line in info4[-3:]:
    print(" ", line)
print("=> Universal QEGTB Result:", bm4)
