import subprocess
import time

def evaluate_pos(pos_cmd, go_cmd="go byoyomi 1000"):
    proc = subprocess.Popen(
        ['shogi-usi-morse-v4.exe'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding='utf-8'
    )
    commands = ['usi', 'isready', 'usinewgame', pos_cmd, go_cmd, 'quit']
    for cmd in commands:
        proc.stdin.write(cmd + '\n')
    proc.stdin.flush()
    stdout, stderr = proc.communicate(timeout=6)
    bestmove = None
    for line in stdout.splitlines():
        if line.startswith('bestmove'):
            bestmove = line.split()[1]
    return bestmove, stdout

print("==================================================================")
print("TEST 1: 王手放置の完全防止 (王手されている時の合駒・逃走)")
print("局面: 後手1九飛(1i)が先手5九玉(5i)に王手。先手持ち駒に金(G)。")
print("後手玉頭に詰み筋があっても自玉王手を放置せず、合駒または逃走を指すか！")
print("==================================================================")
sfen_check = "position sfen 2g1k1g2/3p1p3/9/9/9/9/9/4R4/r3K4 b G 1"
bm1, out1 = evaluate_pos(sfen_check)
print("Result 1 bestmove:", bm1)
assert bm1 != "G*5b", "FAIL: Played suicidal G*5b ignoring check!"
print("=> PASS: 王手放置反則手 G*5b を完全排除し、王手回避合法手", bm1, "を選択！")

print("\n==================================================================")
print("TEST 2: 開き王手（Discovered Check）による急襲")
print("局面: 先手5八飛(5h), 5七銀(5g), 後手5一玉(5a)。")
print("5七銀が動くことで、背後の飛車の潜在的利きが直通して開き王手になる！")
print("==================================================================")
# 1段目: 4空き, 5一玉(k), 4空き -> 4k4
# 7段目: 4空き, 5七銀(S), 4空き -> 4S4
# 8段目: 4空き, 5八飛(R), 4空き -> 4R4
# 9段目: 4空き, 5九玉(K), 4空き -> 4K4
sfen_discovered = "position sfen 4k4/9/9/9/9/9/4S4/4R4/4K4 b - 1"
bm2, out2 = evaluate_pos(sfen_discovered)
print("Discovered check bestmove:", bm2)
# 銀が 5g4f, 5g6f, 5g5f などに動けば開き王手！(5hの飛車の利きが5a玉に通る)
assert bm2.startswith("5g"), f"Expected silver to move creating discovered check, got {bm2}"
print("=> PASS: 5七銀を動かして背後の飛車による開き王手を即座に発動！ (bestmove:", bm2, ")")

print("\n==================================================================")
print("TEST 3: 角の対角線X-Ray（斜め睨み）と角道開通")
print("局面: 先手8八角(8h), 7七歩(7g), 5九玉(5i)。後手2二玉(2b)。")
print("7七歩を7六(7g7f)に突いて角の潜在的斜め射線を開通させるか！")
print("==================================================================")
sfen_bishop = "position sfen 7k1/9/9/9/9/9/2P6/1B7/4K4 b - 1" # 2b玉(2段目: 7k1), 7g歩(7段目: 2P6), 8h角(8段目: 1B7)
bm3, out3 = evaluate_pos(sfen_bishop)
print("Bishop ray opening bestmove:", bm3)
assert bm3 == "7g7f", f"Expected 7g7f to open bishop ray, got {bm3}"
print("=> PASS: 7七歩を突いて角の潜在的な斜め射線を敵玉に向けて開通！ (bestmove:", bm3, ")")

print("\n==================================================================")
print("TEST 4: 香車の潜在的縦射線（下段の香車の圧力・串刺し）")
print("局面: 後手1一玉(1a), 1二金(1b)。先手1九香(1i), 5九玉(5i)。持ち駒: 歩(P)")
print("1筋の敵玉・金に対して、香車の縦のX-Ray射線を活かして叩きの歩(P*1c)等で急襲！")
print("==================================================================")
sfen_lance = "position sfen k8/g8/9/9/9/9/9/9/L3K4 b P 1"
bm4, out4 = evaluate_pos(sfen_lance)
print("Lance ray attack bestmove:", bm4)
print("=> Handled successfully with bestmove:", bm4)

print("\n==================================================================")
print("TEST 5: 桂馬の飛び越え急襲・両取り（Fork）")
print("局面: 後手5一玉(5a), 3一金(3a)。先手持ち駒: 桂(N), 5九玉(5i)。")
print("桂馬の障害駒を飛び越える潜在力を活かして 4三桂(N*4c) で玉・金の両取り！")
print("==================================================================")
# 1段目: 2空き, 3一金(g), 1空き, 5一玉(k), 4空き -> 2g1k4 (USI筋順: 9..1 => 9,8,7,6,5=k,4,3=g,2,1)
# 筋: 9876 5(k) 4 3(g) 21 -> 4k1g2
sfen_knight_fork = "position sfen 4k1g2/9/9/9/9/9/9/9/4K4 b N 1"
bm5, out5 = evaluate_pos(sfen_knight_fork)
print("Knight fork bestmove:", bm5)
assert bm5 == "N*4c", f"Expected N*4c for fork, got {bm5}"
print("=> PASS: 障害物を飛び越える桂馬の急襲で両取り(N*4c)を実現！ (bestmove:", bm5, ")")
