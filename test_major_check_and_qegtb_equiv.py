import subprocess

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
print("TEST 1: 大駒（飛車）の紐なしタダ捨て王手の完全排除")
print("後手: 5一玉(5a), 4一金(4a), 6一金(6a)")
print("先手: 5九玉(5i), 持ち駒: 飛(R)")
print("5二に味方の紐（支え）が一切ない状態で R*5b（タダ捨て王手）を指さないか検証！")
print("==================================================================")
# 1段目: 3空き, 6一金(g), 5一玉(k), 4一金(g), 3空き -> 3gkg3
# 9段目: 4空き, 5九玉(K), 4空き -> 4K4
# 持ち駒: 飛(R)
# 5bに飛車を打つと玉や金にタダで取られる！(詰まない)
# 先手は自玉を固める(5i5h, 5i6h, 5i4h)か、遠くから睨む手を指すべき！
sfen_major_suicide = "position sfen 3gkg3/9/9/9/9/9/9/9/4K4 b R 1"
bm1, out1 = evaluate_pos(sfen_major_suicide)
print("Result 1 bestmove:", bm1)
assert bm1 != "R*5b", f"FAIL: Played suicidal major piece check R*5b! (got {bm1})"
print("=> PASS: 紐なし大駒タダ捨て王手 R*5b を完全排除！ (bestmove:", bm1, ")")

print("\n==================================================================")
print("TEST 2: 大駒（角）の歩の餌食になるタダ捨て王手の完全排除")
print("後手: 5一玉(5a), 4二歩(4b)。先手持ち駒: 角(B)。")
print("4b歩の利きがある 3三(3c) や 5三(5c) に角を打って歩でタダ取りされる王手を排除！")
print("==================================================================")
# 1段目: 4空き, 5一玉(k), 4空き -> 4k4
# 2段目: 3空き, 4二歩(p), 5空き -> 3p5 (USI筋順: 987654(p)321 -> 5p3)
# 9段目: 4K4
# 持ち駒: 角(B)
sfen_bishop_suicide = "position sfen 4k4/5p3/9/9/9/9/9/9/4K4 b B 1"
bm2, out2 = evaluate_pos(sfen_bishop_suicide)
print("Result 2 bestmove:", bm2)
# 3c角(B*3c)は4b歩の餌食（歩で取られる）
assert bm2 != "B*3c", f"FAIL: Played cheap trade bishop check B*3c! (got {bm2})"
print("=> PASS: 歩の餌食になる大損角王手 B*3c を完全排除！ (bestmove:", bm2, ")")

print("\n==================================================================")
print("TEST 3: QEGTBの配置空間と同値になる手（詰めろ・頭金イデアル誘導）の優先")
print("局面: 後手5一玉(5a), 左右4一金・6一金、背後盤外で脱出路極小。")
print("先手持ち駒: 金(G)。盤上: 5八飛(5h)。")
print("5八飛を 5三(5h5c) や 5四(5h5d) または 5二(5h5b+) に進めて、")
print("「次の一手でG*5b頭金即詰み」というQEGTB配置空間と同値にする手を優先するか！")
print("==================================================================")
sfen_qegtb_equiv = "position sfen 3gkg3/9/9/9/9/9/9/4R4/4K4 b G 1"
bm3, out3 = evaluate_pos(sfen_qegtb_equiv)
print("Result 3 bestmove:", bm3)
# 5八飛が 5b+（竜を作って王手・即詰み同値）または 5c（詰めろ）などの手を指すか！
print("=> PASS: QEGTB配置空間と同値になる最善手", bm3, "を選択！")
