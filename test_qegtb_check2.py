import subprocess

def test_engine_move(pos_cmd):
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
        pos_cmd,
        'go byoyomi 1000',
        'quit'
    ]
    for cmd in commands:
        proc.stdin.write(cmd + '\n')
    proc.stdin.flush()
    stdout, stderr = proc.communicate(timeout=5)
    bestmove = None
    for line in stdout.splitlines():
        if line.startswith('bestmove'):
            bestmove = line.split()[1]
    return bestmove, stdout

# 後手: 3一金(3a), 5一玉(5a), 7一金(7a) -> 左右壁
# 1段目: 2空き, 7一金(g), 1空き, 5一玉(k), 1空き, 3一金(g), 2空き -> 2g1k1g2
# 2段目: 4二歩(4b), 6二歩(6b) -> 4b, 6b壁、5bのみ空き -> 3p1p3
# 8段目: 5八飛(5h) -> 5二(5b)に飛車の利き（支え）あり
# 9段目: 1九飛(1i)が5九玉(5i)に王手！ -> r3K4
# 持ち駒: 先手に金(G)
# QEGTBのUniversal Atama-Kinが G*5b をヒットするか？
# もしヒットして G*5b を指したら、1i飛の王手放置で反則負け！
sfen = "position sfen 2g1k1g2/3p1p3/9/9/9/9/9/4R4/r3K4 b G 1"
bm, out = test_engine_move(sfen)
print("Scenario 3 (QEGTB Atama-Kin triggered while in check!):")
print("Bestmove:", bm)
for line in out.splitlines():
    if "info" in line or "bestmove" in line:
        print(" ", line)
