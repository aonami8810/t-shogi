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

# テスト2: 相手から王手されている時に、相手玉への詰み手(頭金など)がある局面
# 後手: 5一玉(5a), 1九飛(1i) -> 1九飛が5九玉(5i)に横から王手！
# 先手: 5九玉(5i), 5八金(5h) -> 5八金が5二を支えている？
# 先手持ち駒: 金(G)
# もしQEGTBが頭金G*5b(または自玉王手無視)を返したら反則負け！
# 正しくは 5九玉を逃げる(5i4h, 5i6h)か、合駒(G*2i〜G*4i)か、5八金で防ぐ等、自玉の王手を防がなければならない！
sfen = "position sfen 4k4/9/9/9/9/9/9/4G4/r3K4 b G 1"
bm, out = test_engine_move(sfen)
print("Scenario 2 (King in check while having mate on enemy king):")
print("Bestmove:", bm)
for line in out.splitlines():
    if "info" in line or "bestmove" in line:
        print(" ", line)
