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

# テスト1: 先手玉が王手されている局面
# 後手: 5一飛(5a) が 5九玉(5i) に向かって王手をかけている（間は空）
# 先手: 5九玉(5i), 4九金(4i), 持ち駒なし
# 5iの玉は 4八(4h), 5八(5h), 6八(6h), 6九(6i) に逃げられるか、4九金が5八金(4i5h)と合駒/遮断できる
# 決して 1七歩のような無関係な手や、王手を放置する手を指してはいけない！
sfen_check = "position sfen 4r4/9/9/9/9/9/9/9/3GK4 b - 1"
bm, out = test_engine_move(sfen_check)
print("Check scenario 1 (Rook check on 59 King):")
print("Bestmove:", bm)
for line in out.splitlines():
    if "info" in line or "bestmove" in line:
        print(" ", line)
