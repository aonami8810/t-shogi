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

# テスト4: ピンされている金
# 後手: 5一飛(5a)
# 先手: 5七金(5g), 5九玉(5i)
# 5七金は縦にピンされている。左右（4g, 6g）に動くと5一飛の利きが5九玉に通って自殺手（反則）！
# 5七金が動くなら 5八(5h), 5六(5f) など縦のライン上のみ。
# 先手が 5g4g や 5g6g を指さないか検証！
sfen_pin = "position sfen 4r4/9/9/9/9/9/4G4/9/4K4 b - 1"
bm, out = test_engine_move(sfen_pin)
print("Scenario 4 (Pinned Gold):")
print("Bestmove:", bm)
for line in out.splitlines():
    if "info" in line or "bestmove" in line:
        print(" ", line)
