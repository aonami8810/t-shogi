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
    for line in stdout.splitlines():
        if line.startswith('bestmove'):
            bestmove = line.split()[1]
    return bestmove, stdout

print("=== Test 1: Startpos ===")
bm, out = evaluate_pos("position startpos")
print("Startpos bestmove:", bm)

# テスト2: 序盤の進行（飛車先突き・角道オープン・数の攻めの足がかり）
print("\n=== Test 2: Moves sequence ===")
moves = ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e"]
pos = "position startpos moves " + " ".join(moves)
bm, out = evaluate_pos(pos)
print("Position after 2f2e, bestmove:", bm)
