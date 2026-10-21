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

print("==================================================")
print("TEST 1: 大駒の成り捨て・サクリファイス (Major Piece Sacrifice)")
print("後手玉1一、2一金、1二歩。先手2八飛、1九玉、持ち駒: 金")
print("最善手: 2八飛成 (2h2a+ / 2821+) で金を取って成り捨て王手！")
print("==================================================")
sfen_sac = "position sfen 7gk/8p/9/9/9/9/9/7R1/8K b G 1"
bm, out = evaluate_pos(sfen_sac)
print("Result bestmove:", bm)

print("\n==================================================")
print("TEST 2: 持ち駒の打ち込み (Drop Move Check/Mate)")
print("後手玉1一、先手2八龍、1九玉、持ち駒: 金")
print("最善手: 1二金打 (G*1b / G*12) 頭金で即詰み！")
print("==================================================")
sfen_drop = "position sfen 8k/9/9/9/9/9/9/7+R1/8K b G 1"
bm, out = evaluate_pos(sfen_drop)
print("Result bestmove:", bm)

print("\n==================================================")
print("TEST 3: 数の攻め (Numerical Assault / Break)")
print("後手: 2三歩、2一金（利き1枚）")
print("先手: 2八飛、3四銀、1九玉（利き2枚、2三を集中砲火）")
print("数の攻め成立: nAtt(2) > nDef(1)")
print("最善手: 3四銀成/不成で2三歩を突破 (3d2c / 3d2c+)")
print("==================================================")
sfen_assault = "position sfen 7gk/9/7p1/6S2/9/9/9/7R1/8K b - 1"
bm, out = evaluate_pos(sfen_assault)
print("Result bestmove:", bm)
