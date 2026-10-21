import subprocess

def test_engine():
    p = subprocess.Popen(
        ['shogi-usi-morse-v4.exe'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        cwd=r'c:\VS\Workspace\t-shogi'
    )

    def send_cmd(cmd):
        p.stdin.write(cmd + '\n')
        p.stdin.flush()

    def read_until(pattern):
        output = []
        while True:
            line = p.stdout.readline()
            if not line:
                break
            line_str = line.strip()
            output.append(line_str)
            if pattern in line_str:
                break
        return output

    send_cmd('usi')
    print('USI Init:', read_until('usiok'))

    send_cmd('isready')
    print('IsReady:', read_until('readyok'))

    # 1. 矢倉でかつて 9a9b が指されていた局面をテスト
    # 先手 B*1f の後、後手が 9a9b を指さないことを検証
    yagura_moves = "7g7f 8c8d 6g6f 3c3d 7i6h 6c6d 6h7g 7a6b 5g5f 5a4b 4i5h 3a3b 5i6h 4b3c 6h7h 5c5d 7h8h 4c4d 8h7h 8d8e 7h8h 6b5c 2g2f 7c7d 2f2e 8e8f 8g8f 8b8f P*8g 8f8b 4g4f 3b4c 3i4h 4c3b 4h4g 6a5b 2e2d 4a4b B*1f"
    send_cmd(f'position startpos moves {yagura_moves}')
    send_cmd('go btime 10000 wtime 10000 byoyomi 1000')
    yagura_resp = read_until('bestmove')
    print('\n[Test 1] Yagura response after B*1f:')
    for line in yagura_resp:
        print(' ', line)
    
    # bestmove を取得
    best_line = [l for l in yagura_resp if l.startswith('bestmove')][0]
    best_move = best_line.split()[1]
    print(f'-> Picked move: {best_move}')
    assert best_move != '9a9b', "ERROR: 9a9b (futile lance lift) was chosen!"
    print('-> SUCCESS: 9a9b was completely eliminated!')

    # 2. 初期局面からの初手テスト
    send_cmd('position startpos')
    send_cmd('go btime 10000 wtime 10000 byoyomi 1000')
    start_resp = read_until('bestmove')
    print('\n[Test 2] Startpos response:')
    for line in start_resp:
        print(' ', line)
    start_best = [l for l in start_resp if l.startswith('bestmove')][0].split()[1]
    print(f'-> Startpos move: {start_best}')
    assert start_best in ['7g7f', '2g2f'], f"Unexpected opening move: {start_best}"

    # 3. 詰み局面で即座に QEGTB が反応するかテスト (敵玉が 5一 にいて 5三 に飛車の支えがあり先手持ち駒金)
    send_cmd('position sfen 4k4/9/4R4/9/9/9/9/9/9 b G 1')

    send_cmd('go btime 10000 wtime 10000 byoyomi 1000')
    egtb_resp = read_until('bestmove')
    print('\n[Test 3] QEGTB Checkmate response:')
    for line in egtb_resp:
        print(' ', line)
    egtb_best = [l for l in egtb_resp if l.startswith('bestmove')][0].split()[1]
    print(f'-> QEGTB move: {egtb_best}')
    assert egtb_best == 'G*5b', f"Unexpected mate move: {egtb_best}"


    send_cmd('quit')
    p.wait()
    print('\n=== ALL TESTS PASSED SUCCESSFULLY! ===')

if __name__ == '__main__':
    test_engine()
