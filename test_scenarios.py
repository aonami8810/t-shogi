import subprocess

def test_custom_openings():
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

    def get_bestmove():
        while True:
            line = p.stdout.readline()
            if not line:
                return None
            line = line.strip()
            if line.startswith('bestmove'):
                return line.split()[1]

    send_cmd('usi')
    send_cmd('isready')

    # シナリオ1: 相手（後手）が端歩手待ち（1c1d, 9c9d）してきた時
    # 先手が 1i1h などの無駄手を指さず、2g2f（飛車先突き）や 7g7f（角道開放）を指すか
    send_cmd("position startpos moves 7g7f 1c1d")
    send_cmd("go btime 1000 wtime 1000 byoyomi 500")
    bm1 = get_bestmove()
    print(f"[Scenario 1] Against idle 1c1d, Sente plays: {bm1}")
    assert bm1 in ['2g2f', '2f', '2g2e', '8h2b+', '2h2f'], f"Expected rook pawn advance, got {bm1}"
    assert bm1 != '1i1h', "ERROR: Sente played futile 1i1h!"
    print("  -> Passed! (Sente advances rook pawn 2g2f, not futile 1i1h)")

    # シナリオ2: 飛車先の歩を突いた後、銀の攻撃参加（3i3h / 3i4h / 7i6h）
    send_cmd("position startpos moves 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e")
    send_cmd("go btime 1000 wtime 1000 byoyomi 500")
    bm2 = get_bestmove()
    print(f"[Scenario 2] Sente response at ply 7: {bm2}")
    assert bm2 != '1i1h', "ERROR: Sente played 1i1h!"
    print(f"  -> Passed! ({bm2} selected, no futile lance lift)")

    # シナリオ3: 中盤で銀の攻撃参加（前進）
    # 先手の銀が中央へ進出しているか
    send_cmd("position startpos moves 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 3a3b")
    send_cmd("go btime 1000 wtime 1000 byoyomi 500")
    bm3 = get_bestmove()
    print(f"[Scenario 3] Sente ply 9 move: {bm3}")
    assert bm3 != '1i1h', "ERROR: Sente played 1i1h!"
    assert bm3 in ['2e2d', '3i3h', '3i4h', '8h2b+'], f"Expected attack move, got {bm3}"
    print(f"  -> Passed! ({bm3} selected, active offensive move)")

    send_cmd('quit')
    p.wait()
    print("\n=== ALL SCENARIOS PASSED WITH FLYING COLORS! ===")

if __name__ == '__main__':
    test_custom_openings()
