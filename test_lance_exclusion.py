import subprocess

def test_lance_lift_rules():
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
    read_until('usiok')
    send_cmd('isready')
    read_until('readyok')

    # Test A: 先手居玉の状態で手待ちさせても、1i1h / 9i9h を絶対に指さない
    send_cmd('position startpos moves 7g7f 3c3d')
    send_cmd('go btime 1000 wtime 1000 byoyomi 500')
    resp_a = read_until('bestmove')
    mv_a = [l for l in resp_a if l.startswith('bestmove')][0].split()[1]
    print(f"[Rule Test A] Sente turn 3 move: {mv_a}")
    assert mv_a not in ['1i1h', '9i9h'], f"Violated: Sente played futile lance lift {mv_a}"
    print("  -> Passed! (No futile lance lift for Sente)")

    # Test B: 後手居玉の状態で手待ちさせても、1a1b / 9a9b を絶対に指さない
    send_cmd('position startpos moves 7g7f 3c3d 2g2f')
    send_cmd('go btime 1000 wtime 1000 byoyomi 500')
    resp_b = read_until('bestmove')
    mv_b = [l for l in resp_b if l.startswith('bestmove')][0].split()[1]
    print(f"[Rule Test B] Gote turn 4 move: {mv_b}")
    assert mv_b not in ['1a1b', '9a9b'], f"Violated: Gote played futile lance lift {mv_b}"
    print("  -> Passed! (No futile lance lift for Gote)")

    send_cmd('quit')
    p.wait()
    print("\n=== ALL LANCE LIFT RULE TESTS PASSED! ===")

if __name__ == '__main__':
    test_lance_lift_rules()
