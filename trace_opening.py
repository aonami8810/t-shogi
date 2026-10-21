import subprocess

def trace_game():
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

    moves = []
    for ply in range(1, 31):
        if moves:
            send_cmd(f"position startpos moves {' '.join(moves)}")
        else:
            send_cmd("position startpos")
        send_cmd("go btime 1000 wtime 1000 byoyomi 500")
        bm = get_bestmove()
        if not bm or bm in ['resign', 'win']:
            print(f"Game ended at ply {ply}: {bm}")
            break
        moves.append(bm)
        side = "Sente" if ply % 2 == 1 else "Gote"
        print(f"Ply {ply:2d} ({side}): {bm}")
        if '1h' in bm or '1b' in bm or '9h' in bm or '9b' in bm:
            print(f"  *** EDGE / LANCE MOVE DETECTED: {bm} ***")

    send_cmd('quit')
    p.wait()

if __name__ == '__main__':
    trace_game()
