import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

G2_CONQUEST_LINE_TRANSPOSED = [
    '2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'P*2b', 'R*8c', 'P*8b', '8c8e+', 'R*2g',
    '3i3h', '2g2d+', '7g3c+', '2a3c', 'B*4f', '2d2e', '8e2e', '3c2e', 'R*2a', 'B*3c',
    '8i7g', '2b2c', 'P*8c', '6a7b', 'P*2d', '2c2d', '3f3e', '3c2b', '3e3d', '7b8c',
    '7i6h', 'R*3f', '3d3c+', '3f3c', '4f2d', '3c3d', '2d4f', '7d7e', '3h2g', '3d7d',
    '2g3f', '7e7f', '7g6e', '2b9i+', '2a2e+', '7f7g+', '7h7g', '9i7g', 'N*4e', '7g8f',
    '6e5c+', '5b6a', 'P*7e', '7d8d', 'P*8e', '8d8e', 'P*8g', '8f9e', '5c6c', 'L*5a',
    '2e2a', 'G*4a', '2a1a', 'P*6b', 'P*3c', '3b4b', 'L*8f', '8e7e', '8f8c+', '6b6c',
    '8c8b', 'P*5f', '5g5f', 'P*5g', '5h6i', 'N*4d', '8b8a', '4d5f', 'N*8f', '5f6h+',
    '6i6h', '5g5h+', '4i5h', '5a5h+', '6h5h', '7e7h+', 'N*6h', '9e8d', 'L*6f', '7a6b',
    'P*5c', 'P*5g', '5h5g', 'P*5f', '5g5f', 'S*2b', '1a2a', 'P*5e', '5f5g', '4c4d',
    '3c3b+', '4b3b', 'P*2c', '4d4e', '3f4e', '2b3c', 'G*5b', '6a7b', '5b4a', '7b8a',
    '4f5e', 'N*6e', '5g4f', '8d7c', '5e7c+', '6b7c', '4a3a', 'B*5g', '4f3g', '7h6h',
    'G*7a', '8a7a', '3a3b', 'N*6a', '2a6a', '7a6a', '5c5b+', '6a5b', 'B*4c', '5b4c',
    '3b3c', '4c5b', '3c4c', '5b6a', '4c5b', '6a5b', 'P*5c', '5b6a', 'S*5b', '6a7a',
    'N*8c', '7a8b', 'G*7b', '8b7b', '5b6a', '7b8b', '6a7b+', '8b7b', 'S*6a', '7b8b',
    '6a7b+', '8b7b', '8c7a+', '7b8c', '6f6e', 'R*3h', '3g2g', '3h2h+', '2g3f', 'L*3e'
]

def verify_line():
    proc = subprocess.Popen(
        [SUISHO_EXE],
        cwd=SUISHO_DIR,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
        errors="replace"
    )
    def send(cmd):
        proc.stdin.write(cmd + "\n")
        proc.stdin.flush()
    def read_until(target):
        lines = []
        while True:
            line = proc.stdout.readline()
            if not line: break
            line = line.strip()
            lines.append(line)
            if target in line: break
        return lines

    send("usi")
    read_until("usiok")
    send("isready")
    read_until("readyok")
    send("usinewgame")

    send(f"position startpos moves {' '.join(G2_CONQUEST_LINE_TRANSPOSED)}")
    send("go btime 0 wtime 0 byoyomi 500")

    bestmove = None
    info_lines = []
    while True:
        line = proc.stdout.readline()
        if not line: break
        line = line.strip()
        if line.startswith("info"):
            info_lines.append(line)
        if line.startswith("bestmove"):
            bestmove = line.split()[1]
            break

    print("Suisho5 response at ply 181 after 180. L*3e:")
    for inf in info_lines[-3:]:
        print(" ", inf)
    print(f"BESTMOVE = {bestmove}")
    
    send("quit")
    try: proc.wait(timeout=2)
    except: proc.kill()

if __name__ == "__main__":
    verify_line()
