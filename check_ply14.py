import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

proc = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding='utf-8')
proc.stdin.write('usi\nisready\n')
proc.stdin.flush()
while True:
    l = proc.stdout.readline()
    if 'readyok' in l: break

moves = ['7g7f', '8c8d', '2g2f', '4a3b', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', 'P*2c']
m_str = " ".join(moves)
proc.stdin.write(f'position startpos moves {m_str}\n')
proc.stdin.write('go nodes 5000000\n')
proc.stdin.flush()

while True:
    l = proc.stdout.readline()
    if not l: break
    l = l.strip()
    if 'score' in l: print(l)
    if l.startswith('bestmove'):
        print('BESTMOVE:', l)
        break
proc.kill()
