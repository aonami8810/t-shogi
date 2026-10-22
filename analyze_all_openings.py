import subprocess
import re

def run_pos(moves):
    cmd = ['c:\\VS\\Workspace\\t-shogi\\shogi-usi-morse-v4.exe']
    p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    pos_cmd = 'position startpos moves ' + ' '.join(moves) + '\n'
    p.stdin.write('usi\nisready\n' + pos_cmd + 'go byoyomi 1000\n')
    p.stdin.flush()
    bestmove = None
    info = []
    while True:
        l = p.stdout.readline()
        if not l: break
        l_str = l.strip()
        if l_str.startswith('info '):
            info.append(l_str)
        if l_str.startswith('bestmove '):
            bestmove = l_str.split()[1]
            break
    p.kill()
    return bestmove, info

with open('GenerateMasterMorseBook.hs', 'r', encoding='utf-8') as f:
    content = f.read()

pattern = r'\(\s*"([^"]+)",\s*"([^"]+)",\s*([\d\.]+)\s*,\s*\[(.*?)\]\s*\)'
matches = re.findall(pattern, content, re.DOTALL)

for name, ntype, pot, mvs_str in matches:
    mvs = [m.strip().strip('"').lstrip('+') for m in mvs_str.split(',') if m.strip()]
    bm, info = run_pos(mvs)
    last_info = info[-1] if info else "no info"
    print(f"{name:32s} ({len(mvs):2d} mvs, turn: {'Gote' if len(mvs)%2==1 else 'Sente'}): bestmove = {bm} | {last_info}")
