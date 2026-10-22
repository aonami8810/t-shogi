import subprocess

def test_sequence(name, moves):
    cmd = ['c:\\VS\\Workspace\\t-shogi\\shogi-usi-morse-v4.exe']
    p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    p.stdin.write("usi\nisready\n")
    p.stdin.flush()
    # read readyok
    while True:
        l = p.stdout.readline()
        if "readyok" in l: break
    
    # send moves one by one and check valid
    cur_moves = []
    for i, m in enumerate(moves):
        cur_moves.append(m)
        p.stdin.write(f"position startpos moves {' '.join(cur_moves)}\n")
        p.stdin.write("go byoyomi 100\n")
        p.stdin.flush()
        bm = None
        has_error = False
        while True:
            l = p.stdout.readline()
            if not l:
                has_error = True
                break
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        if has_error or not bm:
            print(f"FAILED at move {i+1} ({m}): {cur_moves}")
            p.kill()
            return False, i, m
    p.stdin.write("quit\n")
    p.kill()
    return True, len(moves), None

with open('GenerateMasterMorseBook.hs', 'r', encoding='utf-8') as f:
    content = f.read()

import re
pattern = r'\(\s*"([^"]+)",\s*"([^"]+)",\s*([\d\.]+)\s*,\s*\[(.*?)\]\s*\)'
matches = re.findall(pattern, content, re.DOTALL)

for name, ntype, pot, mvs_str in matches:
    mvs = [m.strip().strip('"').lstrip('+') for m in mvs_str.split(',') if m.strip()]
    ok, ply, bad_m = test_sequence(name, mvs)
    if ok:
        print(f"[OK] {name:32s} (all {ply} moves valid)")
    else:
        print(f"[ERROR] {name:32s} failed at ply {ply+1} ({bad_m})")
