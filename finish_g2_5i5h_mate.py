# -*- coding: utf-8 -*-
import ast
import subprocess

with open('g2_5i5h_full.txt', 'r', encoding='utf-8') as f:
    moves = ast.literal_eval(f.read().strip())

p = subprocess.Popen(
    [r'C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe'],
    cwd=r'C:\Suisho5-YaneuraOu-v7.5.0-windows',
    stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True
)
p.stdin.write('usi\nisready\n')
p.stdin.flush()
while "readyok" not in p.stdout.readline(): pass

p.stdin.write(f"position startpos moves {' '.join(moves)}\ngo btime 0 wtime 0 byoyomi 1000\n")
p.stdin.flush()
while True:
    l = p.stdout.readline()
    if 'bestmove' in l:
        bm180 = l.split()[1]
        print('Ply 180 (Morse):', bm180)
        break

moves.append(bm180)
p.stdin.write(f"position startpos moves {' '.join(moves)}\ngo btime 0 wtime 0 byoyomi 500\n")
p.stdin.flush()
while True:
    l = p.stdout.readline()
    if 'bestmove' in l:
        bm181 = l.split()[1]
        print('Ply 181 (Suisho):', bm181)
        break

p.kill()
with open('g2_5i5h_full.txt', 'w', encoding='utf-8') as f:
    f.write(repr(moves))
print('Final g2_5i5h length:', len(moves))
