import subprocess

p = subprocess.Popen(['./shogi-usi-morse.exe'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
p.stdin.write('position startpos moves 7g7f 3c3d 6g6f 8c8d 2h5h 8d8e 7i6h 8e8f 8g8f\neval\n')
p.stdin.write('position startpos moves 7g7f 3c3d 6g6f 8c8d 2h5h 8d8e 7i6h 8e8f 5i4h\neval\nquit\n')
p.stdin.flush()
print(p.stdout.read())
