import subprocess
import time

def test_engine():
    p = subprocess.Popen(
        ['./shogi-usi-morse.exe'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1
    )
    
    commands = [
        "usi\n",
        "isready\n",
        "position startpos moves 7g7f 3c3d\n",
        "go\n",
        "quit\n"
    ]
    
    for cmd in commands:
        p.stdin.write(cmd)
        p.stdin.flush()
        time.sleep(0.1)
        
    out, err = p.communicate(timeout=5)
    print("=== USI Engine Output ===")
    print(out)
    if err:
        print("=== Stderr ===")
        print(err)

if __name__ == '__main__':
    test_engine()
