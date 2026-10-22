import subprocess
import time

def test_moves():
    p = subprocess.Popen(
        ['./shogi-usi-morse.exe'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1
    )
    
    # 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 3a3b 2e2d 2c2d 2h2d P*2c 2d2h 8e8f 8g8f 8b8f (Aigakari 16手目)
    commands = [
        "usi\n",
        "isready\n",
        "position startpos moves 7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 3a3b 2e2d 2c2d 2h2d P*2c 2d2h 8e8f 8g8f 8b8f\n",
        "go\n",
        "quit\n"
    ]
    
    for cmd in commands:
        p.stdin.write(cmd)
        p.stdin.flush()
        time.sleep(0.1)
        
    out, err = p.communicate(timeout=5)
    print("=== Midgame Position USI Output ===")
    print(out)

if __name__ == '__main__':
    test_moves()
