# Generator and interactive validator using Haskell engine
import subprocess
import json

def get_legal_moves(moves):
    # Call Haskell via USI
    proc = subprocess.Popen(
        ['./shogi-usi-morse.exe'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1
    )
    def send(c):
        proc.stdin.write(c + "\n")
        proc.stdin.flush()
    def read_until(token):
        while True:
            l = proc.stdout.readline()
            if not l: break
            if token in l: break

    send("usi")
    read_until("usiok")
    send("isready")
    read_until("readyok")
    pos_cmd = "position startpos"
    if moves:
        pos_cmd += " moves " + " ".join(moves)
    send(pos_cmd)
    send("go byoyomi 50")
    best = None
    while True:
        l = proc.stdout.readline()
        if not l: break
        if l.startswith("bestmove"):
            best = l.split()[1]
            break
    send("quit")
    proc.terminate()
    return best

print("Testing Haskell engine response...")
b = get_legal_moves(["7g7f", "3c3d", "5g5f"])
print(f"Haskell returned bestmove: {b}")
