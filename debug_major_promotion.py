import subprocess

def query_usi(moves, depth=3):
    p = subprocess.Popen(
        ["c:\\VS\\Workspace\\t-shogi\\shogi-usi-morse.exe"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8"
    )
    def send(c):
        p.stdin.write(c + "\n")
        p.stdin.flush()
    def read(t):
        lines = []
        while True:
            l = p.stdout.readline()
            if not l: break
            lines.append(l.strip())
            if t in l: break
        return lines

    send("usi")
    read("usiok")
    send("isready")
    read("readyok")
    send(f"position startpos moves {moves}")
    send(f"go depth {depth}")
    lines = read("bestmove")
    send("quit")
    p.wait()
    return lines

print("=== Debug Game 1: after 2b8h+ ===")
moves1 = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 2b8h+"
for l in query_usi(moves1, depth=3):
    print(l)

print("\n=== Debug Game 2: after 2d2c+ ===")
moves2 = "2g2f 5c5d 2f2e 8b5b 2e2d 2c2d 2h2d P*2c 2d2c+"
for l in query_usi(moves2, depth=3):
    print(l)
