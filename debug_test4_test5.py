import subprocess

def query_usi(moves, depth=1):
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

print("--- Test 4: depth 1 ---")
for l in query_usi("7g7f 8c8d 2g2f 8d8e 2f2e 8e8f", depth=1):
    print(l)

print("\n--- Test 4: depth 2 ---")
for l in query_usi("7g7f 8c8d 2g2f 8d8e 2f2e 8e8f", depth=2):
    print(l)

print("\n--- After 5i6h (Sente slacks), what does Gote do at depth 1? ---")
for l in query_usi("7g7f 8c8d 2g2f 8d8e 2f2e 8e8f 5i6h", depth=1):
    print(l)

print("\n--- Test 5: depth 1 ---")
for l in query_usi("7g7f 8c8d 2g2f 8d8e 2f2e 8e8f 8g8f 8b8f", depth=1):
    print(l)

print("\n--- Test 5: depth 2 ---")
for l in query_usi("7g7f 8c8d 2g2f 8d8e 2f2e 8e8f 8g8f 8b8f", depth=2):
    print(l)

print("\n--- After 2e2d (Sente slacks), what does Gote do at depth 1? ---")
for l in query_usi("7g7f 8c8d 2g2f 8d8e 2f2e 8e8f 8g8f 8b8f 2e2d", depth=1):
    print(l)
