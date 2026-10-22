import subprocess

def run_pos(moves_str):
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
    send(f"position startpos moves {moves_str}")
    send("go depth 2")
    lines = read("bestmove")
    send("quit")
    p.wait()
    bm = None
    for l in lines:
        if "bestmove" in l:
            bm = l.split()[1]
    return bm

# 角交換中飛車の進行
moves = "7g7f 3c3d 8h2b+ 3a2b 5g5f 8c8d 2h5h 8d8e 7i7h 8e8f 8g8f 8b8f P*8g 8f8b"
print("Base position reached (Defended 8g):", moves)

# 手番：先手 -> 4八玉
m1 = run_pos(moves)
print("Ply 15 (Sente):", m1)
moves += f" {m1} 5a4b"

# 手番：先手 -> 3八玉 or 4八銀 or 6八銀
m2 = run_pos(moves)
print("Ply 17 (Sente):", m2)
moves += f" {m2} 4a3b"

# 手番：先手 -> 2八玉 or 3八玉
m3 = run_pos(moves)
print("Ply 19 (Sente):", m3)
