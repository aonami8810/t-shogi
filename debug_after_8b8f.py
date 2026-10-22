import subprocess

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
# 後手が 8b8f と取った局面
send("position startpos moves 7g7f 8c8d 2g2f 8d8e 2f2e 8e8f 8g8f 8b8f")
send("go depth 1")
lines = read("bestmove")
send("quit")
p.wait()

for l in lines:
    print(l)
