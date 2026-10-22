import subprocess

p = subprocess.Popen(
    ["c:\\VS\\Workspace\\t-shogi\\shogi-usi-morse.exe"],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True,
    encoding="utf-8"
)

def send_cmd(cmd):
    p.stdin.write(cmd + "\n")
    p.stdin.flush()

def read_until(target):
    lines = []
    while True:
        line = p.stdout.readline()
        if not line:
            break
        lines.append(line.strip())
        if target in line:
            break
    return lines

send_cmd("usi")
read_until("usiok")
send_cmd("isready")
read_until("readyok")

send_cmd("position startpos moves 7g7f 3c3d 8h2b+ 3a2b 5g5f 8c8d 2h5h 8d8e 7i7h 8e8f 8g8f 8b8f")
send_cmd("go depth 1")
lines = read_until("bestmove")
send_cmd("quit")
p.wait()

for l in lines:
    print(l)
