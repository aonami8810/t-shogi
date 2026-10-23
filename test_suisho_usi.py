import subprocess
import os

suisho_exe = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
suisho_dir = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

print("Checking Suisho5...")
p = subprocess.Popen(
    [suisho_exe],
    cwd=suisho_dir,
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True,
    encoding="utf-8",
    errors="replace"
)

def send(cmd):
    print(">>", cmd)
    p.stdin.write(cmd + "\n")
    p.stdin.flush()

def read_until(target):
    lines = []
    while True:
        line = p.stdout.readline()
        if not line:
            break
        line = line.strip()
        lines.append(line)
        if target in line:
            break
    return lines

send("usi")
usi_lines = read_until("usiok")
for l in usi_lines:
    if "id name" in l or "option name" in l and ("EvalDir" in l or "Book" in l or "Threads" in l):
        print("  ", l)

send("isready")
ready_lines = read_until("readyok")
print("Ready response:")
for l in ready_lines:
    print("  ", l)

send("quit")
p.wait()
print("Suisho5 exited successfully.")
