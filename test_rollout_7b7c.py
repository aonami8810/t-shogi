import subprocess
import ast

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open("g1_pure_suisho_2g2f_win.txt", "r") as f:
    g1_win = ast.literal_eval(f.read().strip())

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi"); send("isready")
while "readyok" not in p.stdout.readline(): pass

def ask(moves):
    send(f"position startpos moves {' '.join(moves)}")
    send("go btime 0 wtime 0 byoyomi 500")
    bm, sc = None, ""
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("info") and "score" in l: sc = l
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    return bm, sc

# 21 plies from g1_win + ply 22 7b7c
moves = g1_win[:21] + ['7b7c']
print("Rollout from ply 23 after 7b7c...")
for ply in range(23, 70):
    bm, sc = ask(moves)
    role = "Sente" if ply % 2 == 1 else "Gote"
    print(f"Ply {ply:2d} [{role}]: {bm:6s} | {sc[:60]}")
    moves.append(bm)
    if bm == "resign":
        print("Resigned!")
        break

send("quit")
