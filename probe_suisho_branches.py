import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def ask(moves, byoyomi=1000):
    p = subprocess.Popen(
        [SUISHO_EXE],
        cwd=SUISHO_DIR,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
        errors="replace"
    )
    def send(c):
        p.stdin.write(c + "\n")
        p.stdin.flush()
    def read_until(t):
        while True:
            l = p.stdout.readline()
            if not l or t in l: break

    send("usi")
    read_until("usiok")
    send("isready")
    read_until("readyok")
    if moves:
        send(f"position startpos moves {' '.join(moves)}")
    else:
        send("position startpos")
    send(f"go btime 0 wtime 0 byoyomi {byoyomi}")

    bm = None
    sc = None
    pv = None
    while True:
        l = p.stdout.readline()
        if not l: break
        l = l.strip()
        if "info" in l and "score" in l and "pv" in l:
            pv = l
            parts = l.split()
            if "cp" in parts:
                sc = parts[parts.index("cp")+1]
            elif "mate" in parts:
                sc = "mate " + parts[parts.index("mate")+1]
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    send("quit")
    p.wait()
    return bm, sc, pv

print("=== 1. 先手角換わり: 5手目 7i8h に対する水匠5の応手 ===")
m_sente = ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h"]
bm, sc, pv = ask(m_sente, 1000)
print(f"Suisho 6th move: {bm} (eval: {sc})")
print(f"PV: {pv}")

print("\n=== 2. 後手番: 初手 2g2f に対する水匠5の思考 ===")
bm, sc, pv = ask(["2g2f"], 1000)
print(f"Move 2: {bm} (eval: {sc})")
print(f"PV: {pv}")

print("\n=== 3. 後手番: 2g2f 8c8d に対する水匠5の3手目 ===")
bm, sc, pv = ask(["2g2f", "8c8d"], 1000)
print(f"Move 3: {bm} (eval: {sc})")
print(f"PV: {pv}")

print("\n=== 4. 後手番: 2g2f 8c8d 2f2e に対する水匠5の4手目 ===")
bm, sc, pv = ask(["2g2f", "8c8d", "2f2e"], 1000)
print(f"Move 4: {bm} (eval: {sc})")
print(f"PV: {pv}")
