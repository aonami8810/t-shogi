import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open(r"C:\Users\Admin\.gemini\antigravity-ide\brain\bd2915c5-3e2f-47d2-949f-f4c7a00ac1fe\.system_generated\tasks\task-8129.log", "r") as f:
    lines = f.readlines()
for l in lines:
    if "ALL_G2_FINAL_MOVES = " in l:
        all_moves = eval(l.split("ALL_G2_FINAL_MOVES = ")[1].strip())
        break

p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8", errors="replace")
def send(c): p.stdin.write(c + "\n"); p.stdin.flush()
send("usi")
send("isready")
while "readyok" not in p.stdout.readline(): pass

def get_mv(m, cmd):
    send(f"position startpos moves {' '.join(m)}")
    send(cmd)
    bm, sc, pv = None, None, []
    while True:
        l = p.stdout.readline().strip()
        if l.startswith("info") and "score" in l:
            parts = l.split()
            if "cp" in parts: sc = "cp " + parts[parts.index("cp") + 1]
            elif "mate" in parts: sc = "mate " + parts[parts.index("mate") + 1]
            if "pv" in parts: pv = parts[parts.index("pv") + 1:]
        if l.startswith("bestmove"):
            bm = l.split()[1]
            break
    return bm, sc, pv

cur = list(all_moves)
print(f"Starting from ply {len(cur)}...")
for ply in range(len(cur) + 1, 280):
    if ply % 2 == 1:
        # Suisho5 (byoyomi 500)
        bm, sc, _ = get_mv(cur, "go btime 0 wtime 0 byoyomi 500")
        print(f"Ply {ply:3d} [Suisho5]: {bm} | {sc}")
        if bm in ("resign", "win"):
            print(f"SUISHO5 FINISHED: {bm} at ply {ply}!")
            break
        cur.append(bm)
    else:
        # Morse (Gote depth 20)
        bm, sc, pv = get_mv(cur, "go depth 20")
        print(f"Ply {ply:3d} [Morse  ]: {bm} | {sc}")
        cur.append(bm)
        if sc and "mate" in sc and "mate -" not in sc:
            print(f"MATE DETECTED: {sc}! PV: {' '.join(pv)}")
            for m in pv: cur.append(m)
            print(f"FINAL MATE at ply {len(cur)}!")
            # verify resign
            bm_res, _, _ = get_mv(cur, "go btime 0 wtime 0 byoyomi 500")
            print(f"Suisho5 reply to mate: {bm_res}")
            break

send("quit")
print("COMPLETE_G2_WIN_LINE = " + repr(cur))
