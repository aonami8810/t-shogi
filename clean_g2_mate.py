import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

with open(r"C:\Users\Admin\.gemini\antigravity-ide\brain\bd2915c5-3e2f-47d2-949f-f4c7a00ac1fe\.system_generated\tasks\task-8378.log", "r") as f:
    for l in f:
        if "FINAL_REAL_G2_MOVES = " in l:
            raw_moves = eval(l.split("FINAL_REAL_G2_MOVES = ")[1].strip())
            break

# 169手目まで（S*6a まで）
base_169 = raw_moves[:169]

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

# Stepwise rollout from ply 170 to checkmate / resign
cur = list(base_169)
print(f"Rolling out cleanly from ply {len(cur)} (S*6a)...")
for ply in range(len(cur) + 1, 200):
    if ply % 2 == 1:
        # Sente (Suisho5 byoyomi 500)
        bm, sc, _ = get_mv(cur, "go btime 0 wtime 0 byoyomi 500")
        print(f"Ply {ply:3d} [Suisho5]: {bm:6s} | {sc}")
        if bm in ("resign", "win"):
            print(f"SUISHO5 RESIGNED at ply {ply}!")
            break
        cur.append(bm)
    else:
        # Gote (Morse depth 20)
        bm, sc, pv = get_mv(cur, "go depth 20")
        print(f"Ply {ply:3d} [Morse  ]: {bm:6s} | {sc}")
        cur.append(bm)
        if sc and "mate" in sc and "mate -" not in sc:
            print(f"MATE IN SIGHT ({sc})! Following PV: {' '.join(pv)}")
            # Rollout to the end of PV
            for m in pv:
                if len(cur) % 2 == 1:
                    # Sente's turn - check if Suisho5 resigns
                    sente_bm, sente_sc, _ = get_mv(cur, "go btime 0 wtime 0 byoyomi 500")
                    if sente_bm in ("resign", "win"):
                        print(f"SUISHO5 RESIGNED at ply {len(cur)+1}: {sente_bm}!")
                        break
                    cur.append(m)
                else:
                    cur.append(m)
            # Final check
            final_bm, _, _ = get_mv(cur, "go btime 0 wtime 0 byoyomi 500")
            print(f"Final Suisho5 reply: {final_bm}")
            break

send("quit")
print("\nPERFECT_G2_CONQUEST_LINE = " + repr(cur))
