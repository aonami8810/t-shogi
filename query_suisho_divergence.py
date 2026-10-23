import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def query_suisho(moves, byoyomi_ms=500):
    proc = subprocess.Popen(
        [SUISHO_EXE],
        cwd=SUISHO_DIR,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
        errors="replace"
    )
    def send(cmd):
        proc.stdin.write(cmd + "\n")
        proc.stdin.flush()
    def read_until(target):
        lines = []
        while True:
            line = proc.stdout.readline()
            if not line: break
            line = line.strip()
            lines.append(line)
            if target in line: break
        return lines

    send("usi")
    read_until("usiok")
    send("isready")
    read_until("readyok")
    send("usinewgame")
    
    if moves:
        send(f"position startpos moves {' '.join(moves)}")
    else:
        send("position startpos")
    send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
    
    bestmove = None
    score = None
    pv = None
    while True:
        line = proc.stdout.readline()
        if not line: break
        line = line.strip()
        if line.startswith("info") and "score" in line and "pv" in line:
            pv = line
            parts = line.split()
            if "cp" in parts:
                idx = parts.index("cp")
                if idx + 1 < len(parts):
                    score = parts[idx + 1]
            elif "mate" in parts:
                idx = parts.index("mate")
                if idx + 1 < len(parts):
                    score = "mate " + parts[idx + 1]
        if line.startswith("bestmove"):
            bestmove = line.split()[1]
            break
            
    proc.stdin.write("quit\n")
    proc.stdin.flush()
    try:
        proc.wait(timeout=2)
    except:
        proc.kill()
        
    return bestmove, score, pv

# Game 1: 15手目 2d3d までの後の水匠5の手
g1_15 = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d".split()
bm1, sc1, pv1 = query_suisho(g1_15)
print(f"[Game 1] After 15 plies, Suisho5 plays: {bm1} (eval: {sc1})")
print(f"  PV: {pv1}")

# Game 2: 2手目 8c8d までの後の水匠5の手
g2_2 = "2g2f 8c8d".split()
bm2, sc2, pv2 = query_suisho(g2_2)
print(f"\n[Game 2] After 2 plies (2g2f 8c8d), Suisho5 plays: {bm2} (eval: {sc2})")
print(f"  PV: {pv2}")
