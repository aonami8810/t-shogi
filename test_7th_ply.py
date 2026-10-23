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
    
    send(f"position startpos moves {' '.join(moves)}")
    send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
    
    bestmove = None
    score = None
    pv = ""
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
    try: proc.wait(timeout=2)
    except: proc.kill()
        
    return bestmove, score, pv

# 2g2f 8c8d 7g7f 3c3d 2f2e 8d8e
moves = ["2g2f", "8c8d", "7g7f", "3c3d", "2f2e", "8d8e"]
bm7, sc7, pv7 = query_suisho(moves)
print(f"After {' '.join(moves)}:")
print(f"  7th ply: Suisho5 plays {bm7} (eval: {sc7})")
print(f"  PV: {pv7}")
