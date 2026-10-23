import subprocess
import sys

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def analyze_position(moves_str, byoyomi_ms=3000):
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
        while True:
            line = proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            if target in line:
                break

    send("usi")
    read_until("usiok")
    send("setoption name MultiPV value 3")
    send("isready")
    read_until("readyok")
    
    if moves_str:
        send(f"position startpos moves {moves_str}")
    else:
        send("position startpos")
        
    send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
    
    pvs = []
    bestmove = None
    
    while True:
        line = proc.stdout.readline()
        if not line:
            break
        line = line.strip()
        if line.startswith("info") and "multipv" in line and "pv" in line:
            pvs.append(line)
        if line.startswith("bestmove"):
            bestmove = line.split()[1]
            break
            
    send("quit")
    try:
        proc.wait(timeout=2)
    except Exception:
        proc.kill()
        
    return bestmove, pvs[-3:] if len(pvs) >= 3 else pvs

if __name__ == "__main__":
    print("=== Analyzing Game 1 Branch: 8th ply 9c9d ===")
    pos1 = "7g7f 3c3d 8h2b+ 3a2b 7i8h 2b3c 8h7g 9c9d"
    bm1, pvs1 = analyze_position(pos1, byoyomi_ms=3000)
    print(f"Position 1 Bestmove: {bm1}")
    for p in pvs1:
        print("  " + p)
        
    print("\n=== Analyzing Game 2 Branch: 31st ply 8a8e ===")
    pos2 = "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8i7g 3c5e P*2b 2a3c 2b2a+ 3a4b 3d8d 5e1i+ 8d8a+ 3c4e 8a8e"
    bm2, pvs2 = analyze_position(pos2, byoyomi_ms=4000)
    print(f"Position 2 Bestmove: {bm2}")
    for p in pvs2:
        print("  " + p)
