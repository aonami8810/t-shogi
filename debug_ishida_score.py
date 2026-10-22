import subprocess

def query_position_moves(moves_str):
    p = subprocess.Popen(['./shogi-usi-morse.exe'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, encoding='utf-8')
    p.stdin.write("usi\n")
    p.stdin.flush()
    while True:
        line = p.stdout.readline()
        if not line or "usiok" in line:
            break
    
    p.stdin.write("isready\n")
    p.stdin.flush()
    while True:
        line = p.stdout.readline()
        if not line or "readyok" in line:
            break

    p.stdin.write(f"position startpos moves {moves_str}\n")
    p.stdin.flush()
    # 思考時間を少し長めに取る (movetime 2000)
    p.stdin.write("go movetime 1500\n")
    p.stdin.flush()

    bestmove = None
    pv_lines = []
    while True:
        line = p.stdout.readline()
        if not line:
            break
        line = line.strip()
        if line.startswith("info "):
            pv_lines.append(line)
        if line.startswith("bestmove"):
            parts = line.split()
            if len(parts) >= 2:
                bestmove = parts[1]
            break

    p.stdin.write("quit\n")
    p.stdin.flush()
    p.terminate()
    return bestmove, pv_lines

print("--- Checking 7g7f 3c3d 7f7e 8c8d ---")
bm, pvs = query_position_moves("7g7f 3c3d 7f7e 8c8d")
print("Bestmove:", bm)
for pv in pvs:
    print(pv)
