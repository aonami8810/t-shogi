import subprocess

def test_engine_moves_detailed(moves):
    cmd = ["c:\\VS\\Workspace\\t-shogi\\shogi-usi-morse-v4.exe"]
    p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    p.stdin.write("usi\nisready\n")
    p.stdin.flush()
    while True:
        l = p.stdout.readline()
        if "readyok" in l: break
        
    pos_cmd = "position startpos moves " + " ".join(moves) + "\n"
    p.stdin.write(pos_cmd + "go byoyomi 500\n")
    p.stdin.flush()
    bestmove = None
    output = []
    while True:
        line = p.stdout.readline()
        if not line: break
        output.append(line.strip())
        if line.startswith("bestmove"):
            bestmove = line.split()[1]
            break
    p.stdin.write("quit\n")
    p.kill()
    return bestmove, output

# Test Yokofudori 8d8b peaceful solid line
yoko_peace = [
    "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b",
    "2e2d", "2c2d", "2h2d", "8e8f", "8g8f", "8b8f", "2d3d", "8f8d",
    "3d2d", "P*2c", "2d2h", "P*8g", "7h8g", "8d8b", "3i3h", "4a3b"
]

bm, out = test_engine_moves_detailed(yoko_peace)
print("Yokofudori Peaceful Terminal bestmove:", bm)
for l in out[-5:]:
    print(" ", l)
