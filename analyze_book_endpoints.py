import subprocess

def test_engine_moves(moves):
    cmd = ["c:\\VS\\Workspace\\t-shogi\\shogi-usi-morse-v4.exe"]
    p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    pos_cmd = "position startpos moves " + " ".join(moves) + "\n"
    p.stdin.write("usi\nisready\n" + pos_cmd + "go byoyomi 1000\n")
    p.stdin.flush()
    
    bestmove = None
    output = []
    while True:
        line = p.stdout.readline()
        if not line:
            break
        output.append(line.strip())
        if line.startswith("bestmove"):
            bestmove = line.split()[1]
            break
    p.stdin.write("quit\n")
    p.stdin.flush()
    p.kill()
    return bestmove, output

# Test each line
lines = [
  ("Saddle_Aigakari_Standard", ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3i3h", "7a7b", "5i6h", "5a4b", "4i5h", "4b3c", "6h7h", "7b6c", "3h2g", "8b8d", "2h2f", "8d7d", "2f2e", "7d8d", "2e2f", "8d7d"]),
  ("Saddle_Kakugawari_Hayaguri", ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "8h2b+", "3a2b", "7i8h", "7a6b", "3i4h", "6b7b", "4h3g", "4a3b", "6i7h", "5a4b", "5i6h", "6c6d", "3g4f", "7c7d", "4f3e", "3c3d", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h"]),
  ("Saddle_Kakugawari_Koshikake", ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "8h2b+", "3a2b", "7i8h", "7a6b", "3i3h", "6b7b", "3h4h", "4a3b", "4h4g", "5a4b", "5i6h", "6c6d", "4g5f", "7b6c", "6i7h", "5a4b"]),
  ("Saddle_Yokofudori_34Rook", ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d", "2h2d", "8e8f", "8g8f", "8b8f", "2d3d", "8f8h+", "7i8h", "B*4e", "3d3f", "4e2g+", "3f3d", "+2g1g", "3d3f", "1g1f", "3f3e", "1f2f", "B*3c", "2f2e"]),
]

for name, mvs in lines:
    print(f"Testing {name} ({len(mvs)} moves)...")
    bm, out = test_engine_moves(mvs)
    print(f"  Result bestmove: {bm}")
    # check for error or illegal move output
    errors = [l for l in out if "error" in l.lower() or "illegal" in l.lower()]
    if errors:
        print(f"  ERRORS: {errors}")
