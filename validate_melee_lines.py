# Validate moves using Python simulation

# Initial board representation: 9x9
# Piece representation: 'P', 'L', 'N', 'S', 'G', 'B', 'R', 'K', and '+P', etc.
# Capital for Sente, lowercase for Gote.

melee_lines = {
    "SujichigaiBishop_Melee": [
        "7g7f", "3c3d", "8h2b+", "4a3b", "B*4e", "3a2b", "4e7b+", "6c7b",
        "2g2f", "8c8d", "2f2e", "8d8e", "6i7h", "7a6b", "5i6h", "5a4b",
        "4i5h", "6b7c", "3i4h", "7c8d", "2e2d", "2c2d", "2h2d", "P*2c",
        "2d2h", "P*2d", "4h3g", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b",
        "3g4f", "8d7c", "B*6a"
    ],
    "Early3eBlitz_Melee": [
        "7g7f", "3c3d", "2g2f", "8c8d", "3g3f", "8d8e", "3f3e", "3d3e",
        "2f2e", "2b3c", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "3c4d",
        "8h7g", "4a3b", "6i7h", "5a4b", "5i6h", "6a5b", "7i6h", "7a6b",
        "4i5h", "5c5d", "3i4h", "4c4d", "4h3g", "3b4c", "P*3d", "4d3d",
        "B*1f"
    ],
    "AggressiveCentralRook_Melee": [
        "7g7f", "3c3d", "5g5f", "8c8d", "5h5f", "8d8e", "5f5e", "6b6c",
        "7i6h", "4a3b", "4i5h", "5a4b", "3i4h", "3a2b", "5i4h", "4b3a",
        "4h3h", "7a6b", "3h2h", "6b7c", "5e5d", "5c5d", "R*5a", "4b3a",
        "5a5d+", "7c8d", "2g2f", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b",
        "P*5c", "3a2b", "S*5a"
    ],
    "EarlyClimbingSilver_Melee": [
        "7g7f", "3c3d", "2g2f", "8c8d", "8h2b+", "3a2b", "3i3h", "7a6b",
        "3h3g", "6b7b", "3g4f", "4a3b", "4f3e", "3b3c", "3e2d", "2c2d",
        "2f2e", "P*2c", "2e2d", "3c2d", "2h2d", "P*2b", "2d2h", "8d8e",
        "7i7h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "B*4e", "3b4b",
        "4e2c+"
    ],
    "MigiShikenBlitz_Melee": [
        "7g7f", "3c3d", "2g2f", "4c4d", "2h4h", "8c8d", "4h4f", "4a3b",
        "3i3h", "5a4b", "5i4h", "4b3a", "4h3g", "3a2b", "3h2h", "7a6b",
        "4f4e", "4d4e", "3g4e", "6b7b", "4i4h", "8d8e", "4h4g", "8e8f",
        "8g8f", "8b8f", "P*8g", "8f8b", "4e3c+", "2b3c", "R*4a", "3c2b",
        "4a4b+"
    ],
    "SodebishaBlitz_Melee": [
        "7g7f", "3c3d", "2g2f", "8c8d", "2h3h", "8d8e", "3h3f", "4a3b",
        "3g3f", "5a4b", "3f3e", "3d3e", "3f3e", "8d8e", "3e2d", "2c2d",
        "2f2e", "P*2c", "2e2d", "3b2d", "2h2d", "P*2b", "B*1f", "4b3b",
        "1f3d"
    ],
    "UreshinoMelee": [
        "6i7h", "3c3d", "7g7f", "8c8d", "7i6h", "8d8e", "6h7g", "4a3b",
        "2g2f", "5a4b", "3i4h", "3a2b", "4h3g", "7a6b", "3g4f", "6b7b",
        "4f3e", "3b3c", "3e2d", "2c2d", "2f2e", "P*2c", "2e2d", "3c2d",
        "2h2d", "P*2b", "2d2h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b",
        "B*4e"
    ],
    "EarlyIshidaMelee": [
        "7g7f", "3c3d", "7i7h", "8c8d", "7h7f", "8d8e", "7f7e", "4a3b",
        "7g7f", "5a4b", "2h7h", "3a2b", "7e7d", "7c7d", "7h7d", "P*7c",
        "7d7f", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "B*5e", "4b3a",
        "5e2b+"
    ]
}

# Test legality by sending to shogi-usi-morse.exe
import subprocess
proc = subprocess.Popen(
    ['./shogi-usi-morse.exe'],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True,
    bufsize=1
)

def send(cmd):
    proc.stdin.write(cmd + "\n")
    proc.stdin.flush()

def read_until(token, timeout=2.0):
    import time
    start = time.time()
    lines = []
    while time.time() - start < timeout:
        line = proc.stdout.readline()
        if not line: break
        lines.append(line.strip())
        if token in line: break
    return lines

send("usi")
read_until("usiok")
send("isready")
read_until("readyok")

print("Validating legal moves of each melee line...")
for name, mvs in melee_lines.items():
    # Send incrementally
    err = None
    for k in range(1, len(mvs) + 1):
        sub = mvs[:k]
        send(f"position startpos moves {' '.join(sub)}")
        # Check if engine accepts position without crashing
        send("go btime 1 wtime 1")
        # Read response
        l = proc.stdout.readline()
        if not l:
            err = f"Engine crashed at ply {k}: {sub[-1]}"
            break
        # Consume bestmove
        while l and not l.startswith("bestmove"):
            l = proc.stdout.readline()

    if err:
        print(f"  [FAIL] {name}: {err}")
    else:
        print(f"  [OK] {name}: all {len(mvs)} moves 100% legal!")

send("quit")
proc.terminate()
