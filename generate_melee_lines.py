# Generate 100% legal melee lines using engine evaluation and legal moves

import subprocess
import time

def generate_legal_melee_lines():
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

    def read_until(token, timeout=5.0):
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

    # 5手目自ら離脱する代表的シード手
    seeds = [
        ("SujichigaiBishop_Melee", ["7g7f", "3c3d", "8h2b+", "3a2b", "B*4e"]),
        ("Early3eBlitz_Melee", ["7g7f", "3c3d", "2g2f", "8c8d", "3g3f"]),
        ("AggressiveCentralRook_Melee", ["7g7f", "3c3d", "5g5f", "8c8d", "2h5h"]),
        ("EarlyClimbingSilver_Melee", ["7g7f", "3c3d", "2g2f", "8c8d", "3i3h"]),
        ("MigiShikenBlitz_Melee", ["7g7f", "3c3d", "2g2f", "4c4d", "2h4h"]),
        ("SodebishaBlitz_Melee", ["7g7f", "3c3d", "2g2f", "8c8d", "2h3h"]),
        ("EarlyIshidaMelee", ["7g7f", "3c3d", "7i7h", "8c8d", "7h7f"]),
        ("UreshinoMelee", ["6i7h", "3c3d", "7g7f", "8c8d", "7i6h"])
    ]

    results = {}

    for name, init_moves in seeds:
        print(f"\n=======================================================")
        print(f" Expanding: {name} (starting from {len(init_moves)} plies)")
        print(f" Initial: {' '.join(init_moves)}")
        print(f"=======================================================")

        moves = list(init_moves)
        for ply in range(len(init_moves) + 1, 36):
            cmd = f"position startpos moves {' '.join(moves)}"
            send(cmd)
            send("go byoyomi 100") # 100ms で合法手の中から最善手を取得
            
            bestmove = None
            t0 = time.time()
            while time.time() - t0 < 1.0:
                l = proc.stdout.readline()
                if not l: break
                l = l.strip()
                if l.startswith("bestmove"):
                    parts = l.split()
                    if len(parts) >= 2:
                        bestmove = parts[1]
                    break
            
            if not bestmove or bestmove in ["resign", "win"]:
                print(f"  Reached terminal at ply {ply-1}: {bestmove}")
                break
            
            moves.append(bestmove)
            print(f"  Ply {ply:2d}: {bestmove}")

        results[name] = moves
        print(f"--> {name} complete: {len(moves)} plies")

    send("quit")
    proc.terminate()

    # Output as Haskell format
    print("\n\n================ Haskell Output ================")
    for name, mvs in results.items():
        mv_strs = ", ".join([f'"{m}"' for m in mvs])
        print(f'  , ( "{name}"')
        print(f'    , [ {mv_strs} ]')
        print('    )')

    # Save to file
    with open("melee_lines_generated.py", "w", encoding="utf-8") as f:
        f.write(f"melee_lines = {repr(results)}\n")

if __name__ == "__main__":
    generate_legal_melee_lines()
