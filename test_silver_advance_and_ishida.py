import subprocess
import time

def query_engine(moves_str, go_cmd="go byoyomi 1000"):
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

    if moves_str:
        p.stdin.write(f"position startpos moves {moves_str}\n")
    else:
        p.stdin.write("position startpos\n")
    p.stdin.flush()

    p.stdin.write(f"{go_cmd}\n")
    p.stdin.flush()

    bestmove = None
    pv_lines = []
    while True:
        line = p.stdout.readline()
        if not line:
            break
        line = line.strip()
        if line.startswith("info ") and "pv " in line:
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

print("================================================================================")
print("TEST 1: 棒銀・銀の前線繰り出し検証 (Climbing Silver Advance)")
print("================================================================================")
# 1. 7g7f (先) 2. 3c3d (後) 3. 2g2f (先) 4. 8c8d (後) 5. 2f2e (先) 6. 8d8e (後) 
# 7. 6i7h (先) 8. 4a32 (後) 9. 3i3h (先) 10. 7a6b (後) 
# ここで11手目、先手が銀を前線へ繰り出すか (3h2g 2七銀、または 2e2d)
moves_climbing = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 4a32 3i3h 7a6b"
bm1, pvs1 = query_engine(moves_climbing, "go byoyomi 1000")
print(f"Position: 3八銀・6二銀のあと (手番: 先手)")
print(f"Bestmove: {bm1}")
for pv in pvs1[-3:]:
    print(f"  {pv}")
# 銀の前進 (3h2g: 2七銀) または飛車先突き (2e2d) が期待される
is_silver_or_pawn = bm1 in ["3h2g", "2e2d", "3h4g"]
print(f"Result: {bm1} -> Silver Advance / Direct Attack: {is_silver_or_pawn}")

print("\n================================================================================")
print("TEST 2: 石田流本組の形成 (7五歩・7八飛・7六浮き飛車)")
print("================================================================================")
# 1. 7g7f (先) 2. 3c3d (後) 3. 7f7e (先: 7五歩!) 4. 8c8d (後)
# ここで5手目、先手が7八飛(2h7h)を指すか
moves_ishida_open = "7g7f 3c3d 7f7e 8c8d"
bm2, pvs2 = query_engine(moves_ishida_open, "go byoyomi 1000")
print(f"Position: 7六歩 3四歩 7五歩 8四歩 (手番: 先手)")
print(f"Bestmove (expected 7八飛 2h7h): {bm2}")
for pv in pvs2[-3:]:
    print(f"  {pv}")

# 5. 2h7h (7八飛) 6. 8d8e (8五歩)
# ここで7手目、先手が7六飛(7h7f 浮き飛車!)を指すか
moves_ishida_float = "7g7f 3c3d 7f7e 8c8d 2h7h 8d8e"
bm3, pvs3 = query_engine(moves_ishida_float, "go byoyomi 1000")
print(f"\nPosition: 7六歩 3四歩 7五歩 8四歩 7八飛 8五歩 (手番: 先手)")
print(f"Bestmove (expected 7六飛 7h7f 浮き飛車!): {bm3}")
for pv in pvs3[-3:]:
    print(f"  {pv}")

print("\n================================================================================")
print("TEST 3: 飛車のうろうろ手損抑止 (Anti-Wandering Rook Check)")
print("================================================================================")
# 飛車先が歩で塞がれている局面で、飛車が2七や3八にうろうろ移動しないか
moves_anti_wander = "7g7f 3c3d 2g2f 8c8d 6i7h 4a32 4i5h 5a4b 5i6i 6a5b"
bm4, pvs4 = query_engine(moves_anti_wander, "go byoyomi 1000")
print(f"Position: 駒組みの静かな序盤 (手番: 先手)")
print(f"Bestmove: {bm4}")
for pv in pvs4[-3:]:
    print(f"  {pv}")
is_wandering = bm4 in ["2h2g", "2h3h", "2h4h", "2h1h"]
print(f"Did Rook wander aimlessly? {is_wandering} (Should be False)")

print("\n================================================================================")
print("TEST 4: 腰掛け銀・中央進出 (Reclining / Central Silver)")
print("================================================================================")
# 4八銀・5七銀と上がった局面から、銀をさらに前線(4六銀 5g4f または 5六銀 5g5f)へ繰り出すか
moves_silver_advance = "7g7f 3c3d 2g2f 8c8d 6i7h 4a32 3i4h 5a4b 4h5g 6a5b"
bm5, pvs5 = query_engine(moves_silver_advance, "go byoyomi 1000")
print(f"Position: 5七銀の局面 (手番: 先手)")
print(f"Bestmove (expected 4六銀 5g4f or 5六銀 5g5f or 2五歩 2f2e): {bm5}")
for pv in pvs5[-3:]:
    print(f"  {pv}")
