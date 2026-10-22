import subprocess

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
print("TEST 1: 金の無駄戻り（Retreat）の完全根絶検証")
print("================================================================================")
# 先手が 4九金から5八金と上がった局面 (7g7f 3c3d 2g2f 8c8d 4i5h 8d8e)
# ここで先手が 5八金を元の4九(5h4i)や6九(5h6i)に戻す手を選ばないか
moves_gold = "7g7f 3c3d 2g2f 8c8d 4i5h 8d8e"
bm1, pvs1 = query_engine(moves_gold, "go byoyomi 1000")
print(f"Position: 5八金と上がった直後 (手番: 先手)")
print(f"Bestmove: {bm1}")
if pvs1:
    print(f"PV: {pvs1[-1]}")
is_gold_retreating = bm1 in ["5h4i", "5h5i", "5h6i"]
print(f"Did Gold retreat back to bottom rank? {is_gold_retreating} (Should be False)")

print("\n================================================================================")
print("TEST 2: 飛車の振り直し（Re-swinging）の完全根絶検証")
print("================================================================================")
# 先手が中飛車 (5八飛 2h5h) を指した局面 (7g7f 3c3d 5g5f 8c8d 2h5h 8d8e)
# ここで先手が飛車を7八(5h7h)や6八(5h6h)や2八(5h2h)に振り直さないか
moves_nakabisha = "7g7f 3c3d 5g5f 8c8d 2h5h 8d8e"
bm2, pvs2 = query_engine(moves_nakabisha, "go byoyomi 1000")
print(f"Position: 中飛車 5八飛の局面 (手番: 先手)")
print(f"Bestmove: {bm2}")
if pvs2:
    print(f"PV: {pvs2[-1]}")
is_rook_reswinging = bm2 in ["5h7h", "5h6h", "5h4h", "5h3h", "5h2h", "5h1h"]
print(f"Did Rook re-swing to another file? {is_rook_reswinging} (Should be False)")

print("\n================================================================================")
print("TEST 3: 駒の前進（歩・銀・桂）の包括的推進検証")
print("================================================================================")
# 1. 居飛車の飛車先歩突き (2g2f のあと 2f2e と前進するか)
moves_ibisha_advance = "7g7f 3c3d 2g2f 8c8d"
bm3_1, pvs3_1 = query_engine(moves_ibisha_advance, "go byoyomi 1000")
print(f"Position: 居飛車 2六歩の局面 (手番: 先手)")
print(f"Bestmove (expected 2五歩 2f2e): {bm3_1}")
if pvs3_1:
    print(f"PV: {pvs3_1[-1]}")

# 2. 5七銀から前線への繰り出し (4六銀 5g4f または 5六銀 5g5f)
moves_silver_fwd = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 4a32 3i4h 5a4b 4h5g 6a5b"
bm3_2, pvs3_2 = query_engine(moves_silver_fwd, "go byoyomi 1000")
print(f"\nPosition: 5七銀の局面 (手番: 先手)")
print(f"Bestmove (expected 4六銀 5g4f or 5六銀 5g5f): {bm3_2}")
if pvs3_2:
    print(f"PV: {pvs3_2[-1]}")

print("\n================================================================================")
print("TEST 4: 攻め筋の一貫性（Strategic Commitment）検証")
print("================================================================================")
# 石田流を選択した先手 (7六歩 3四歩 7五歩 8四歩 7八飛 8五歩)
# 7六飛（浮き飛車）で石田流の攻め筋を一貫して貫くか
moves_ishida_consistency = "7g7f 3c3d 7f7e 8c8d 2h7h 8d8e"
bm4, pvs4 = query_engine(moves_ishida_consistency, "go byoyomi 1000")
print(f"Position: 石田流 7八飛の局面 (手番: 先手)")
print(f"Bestmove (expected 7六飛 7h7f 浮き飛車!): {bm4}")
if pvs4:
    print(f"PV: {pvs4[-1]}")
