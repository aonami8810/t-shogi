import subprocess

def query_engine(moves_str, go_cmd="go byoyomi 1000"):
    p = subprocess.Popen(['./shogi-usi-morse.exe'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, encoding='utf-8')
    p.stdin.write("usi\nisready\n")
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
print("TEST 1: 居飛車の飛車先受け検証 (相手の8六歩に同歩で応じるか)")
print("================================================================================")
# 1. 7g7f 2. 3c3d 3. 5g5f 4. 8c8d 5. 2h5h 6. 8d8e 7. 6i7h 8. 8e8f
# ここで後手が 8六歩(8e8f) と突いてきた！
# 先手の手番で、8七の歩で 8六の歩を取る「8g8f (8六同歩)」が選ばれるか！
moves_pawn_push = "7g7f 3c3d 5g5f 8c8d 2h5h 8d8e 6i7h 8e8f"
bm1, pvs1 = query_engine(moves_pawn_push, "go byoyomi 1000")
print(f"Position: 後手が8六歩と突いた局面 (手番: 先手)")
print(f"Bestmove (expected 8六同歩 8g8f): {bm1}")
if pvs1:
    print(f"PV: {pvs1[-1]}")
is_take_pawn = bm1 == "8g8f"
print(f"Did Sente take the vanguard pawn with 8g8f? {is_take_pawn} (Should be True)")

print("\n================================================================================")
print("TEST 2: 飛車先直射への「歩打ち受け」検証 (8六同飛に8七歩打つか)")
print("================================================================================")
# 8六同歩 8六同飛 と走られた局面 (手番: 先手)
# 先手の手持ちに歩がある。ここで「P*8g (8七歩打ち)」で飛車先を鉄壁に受けるか！
moves_rook_run = "7g7f 3c3d 5g5f 8c8d 2h5h 8d8e 6i7h 8e8f 8g8f 8b8f"
bm2, pvs2 = query_engine(moves_rook_run, "go byoyomi 1000")
print(f"Position: 後手が8六同飛と走った局面 (手番: 先手)")
print(f"Bestmove (expected 8七歩打 P*8g): {bm2}")
if pvs2:
    print(f"PV: {pvs2[-1]}")
is_drop_pawn = bm2 == "P*8g"
print(f"Did Sente drop defensive pawn P*8g? {is_drop_pawn} (Should be True)")

print("\n================================================================================")
print("TEST 3: 中飛車における居玉脱出・美濃囲い入城検証")
print("================================================================================")
# 中飛車 (5八飛) を振った直後 (7g7f 3c3d 5g5f 8c8d 2h5h 6a5b)
# 先手が 5九の居玉を放置せず、4八玉 (5i4h) または 3八銀 と美濃囲いに向かうか！
moves_nakabisha_king = "7g7f 3c3d 5g5f 8c8d 2h5h 6a5b"
bm3, pvs3 = query_engine(moves_nakabisha_king, "go byoyomi 1000")
print(f"Position: 中飛車 5八飛の直後 (手番: 先手)")
print(f"Bestmove (expected 4八玉 5i4h or 3八銀 3i3h or 7八銀 7i7h): {bm3}")
if pvs3:
    print(f"PV: {pvs3[-1]}")
is_castling = bm3 in ["5i4h", "3i3h", "3i4h", "7i7h"]
print(f"Did King mobilize toward castle / defense? {is_castling} (Should be True)")

print("\n================================================================================")
print("TEST 4: 角交換中飛車における美濃囲い・つの銀形成検証")
print("================================================================================")
# 角交換中飛車の典型手順：
# 1. 7g7f (先) 2. 3c3d (後) 3. 8h2b+ (先: 角交換!) 4. 3a2b (後) 5. 5g5f (先) 6. 8c8d (後) 7. 2h5h (先: 中飛車!) 8. 8d8e (後)
# ここで9手目、先手が居玉のまま放置せず、4八玉 (5i4h) と美濃囲いに向かうか！
moves_kaku_nakabisha = "7g7f 3c3d 8h2b+ 3a2b 5g5f 8c8d 2h5h 8d8e"
bm4, pvs4 = query_engine(moves_kaku_nakabisha, "go byoyomi 1000")
print(f"Position: 角交換中飛車の局面 (手番: 先手)")
print(f"Bestmove (expected 4八玉 5i4h or 6八銀 7i6h or 7八金 6i7h): {bm4}")
if pvs4:
    print(f"PV: {pvs4[-1]}")
is_safe_nakabisha = bm4 in ["5i4h", "7i6h", "7i7h", "6i7h", "3i3h", "3i4h"]
print(f"Did Sente castle king or advance silver? {is_safe_nakabisha} (Should be True)")
