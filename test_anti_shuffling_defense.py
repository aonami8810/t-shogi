import subprocess

def evaluate_moves(pos_moves, byoyomi=500):
    cmd = ["c:\\VS\\Workspace\\t-shogi\\shogi-usi-morse-v4.exe"]
    p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    pos_str = "position startpos moves " + " ".join(pos_moves)
    p.stdin.write(f"usi\nisready\n{pos_str}\ngo byoyomi {byoyomi}\n")
    p.stdin.flush()
    bestmove = None
    output = []
    while True:
        line = p.stdout.readline()
        if not line: break
        l = line.strip()
        output.append(l)
        if l.startswith("bestmove"):
            bestmove = l.split()[1]
            break
    p.stdin.write("quit\n")
    p.kill()
    return bestmove, output

print("================================================================================")
print("TEST: 飛車と金のうろうろ手損防止 ＆ プロ棋戦「受けの手筋」検証")
print("================================================================================")

# -----------------------------------------------------------------------------
# 1. 飛車の往復手損（2h2f -> 2f2h などのうろうろ）防止
# -----------------------------------------------------------------------------
print("\n[TEST 1-1] 飛車の目的のない往復移動（うろうろ戻り手損）の根絶")
# 先手: 2g2f, 後手: 8c8d, 先手: 2h2f (2六飛に浮いた！), 後手: 8d8e
# 次の先手番で、直前の手で上がった 2f の飛車を理由もなく 2f2h と戻す手はうろうろ手損！
# 前進手（2f2e 飛車先進出）や守備手（7i7h 金上がり等）を選ぶべき！
moves_rook_hover = ["2g2f", "8c8d", "2h2f", "8d8e"]
bm_rook, out_rook = evaluate_moves(moves_rook_hover)
print("  Rook hover response bestmove:", bm_rook)
assert bm_rook != "2f2h", f"Rook should NOT immediately reverse to 2f2h without reason! Got {bm_rook}"
print(f"  => PASS: 飛車の無駄なうろうろ往復（2f2h）を完全防止！ (選択手: {bm_rook})")

# -----------------------------------------------------------------------------
# 2. 金の目的のない往復移動（4i5h -> 5h4i などのうろうろ）防止
# -----------------------------------------------------------------------------
print("\n[TEST 1-2] 金の目的のない往復移動（うろうろ戻り手損）の根絶")
# 先手: 7g7f, 後手: 3c3d, 先手: 4i5h (5八金と上がった！), 後手: 8c8d
# 次の先手番で、上がった金を理由もなく 5h4i と戻す手は明白なうろうろ手損！
# 2g2f や 7i6h や 5i6h などの陣形前進手を選ぶべき！
moves_gold_hover = ["7g7f", "3c3d", "4i5h", "8c8d"]
bm_gold, out_gold = evaluate_moves(moves_gold_hover)
print("  Gold hover response bestmove:", bm_gold)
assert bm_gold != "5h4i", f"Gold should NOT immediately reverse to 5h4i without reason! Got {bm_gold}"
print(f"  => PASS: 金の無駄なうろうろ往復（5h4i）を完全防止！ (選択手: {bm_gold})")

# -----------------------------------------------------------------------------
# 3. 飛車先突破に対する「歩打ち受けの手筋（P*2c, P*8g）」の検証
# -----------------------------------------------------------------------------
print("\n[TEST 2-1] 相手の飛車先突破に対する「受けの手筋（歩打ち受け）」の読み切り")
# 相掛かり序盤：先手 2四飛（2h2d）と走った局面
# 後手の手番：相手の飛車に自陣突破されないよう、プロの手筋「P*2c（2三歩打ち）」で受ける！
moves_pawn_drop = ["2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d", "2h2d"]
bm_pawn_drop, out_pawn_drop = evaluate_moves(moves_pawn_drop)
print("  Response to 2h2d bestmove:", bm_pawn_drop)
assert bm_pawn_drop == "P*2c", f"Expected P*2c (受けの手筋・歩打ち), got {bm_pawn_drop}"
print("  => PASS: 飛車先突破に対して正確な受けの手筋（P*2c）を選択！")

# -----------------------------------------------------------------------------
# 4. 角換わりにおける「同銀の引き受け手筋」の検証
# -----------------------------------------------------------------------------
print("\n[TEST 2-2] 角交換に対する「同銀の受け手筋」の読み切り")
# 角換わり序盤：先手が 8h2b+ と角を成り込んできた局面
# 後手の手番：プロの手筋「3c2b（同銀）」で銀を引いて最も手堅く受ける！
moves_kaku_exchange = ["7g7f", "8c8d", "2g2f", "8d8e", "2f2e", "3c3d", "6i7h", "2b3c", "8h2b+"]
bm_kaku_rec, out_kaku_rec = evaluate_moves(moves_kaku_exchange)
print("  Response to 8h2b+ bestmove:", bm_kaku_rec)
assert bm_kaku_rec == "3c2b", f"Expected 3c2b (同銀・受けの手筋), got {bm_kaku_rec}"
print("  => PASS: 角交換に対して正確な受けの手筋（3c2b 同銀）を選択！")

print("\n================================================================================")
print("ALL ANTI-SHUFFLING & DEFENSIVE TECHNIQUE TESTS PASSED!")
print("================================================================================")
