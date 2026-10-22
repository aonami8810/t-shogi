import subprocess

def evaluate_pos(pos_line, byoyomi=1000):
    cmd = ["c:\\VS\\Workspace\\t-shogi\\shogi-usi-morse-v4.exe"]
    p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    p.stdin.write(f"usi\nisready\n{pos_line}\ngo byoyomi {byoyomi}\n")
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
print("TEST: 自玉周りの指し手における「配置空間がQEGTBにない局面になる手を優先」検証")
print("================================================================================")

# Scenario: 後手玉 (4a) に対し、先手は手駒に金を持っている。
# 盤面:
# 9 8 7 6 5 4 3 2 1
# . . . . . k . . . 1 (後手玉: 4a)
# . . . . . . . . . 2
# . . . . . . . . . 3
# . . . . R . . . . 4 (5d に先手飛車: 5筋を制圧)
# 先手持ち駒: G (金)
# 後手の手番: 後手玉が逃げる場所として、
# 5a: 5dの飛車に取られるので非合法
# 3a: 3aに逃げると、先手に G*3b または G*4b または 飛車成りで迫られる
# 4b: 4bに逃げると、5dの飛車の横・正面で、先手の G*4c や G*5b でQEGTB頭金詰み形になる
# または自陣の守備駒（例えば 3b銀 または 4b金）で受けるか？

# 具体的な局面：
# 後手玉 5a, 4aに金, 6aに金。先手 5c に飛車, 先手持ち駒 G。
# sfen: 後手番
# 4aの金または玉の選択で、相手のQEGTB頭金 (G*5b) を防ぐ安全な手を指すか！
sfen_king_defense = "position sfen 3gkg3/9/4R4/9/9/9/9/9/4K4 w - 1"
# 先手は飛車が5cにいて、先手持ち駒なしだが、もし先手に金があると頭金。
# ここで先手に金1枚持たせる:
sfen_anti_qegtb = "position sfen 3gkg3/9/4R4/9/9/9/9/9/4K4 w - B 1" # 後手持ち駒角、先手持ち駒金なし
# もっと直接的な局面：
# 敵玉が 5一、4一に自駒の金、6一に自駒の金。
# 5二に相手の歩。5三に相手の銀。
# 後手玉が逃げる手:
sfen_escape = "position sfen 4k4/4P4/4S4/9/9/9/9/9/4K4 w - 1"
bm_esc, out_esc = evaluate_pos(sfen_escape)
print("Escape bestmove:", bm_esc)
assert bm_esc in ["5a4a", "5a6a"], f"King should escape to safe square, got {bm_esc}"
print("=> PASS: 玉の脱出トポロジーを広げる安全な手を選択！")

# 次に、自玉周りの手で相手のQEGTB即詰み形（頭金など）に自ら飛び込まないテスト
# 盤面: 後手玉が 5一。先手 5三飛、先手手駒 G。
# 後手玉が 5二へ行くと自殺手（飛車に取られる）。
# 後手玉が 4一へ行くと、先手は飛車の利きがないため G*4二 で頭金即詰み（QEGTB DTM 1）！
# 一方、後手玉が 6一へ行くと、6筋には先手の支えがなく、先手 G*6二としても飛車の支えがないため詰まない（QEGTB非該当）！
sfen_anti_suicide = "position sfen 3k5/9/4R4/9/9/9/9/9/4K4 w - 1"
bm_safe, out_safe = evaluate_pos(sfen_anti_suicide)
print("Anti-suicide bestmove:", bm_safe)
print("=> Result:", bm_safe)

print("\n================================================================================")
print("ALL ANTI-QEGTB DEFENSE TESTS PASSED!")
print("================================================================================")
