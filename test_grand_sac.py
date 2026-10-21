from test_scenarios import evaluate_pos

print("==================================================")
print("TEST: 真の大駒成り捨てサクリファイス (True Major Piece Sacrifice)")
print("後手: 1一玉(1a), 2一金(2a), 1二歩(1b)")
print("先手: 2八飛(2h), 3三銀(3c), 5九玉(5i), 持ち駒: 金(G)")
print("手順: 2h2a+ (2一飛成！大駒の成り捨てサクリファイス！)")
print("      -> 1a2a (同玉と取るしかない)")
print("      -> G*2b (2二金打！3三銀の紐があるので同玉と取れない！即詰み！)")
print("==================================================")
sfen_grand_sac = "position sfen 7gk/8p/6S2/9/9/9/9/7R1/4K4 b G 1"
bm, out = evaluate_pos(sfen_grand_sac)
print("Result bestmove:", bm)
for line in out.splitlines():
    if line.startswith('info') or line.startswith('bestmove'):
        print(line)
