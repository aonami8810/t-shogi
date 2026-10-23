from analyze_ply12 import analyze

print("=== Analyzing Game 1: 4th ply 4a3b ===")
pos1 = "7g7f 8c8d 2g2f 4a3b"
res1 = analyze(pos1, multipv=5, byoyomi_ms=4000)
for r in res1:
    print(r)

print("\n=== Analyzing Game 2: 25th ply P*2d ===")
pos2 = "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d P*2d"
res2 = analyze(pos2, multipv=5, byoyomi_ms=4000)
for r in res2:
    print(r)
