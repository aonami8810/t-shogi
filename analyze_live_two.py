from analyze_ply12 import analyze

print("=== Analyzing Game 1 Branch: 8th ply 5a5b ===")
pos1 = "7g7f 8c8d 2g2f 3c3d 2f2e 8d8e 6i7h 5a5b"
res1 = analyze(pos1, multipv=5, byoyomi_ms=3500)
for r in res1:
    print(r)

print("\n=== Analyzing Game 2 Branch: 5th ply 6i7h ===")
pos2 = "7g7f 3c3d 2g2f 8c8d 6i7h"
res2 = analyze(pos2, multipv=5, byoyomi_ms=3500)
for r in res2:
    print(r)
