from analyze_ply12 import analyze

print("=== Analyzing Game 1: 4th ply 8d8e ===")
pos1 = "7g7f 8c8d 2g2f 8d8e"
res1 = analyze(pos1, multipv=5, byoyomi_ms=4000)
for r in res1:
    print(r)

print("\n=== Analyzing Game 2: 5th ply 6i7h ===")
pos2 = "2g2f 8c8d 7g7f 3c3d 6i7h"
res2 = analyze(pos2, multipv=5, byoyomi_ms=4000)
for r in res2:
    print(r)
