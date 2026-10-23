from analyze_ply12 import analyze

print("=== Analyzing Game 1: 8th ply 8c8d ===")
pos1 = "7g7f 3c3d 8h2b+ 3a2b 7i8h 7c7d 3g3f 8c8d"
res1 = analyze(pos1, multipv=5, byoyomi_ms=4000)
for r in res1:
    print(r)

print("\n=== Analyzing Game 2: 15th ply 5i5h ===")
pos2 = "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 5i5h"
res2 = analyze(pos2, multipv=5, byoyomi_ms=4000)
for r in res2:
    print(r)
