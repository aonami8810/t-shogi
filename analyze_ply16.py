from analyze_ply12 import analyze

# 15th ply: 2d3d (Sente took Yokofu on 3d)
pos_15 = "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d"
print("=== Analyzing 15th ply (Gote 16th move after Yokofu 2d3d) ===")
res15 = analyze(pos_15, multipv=5, byoyomi_ms=3500)
for r in res15:
    print(r)
