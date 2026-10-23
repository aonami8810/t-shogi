from analyze_ply12 import analyze

print("=== Analyzing 3rd ply 2g2f 8c8d 7g7f responses ===")
res = analyze("2g2f 8c8d 7g7f", multipv=5, byoyomi_ms=3000)
for r in res:
    print(r)
