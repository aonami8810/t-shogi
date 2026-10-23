from analyze_ply12 import analyze

print("=== Analyzing 1st ply 2g2f responses ===")
res = analyze("2g2f", multipv=5, byoyomi_ms=3000)
for r in res:
    print(r)
