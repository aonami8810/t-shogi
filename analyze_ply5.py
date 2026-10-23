from analyze_ply12 import analyze

print("=== Analyzing 4th ply 2g2f 8c8d 7g7f 8d8e ===")
res = analyze("2g2f 8c8d 7g7f 8d8e", multipv=3, byoyomi_ms=3000)
for r in res:
    print(r)
