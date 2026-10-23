from analyze_ply12 import analyze

# Sente played 7g7f. What if Gote responds with 8c8d?
print("=== Analyzing 7g7f 8c8d ===")
res = analyze("7g7f 8c8d", multipv=5, byoyomi_ms=3000)
for r in res:
    print(r)
