from analyze_ply12 import analyze

# Check earlier plies of Game 2 (where Morse is Gote):
# 20th ply: after 8f7f 8h7g
# In Game 2, Morse played 7f7d?
pos_20 = "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g"
print("=== Analyzing 20th ply (Gote to move after 8h7g) ===")
res20 = analyze(pos_20, multipv=5, byoyomi_ms=2500)
for r in res20:
    print(r)

# 22nd ply: after 7f7d 3d7d
pos_22 = pos_20 + " 7f7d 3d7d"
print("\n=== Analyzing 22nd ply (Gote to move after 3d7d) ===")
res22 = analyze(pos_22, multipv=5, byoyomi_ms=2500)
for r in res22:
    print(r)

# 24th ply: after 7c7d P*2d
pos_24 = pos_22 + " 7c7d P*2d"
print("\n=== Analyzing 24th ply (Gote to move after P*2d) ===")
res24 = analyze(pos_24, multipv=5, byoyomi_ms=4000)
for r in res24:
    print(r)
