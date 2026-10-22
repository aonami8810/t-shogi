from shogi_sim import ShogiBoard

openings = {}

# 1. Saddle_Aigakari_Standard [VALID]
openings["Saddle_Aigakari_Standard"] = [
    "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b",
    "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "8e8f",
    "8g8f", "8b8f", "P*8g", "8f8b", "3i3h", "7a7b",
    "5i6h", "5a4b", "4i5h", "3c3d", "7g7f", "6c6d",
    "3g3f", "7b6c", "3h3g", "8b8d", "2h2f", "8d7d",
    "6g6f", "7d8d", "6h7i", "5c5d"
]

# 2. Saddle_Aigakari_Hikibisha
# Replace 3h3g with 3g3f and 3h3g or 1g1f and 4b3b
openings["Saddle_Aigakari_Hikibisha"] = [
    "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b",
    "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "8e8f",
    "8g8f", "8b8f", "P*8g", "8f8b", "3i3h", "7a7b",
    "5i6h", "5a4b", "4i5h", "6c6d", "6g6f", "7b6c",
    "7g7f", "3c3d", "1g1f", "1c1d", "3g3f", "6c5d"
]

# 3. Saddle_Kakugawari_Hayaguri [VALID]
openings["Saddle_Kakugawari_Hayaguri"] = [
    "7g7f", "8c8d", "2g2f", "8d8e", "2f2e", "3c3d",
    "6i7h", "2b3c", "8h2b+", "3c2b", "3i4h", "7a6b",
    "3g3f", "6b7b", "4h3g", "5a4b", "5i6h", "6c6d",
    "3g4f", "7c7d", "4f3e", "6d6e", "2e2d", "2c2d",
    "2h2d", "P*2c", "2d2h", "7b7c"
]

# 4. Saddle_Kakugawari_Koshikake
# 3a has Silver! 4a has Gold! 5a has King!
# Gote: 4a3b (Gold 3b), 3a4b (Silver 4b), then 5a4a or 5a4b?
# In Kakugawari, 2b has Silver (taken back from 3c2b!). 3a is empty!
# So 4b (King) can move to 3a! Wait, why did 4b3a fail?
# Let's check: 10 was 3c2b, so 3a never moved! 3a STILL has Silver!
# Ah! 3a had Silver from initial position! 2b3c was Silver 2b? No, 2b was Bishop!
# 2b3c was BISHOP moving to 3c!
# So 3a STILL HAS SILVER!
# That's why 4b3a failed! 3a has Silver!
# So Gote Silver moves 3a4b or 3a3b!
# Let's make 22. 3a3b (Silver to 3b) and 24. 4a4b (Gold to 4b)!
openings["Saddle_Kakugawari_Koshikake"] = [
    "7g7f", "8c8d", "2g2f", "8d8e", "2f2e", "3c3d",
    "6i7h", "2b3c", "8h2b+", "3c2b", "3i3h", "7a6b",
    "3h4h", "6b7b", "4g4f", "6c6d", "4h4g", "7b6c",
    "4g5f", "5a4b", "5i6h", "3a3b", "3g3f", "4a4b"
]

# 5. Saddle_Yagura_Fortress [VALID]
openings["Saddle_Yagura_Fortress"] = [
    "7g7f", "8c8d", "6g6f", "3c3d", "7i6h", "6c6d",
    "6h7g", "7a6b", "5g5f", "5a4b", "4i5h", "3a3b",
    "6i7h", "4b3c", "5i6h", "5c5d", "2g2f", "6b5c",
    "2f2e", "8d8e", "2e2d", "2c2d", "2h2d", "P*2c",
    "2d2h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b"
]

# 6. Saddle_Yagura_Kato
# 14. 6c6d (Pawn 6c to 6d), then 16. 6b6c (Silver 6b to 6c)!
openings["Saddle_Yagura_Kato"] = [
    "7g7f", "8c8d", "7i6h", "3c3d", "6h7g", "7a6b",
    "2g2f", "8d8e", "5g5f", "4a3b", "5i6h", "5a4b",
    "4i5h", "6c6d", "6g6f", "6b6c", "7c7d", "2f2e",
    "3a2b", "6c5d", "3i4h", "4b3a"
]

# 7. Saddle_Shikenbisha_Mino
# Sente plays Shikenbisha. 
# Gote: 7a6b, 7c7d, 4a3b, 5a4b, 3a4b?
# In Gote: 3a is Silver, 4a is Gold, 5a is King, 2b is Bishop.
# Gote moves 16: 6b7c (Silver to 7c).
# Sente moves: 5i4h, 4h3i, 3i2h, 4i3h (Mino complete!)
openings["Saddle_Shikenbisha_Mino"] = [
    "7g7f", "3c3d", "6g6f", "8c8d", "2h6h", "8d8e",
    "8h7g", "7a6b", "3i3h", "7c7d", "5i4h", "4a3b",
    "4h3i", "5a4b", "3i2h", "6b7c", "4i3h", "7c8d",
    "6h7h", "8b7b", "1g1f", "1c1d", "2g2f", "5c5d",
    "2h1h", "4b3a"
]

# 8. Saddle_Shikenbisha_Exchange
openings["Saddle_Shikenbisha_Exchange"] = [
    "7g7f", "3c3d", "2h6h", "8c8d", "8h2b+", "3a2b",
    "7i8h", "7a6b", "8h7g", "8d8e", "6h6f", "6c6d",
    "3i3h", "4a3b", "5i4h", "5a4b", "4h3i", "6b7b",
    "3i2h", "7b8b", "4i3h", "4b3a", "1g1f", "1c1d",
    "2h1h", "8b7b"
]

# 9. Saddle_Sankenbisha_Ishida
openings["Saddle_Sankenbisha_Ishida"] = [
    "7g7f", "3c3d", "7i6h", "8c8d", "2h7h", "8d8e",
    "8h7g", "7a6b", "3i3h", "4a3b", "5i4h", "5a4b",
    "4h3i", "6b7c", "3i2h", "7c7d", "1g1f", "1c1d",
    "3g3f", "2b3c", "7h7f", "5c5d", "4i3h", "4b3a"
]

# 10. Saddle_Nakabisha_Gokigen
openings["Saddle_Nakabisha_Gokigen"] = [
    "5g5f", "3c3d", "2h5h", "8c8d", "7g7f", "8d8e",
    "7i6h", "7a6b", "3i3h", "4a3b", "5i4h", "5a4b",
    "4h3i", "6b7c", "3i2h", "7c8d", "4i3h", "7c8d", # wait, 7c8d was already done
    "2g2f", "8d7c", "2f2e", "8e8f", "8g8f", "8b8f",
    "P*8g", "8f8b", "3g3f", "7c8d"
]
# Let's fix Nakabisha moves:
openings["Saddle_Nakabisha_Gokigen"] = [
    "5g5f", "3c3d", "2h5h", "8c8d", "7g7f", "8d8e",
    "7i6h", "7a6b", "3i3h", "4a3b", "5i4h", "5a4b",
    "4h3i", "6b7c", "3i2h", "7c8d", "4i3h", "2b3c",
    "2g2f", "8d7c", "2f2e", "8e8f", "8g8f", "8b8f",
    "P*8g", "8f8b", "3g3f", "7c8d"
]

# 11. Saddle_Nakabisha_LeftKing
openings["Saddle_Nakabisha_LeftKing"] = [
    "5g5f", "3c3d", "2h5h", "5c5d", "7g7f", "8c8d",
    "7i6h", "8d8e", "3i3h", "7a6b", "5i4h", "4a3b",
    "4h3i", "5a4b", "3i2h", "6b7c", "4i3h", "7c8d",
    "2g2f", "7c8d", "6g6f", "4c4d", "1g1f", "1c1d"
]
# Fix 18/20 in LeftKing:
openings["Saddle_Nakabisha_LeftKing"] = [
    "5g5f", "3c3d", "2h5h", "5c5d", "7g7f", "8c8d",
    "7i6h", "8d8e", "3i3h", "7a6b", "5i4h", "4a3b",
    "4h3i", "5a4b", "3i2h", "6b7c", "4i3h", "2b3c",
    "2g2f", "7c8d", "6g6f", "4c4d", "1g1f", "1c1d"
]

# 12. Saddle_Yokofudori_34Rook [VALID]
openings["Saddle_Yokofudori_34Rook"] = [
    "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e",
    "7i7h", "3a3b", "2e2d", "2c2d", "2h2d", "8e8f",
    "8g8f", "8b8f", "2d3d", "8f8d", "3d2d", "P*2c",
    "2d2h", "P*8g", "7h8g", "8d8b", "6g6f", "4a4b",
    "3i3h", "5a5b"
]

# 13. Saddle_Gote_84Pawn_Ibisha [VALID]
openings["Saddle_Gote_84Pawn_Ibisha"] = [
    "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b",
    "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "8e8f",
    "8g8f", "8b8f", "P*8g", "8f8b", "3i3h", "7a7b",
    "5i6h", "5a4b", "4i5h", "6c6d", "6g6f", "7b6c",
    "6h7i", "3c3d", "7g7f", "1c1d", "1g1f", "6c5d"
]

# 14. Saddle_Gote_34Pawn_Yagura [VALID]
openings["Saddle_Gote_34Pawn_Yagura"] = [
    "7g7f", "3c3d", "2g2f", "4c4d", "4i4h", "3a3b",
    "5g5f", "4a4b", "7i6h", "5a5b", "6h7g", "4b4c",
    "5i6h", "7a6b", "6h7h", "6c6d", "2f2e", "2b3c",
    "4g4f", "8b4b", "6g6f", "6b6c", "3i3h", "5c5d"
]

# 15. Saddle_EdgePawn_Solid
openings["Saddle_EdgePawn_Solid"] = [
    "1g1f", "3c3d", "7g7f", "8c8d", "6g6f", "8d8e",
    "7i6h", "7a6b", "2g2f", "4a3b", "6h7g", "7c7d",
    "5g5f", "5a4b", "4i5h", "6b7c", "5i6h", "7c8d",
    "6h7h", "2b3c", "2f2e", "3a2b", "3i4h", "5c5d"
]

# 16. Saddle_DirectGold_KakutoDefense [VALID]
openings["Saddle_DirectGold_KakutoDefense"] = [
    "7g7f", "8c8d", "7i7h", "8d8e", "8h7g", "3c3d",
    "2g2f", "7a6b", "2f2e", "4a3b", "3i4h", "5a4b",
    "5i6h", "4b3c", "3g3f", "7c7d", "4h3g", "6b7c",
    "3g4f", "7c8d", "6h7i", "8d7c", "2e2d", "2c2d",
    "2h2d", "P*2c", "2d2h", "7c8d", "6g6f", "8d7c"
]

all_valid = True
for name, mvs in openings.items():
    b = ShogiBoard()
    ok, ply, m, err = b.validate_moves(mvs)
    if ok:
        print(f"  [100% VALID] {name:32s} : {ply} plies")
    else:
        print(f"  [ERROR] {name:32s} at ply {ply} ({m}): {err}")
        all_valid = False

if all_valid:
    print("\n>>> ALL 16 OPENINGS ARE 100% PHYSICALLY LEGAL & FLAWLESS! <<<")
