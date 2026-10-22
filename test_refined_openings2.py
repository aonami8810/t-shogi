from shogi_sim import ShogiBoard

# Build authentic Shogi Master Openings step by step

openings = {}

# 1. 相掛かり 標準型 (Aigakari Standard)
# 1. 2g2f 2. 8c8d 3. 2f2e 4. 8d8e 5. 7i7h 6. 3a3b
# 7. 2e2d 8. 2c2d 9. 2h2d 10. P*2c 11. 2d2h
# 12. 8e8f 13. 8g8f 14. 8b8f 15. P*8g 16. 8f8b
# 17. 3i3h 18. 7a7b 19. 5i6h 20. 5a4b 21. 4i5h 22. 3c3d
# 23. 7g7f 24. 6c6d 25. 7b6c 26. 6h7h 27. 4b3b 28. 3g3f
# 29. 3h3g 30. 8b8d 31. 2h2f 32. 8d7d 33. 6g6f 34. 7d8d
openings["Saddle_Aigakari_Standard"] = [
    "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d", "2h2d", "P*2c",
    "2d2h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3i3h", "7a7b", "5i6h", "5a4b",
    "4i5h", "3c3d", "7g7f", "6c6d", "7b6c", "6h7h", "4b3b", "3g3f", "3h3g", "8b8d",
    "2h2f", "8d7d", "6g6f", "7d8d"
]

# 2. 相掛かり 引き飛車 (Aigakari Hikibisha)
openings["Saddle_Aigakari_Hikibisha"] = [
    "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d", "2h2d", "P*2c",
    "2d2h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3i3h", "7a7b", "5i6h", "5a4b",
    "4i5h", "6c6d", "7b6c", "6g6f", "3c3d", "7g7f", "1c1d", "1g1f"
]

# 3. 角換わり 早繰り銀 (Kakugawari Hayaguri)
# 1. 7g7f 2. 8c8d 3. 2g2f 4. 8d8e 5. 2f2e 6. 3c3d 7. 7i7h 8. 2b3c
# 9. 8h2b+ 10. 3c2b 11. 3i4h 12. 7a6b 13. 3g3f 14. 6b7b 15. 4h3g (3七銀!)
# 16. 6i7h 17. 5a4b 18. 5i6h 19. 6c6d 20. 3g4f 21. 7c7d 22. 4f3e (早繰り銀進出!)
# 23. 6d6e 24. 2e2d 25. 2c2d 26. 2h2d 27. P*2c 28. 2d2h 29. 7b7c
openings["Saddle_Kakugawari_Hayaguri"] = [
    "7g7f", "8c8d", "2g2f", "8d8e", "2f2e", "3c3d", "7i7h", "2b3c", "8h2b+", "3c2b",
    "3i4h", "7a6b", "3g3f", "6b7b", "4h3g", "6i7h", "5a4b", "5i6h", "6c6d", "3g4f",
    "7c7d", "4f3e", "6d6e", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "7b7c"
]

# 4. 角換わり 腰掛け銀 (Kakugawari Koshikake)
# 1. 7g7f 2. 8c8d 3. 2g2f 4. 8d8e 5. 2f2e 6. 3c3d 7. 7i7h 8. 2b3c
# 9. 8h2b+ 10. 3c2b 11. 3i3h 12. 7a6b 13. 3h4h 14. 6b7b 15. 4g4f (4六歩!)
# 16. 4h4g (4七銀!) 17. 5a4b 18. 5i6h 19. 6c6d 20. 6b6c? wait, 7b6c needs 6c6d first!
# 19. 6c6d 20. 7b6c (6三銀腰掛け!) 21. 4g5f (5六銀腰掛け!) 22. 6i7h 23. 4a3b 24. 6h7i
openings["Saddle_Kakugawari_Koshikake"] = [
    "7g7f", "8c8d", "2g2f", "8d8e", "2f2e", "3c3d", "7i7h", "2b3c", "8h2b+", "3c2b",
    "3i3h", "7a6b", "3h4h", "6b7b", "4g4f", "4h4g", "5a4b", "5i6h", "6c6d", "7b6c",
    "4g5f", "6i7h", "4a3b", "6h7i"
]

# 5. 相矢倉 本筋 (Yagura Fortress)
# 1. 7g7f 2. 8c8d 3. 6g6f 4. 3c3d 5. 7i6h 6. 6c6d 7. 6h7g 8. 7a6b
# 9. 5g5f 10. 5a4b 11. 4i5h 12. 3a3b 13. 5i6h 14. 4b3c 15. 6h7h 16. 5c5d
# 17. 2g2f 18. 6b5c 19. 2f2e 20. 8d8e 21. 7c7d 22. 8e8f 23. 8g8f 24. 8b8f
# 25. P*8g 26. 8f8b 27. 3i4h 28. 5c6d 29. 4h4g
openings["Saddle_Yagura_Fortress"] = [
    "7g7f", "8c8d", "6g6f", "3c3d", "7i6h", "6c6d", "6h7g", "7a6b", "5g5f", "5a4b",
    "4i5h", "3a3b", "5i6h", "4b3c", "6h7h", "5c5d", "2g2f", "6b5c", "2f2e", "8d8e",
    "7c7d", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3i4h", "5c6d", "4h4g"
]

# 6. 矢倉 加藤流 (Yagura Kato)
openings["Saddle_Yagura_Kato"] = [
    "7g7f", "8c8d", "7i6h", "3c3d", "6h7g", "7a6b", "2g2f", "8d8e", "5g5f", "4a3b",
    "5i6h", "5a4b", "4i5h", "6c6d", "6b5c", "6h7h", "7c7d", "2f2e", "3a2b", "6g6f",
    "5c6d", "3i4h"
]

# 7. 四間飛車 本美濃 (Shikenbisha Mino)
# 1. 7g7f 2. 3c3d 3. 6g6f 4. 8c8d 5. 2h6h 6. 8d8e 7. 8h7g 8. 7a6b
# 9. 4i4h 10. 7c7d 11. 3i3h 12. 4a3b 13. 5i4h 14. 5a4b 15. 4h3h 16. 3a2b
# 17. 3h2h 18. 7b7a? No, 7a6b is already done! 6b7c!
# 18. 6b7c 19. 2h1h 20. 7c8d 21. 6h7h 22. 8b7b 23. 1h2h 24. 1c1d
# 25. 1g1f 26. 5c5d 27. 2g2f 28. 4b3a 29. 2f2e 30. 2b3c 31. 3g3f 32. 8b8a
openings["Saddle_Shikenbisha_Mino"] = [
    "7g7f", "3c3d", "6g6f", "8c8d", "2h6h", "8d8e", "8h7g", "7a6b", "4i4h", "7c7d",
    "3i3h", "4a3b", "5i4h", "5a4b", "4h3h", "3a2b", "3h2h", "6b7c", "2h1h", "7c8d",
    "6h7h", "8b7b", "1h2h", "1c1d", "1g1f", "5c5d", "2g2f", "4b3a", "2f2e", "2b3c",
    "3g3f", "8b8a"
]

# 8. 四間飛車 角交換 (Shikenbisha Exchange)
openings["Saddle_Shikenbisha_Exchange"] = [
    "7g7f", "3c3d", "2h6h", "8c8d", "8h2b+", "3a2b", "7i8h", "7a6b", "8h7g", "8d8e",
    "6h6f", "6c6d", "4i4h", "4a3b", "3i4h", "5a4b", "4h3h", "3a2b", "3h2h", "6b7b",
    "6i5h", "4b3a", "1g1f", "1c1d", "2h1h", "7b8b"
]

# 9. 三間飛車 石田流 (Sankenbisha Ishida)
openings["Saddle_Sankenbisha_Ishida"] = [
    "7g7f", "3c3d", "7i6h", "8c8d", "2h7h", "8d8e", "8h7g", "7a6b", "3i4h", "4a3b",
    "5i4h", "5a4b", "4h3h", "3a2b", "3h2h", "7c7d", "1g1f", "1c1d", "3g3f", "2b3c",
    "7h7f", "5c5d", "4i5h", "6b7c"
]

# 10. ゴキゲン中飛車 (Nakabisha Gokigen)
openings["Saddle_Nakabisha_Gokigen"] = [
    "5g5f", "3c3d", "2h5h", "8c8d", "5h5f", "8d8e", "7g7f", "7a6b", "7i6h", "4a3b",
    "4i5h", "5a4b", "3i4h", "3a2b", "5i4h", "4b3a", "4h3h", "7c7d", "3h2h", "6b7c",
    "2g2f", "7c8d", "2f2e", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3g3f", "8d7c",
    "2h1h", "3a2b"
]

# 11. 中飛車 左玉型 (Nakabisha LeftKing)
openings["Saddle_Nakabisha_LeftKing"] = [
    "5g5f", "3c3d", "2h5h", "5c5d", "5h5f", "8c8d", "7g7f", "8d8e", "7i6h", "7a6b",
    "4i5h", "4a3b", "3i4h", "5a4b", "5i4h", "4b3a", "4h3h", "3a2b", "3h2h", "7c7d",
    "2h1h", "6b7c", "6g6f", "4c4d", "1g1f", "1c1d"
]

# 12. 横歩取り (Yokofudori 34Rook)
openings["Saddle_Yokofudori_34Rook"] = [
    "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d",
    "2h2d", "8e8f", "8g8f", "8b8f", "2d3d", "8f8d", "3d2d", "P*2c", "2d2h", "P*8g",
    "7h8g", "8d8b", "6g6f", "4a4b", "3i3h", "5a4b" # wait, 4a4b then 5a4b? 4a is Gold! 4a3b? 3b has Gold!
]
# Wait, let's fix Yokofudori 24..26:
# 22. 8d8b 23. 6g6f 24. 4a4b is Gold moving from 4a to 4b (which is empty!).
# Then 25. 3i3h 26. 5a5b (King to 5b)!
openings["Saddle_Yokofudori_34Rook"] = [
    "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d",
    "2h2d", "8e8f", "8g8f", "8b8f", "2d3d", "8f8d", "3d2d", "P*2c", "2d2h", "P*8g",
    "7h8g", "8d8b", "6g6f", "4a4b", "3i3h", "5a5b"
]

# 13. 後手番 居飛車 (Gote Ibisha)
openings["Saddle_Gote_84Pawn_Ibisha"] = [
    "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d", "2h2d", "P*2c",
    "2d2h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3i3h", "7a7b", "5i6h", "5a4b",
    "4i5h", "6c6d", "7b6c", "6h7h", "3c3d", "7g7f", "1c1d", "3h2g"
]

# 14. 後手番 矢倉 (Gote Yagura)
openings["Saddle_Gote_34Pawn_Yagura"] = [
    "7g7f", "3c3d", "2g2f", "4c4d", "4i4h", "3a3b", "5g5f", "4a4b", "7i6h", "5a5b",
    "6h7g", "4b4c", "5i6h", "7a6b", "6h7h", "6c6d", "2f2e", "2b3c", "4h4g", "8b4b",
    "6g6f", "6b6c", "3i3h", "5c5d"
]

# 15. 端歩突き型 (EdgePawn Solid)
openings["Saddle_EdgePawn_Solid"] = [
    "1g1f", "3c3d", "7g7f", "8c8d", "6g6f", "8d8e", "7i6h", "7a6b", "2g2f", "4a3b",
    "6h7g", "7c7d", "5g5f", "5a4b", "4i5h", "4b3a", "5i6h", "3a2b", "6h7h", "6b7c",
    "2f2e", "2b3c", "3i4h", "5c5d"
]

# 16. 角頭防衛型 (DirectGold KakutoDefense)
openings["Saddle_DirectGold_KakutoDefense"] = [
    "7g7f", "8c8d", "7i7h", "8d8e", "8h7g", "3c3d", "2g2f", "7a6b", "2f2e", "4a3b",
    "3i4h", "5a4b", "5i6h", "4b3c", "6h7h", "7c7d", "7h8h", "6b7c", "4h3g", "7c8d",
    "3g4f", "8d7c", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "7c8d", "6g6f", "8d7c"
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
