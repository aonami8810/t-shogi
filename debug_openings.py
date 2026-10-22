from shogi_sim import ShogiBoard

# Interactive opening builder helper
def verify_and_debug(name, mvs):
    b = ShogiBoard()
    for i, m in enumerate(mvs):
        expected_plr = 'Sente' if i % 2 == 0 else 'Gote'
        try:
            b.apply_move(m)
        except Exception as e:
            print(f"FAILED [{name}] ply {i+1} ({'Sente' if i%2==0 else 'Gote'}) {m}: {e}")
            return False, i+1
    return True, len(mvs)

# Let's fix each one:

# 1. Aigakari Standard
# 1(S) 2g2f, 2(G) 8c8d, 3(S) 2f2e, 4(G) 8d8e, 5(S) 7i7h, 6(G) 3a3b,
# 7(S) 2e2d, 8(G) 2c2d, 9(S) 2h2d, 10(G) P*2c, 11(S) 2d2h, 12(G) 8e8f,
# 13(S) 8g8f, 14(G) 8b8f, 15(S) P*8g, 16(G) 8f8b, 17(S) 3i3h, 18(G) 7a7b,
# 19(S) 5i6h, 20(G) 5a4b, 21(S) 4i5h, 22(G) 3c3d, 23(S) 7g7f, 24(G) 6c6d,
# 25(S) 6h7h (Sente King 6h to 7h!), 26(G) 7b6c (Gote Silver 7b to 6c!),
# 27(S) 3g3f, 28(G) 4b3b, 29(S) 3h3g, 30(G) 8b8d,
# 31(S) 2h2f, 32(G) 8d7d, 33(S) 6g6f, 34(G) 7d8d
aigakari_std = [
    "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b",
    "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "8e8f",
    "8g8f", "8b8f", "P*8g", "8f8b", "3i3h", "7a7b",
    "5i6h", "5a4b", "4i5h", "3c3d", "7g7f", "6c6d",
    "6h7h", "7b6c", "3g3f", "4b3b", "3h3g", "8b8d",
    "2h2f", "8d7d", "6g6f", "7d8d"
]
verify_and_debug("Aigakari Standard", aigakari_std)

# 2. Aigakari Hikibisha
# 1(S) 2g2f, 2(G) 8c8d, 3(S) 2f2e, 4(G) 8d8e, 5(S) 7i7h, 6(G) 3a3b,
# 7(S) 2e2d, 8(G) 2c2d, 9(S) 2h2d, 10(G) P*2c, 11(S) 2d2h, 12(G) 8e8f,
# 13(S) 8g8f, 14(G) 8b8f, 15(S) P*8g, 16(G) 8f8b, 17(S) 3i3h, 18(G) 7a7b,
# 19(S) 5i6h, 20(G) 5a4b, 21(S) 4i5h, 22(G) 6c6d, 23(S) 6h7h, 24(G) 7b6c,
# 25(S) 6g6f, 26(G) 3c3d, 27(S) 7g7f, 28(G) 1c1d, 29(S) 1g1f, 30(G) 6c5d
aigakari_hiki = [
    "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b",
    "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "8e8f",
    "8g8f", "8b8f", "P*8g", "8f8b", "3i3h", "7a7b",
    "5i6h", "5a4b", "4i5h", "6c6d", "6h7h", "7b6c",
    "6g6f", "3c3d", "7g7f", "1c1d", "1g1f", "6c5d"
]
verify_and_debug("Aigakari Hikibisha", aigakari_hiki)

# 3. Kakugawari Hayaguri
# 1(S) 7g7f, 2(G) 8c8d, 3(S) 2g2f, 4(G) 8d8e, 5(S) 2f2e, 6(G) 3c3d,
# 7(S) 7i7h, 8(G) 2b3c, 9(S) 8h2b+, 10(G) 3c2b, 11(S) 3i4h, 12(G) 7a6b,
# 13(S) 3g3f, 14(G) 6b7b, 15(S) 4h3g, 16(G) 5a4b, 17(S) 6i7h, 18(G) 6c6d,
# 19(S) 5i6h, 20(G) 7b7c, 21(S) 3g4f, 22(G) 7c7d, 23(S) 4f3e, 24(G) 6d6e,
# 25(S) 2e2d, 26(G) 2c2d, 27(S) 2h2d, 28(G) P*2c, 29(S) 2d2h, 30(G) 7d7e
kakugawari_haya = [
    "7g7f", "8c8d", "2g2f", "8d8e", "2f2e", "3c3d",
    "7i7h", "2b3c", "8h2b+", "3c2b", "3i4h", "7a6b",
    "3g3f", "6b7b", "4h3g", "5a4b", "6i7h", "6c6d",
    "5i6h", "7b7c", "3g4f", "7c7d", "4f3e", "6d6e",
    "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "7d7e"
]
verify_and_debug("Kakugawari Hayaguri", kakugawari_haya)

# 4. Kakugawari Koshikake
# 1(S) 7g7f, 2(G) 8c8d, 3(S) 2g2f, 4(G) 8d8e, 5(S) 2f2e, 6(G) 3c3d,
# 7(S) 7i7h, 8(G) 2b3c, 9(S) 8h2b+, 10(G) 3c2b, 11(S) 3i3h, 12(G) 7a6b,
# 13(S) 3h4h, 14(G) 6b7b, 15(S) 4g4f, 16(G) 6c6d, 17(S) 4h4g, 18(G) 7b6c,
# 19(S) 4g5f, 20(G) 5a4b, 21(S) 6i7h, 22(G) 4a3b, 23(S) 5i6h, 24(G) 4b3a
kakugawari_koshi = [
    "7g7f", "8c8d", "2g2f", "8d8e", "2f2e", "3c3d",
    "7i7h", "2b3c", "8h2b+", "3c2b", "3i3h", "7a6b",
    "3h4h", "6b7b", "4g4f", "6c6d", "4h4g", "7b6c",
    "4g5f", "5a4b", "6i7h", "4a3b", "5i6h", "4b3a"
]
verify_and_debug("Kakugawari Koshikake", kakugawari_koshi)
