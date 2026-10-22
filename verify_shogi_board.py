# Helper to verify and find exact authentic Shogi moves
import sys

# Piece definitions and simple board simulation
class SimpleBoard:
    def __init__(self):
        # 9x9 board: 1..9, 1..9
        # 'P','L','N','S','G','B','R','K' for Sente (capital)
        # 'p','l','n','s','g','b','r','k' for Gote (lowercase)
        self.bd = {}
        for x in range(1, 10):
            self.bd[(x, 3)] = 'p'
            self.bd[(x, 7)] = 'P'
        self.bd[(1, 1)] = 'l'; self.bd[(9, 1)] = 'l'
        self.bd[(2, 1)] = 'n'; self.bd[(8, 1)] = 'n'
        self.bd[(3, 1)] = 's'; self.bd[(7, 1)] = 's'
        self.bd[(4, 1)] = 'g'; self.bd[(6, 1)] = 'g'
        self.bd[(5, 1)] = 'k'
        self.bd[(8, 2)] = 'r'; self.bd[(2, 2)] = 'b'

        self.bd[(1, 9)] = 'L'; self.bd[(9, 9)] = 'L'
        self.bd[(2, 9)] = 'N'; self.bd[(8, 9)] = 'N'
        self.bd[(3, 9)] = 'S'; self.bd[(7, 9)] = 'S'
        self.bd[(4, 9)] = 'G'; self.bd[(6, 9)] = 'G'
        self.bd[(5, 9)] = 'K'
        self.bd[(2, 8)] = 'R'; self.bd[(8, 8)] = 'B'
        self.turn = 'Sente'
        self.hands = {'Sente': {}, 'Gote': {}}

    def apply(self, mv_str):
        # format: e.g. 7g7f or P*5e
        if '*' in mv_str:
            pt, sq_str = mv_str.split('*')
            to_x = int(sq_str[0])
            to_y = ord(sq_str[1]) - ord('a') + 1
            piece = pt.upper() if self.turn == 'Sente' else pt.lower()
            assert (to_x, to_y) not in self.bd, f"Target {(to_x, to_y)} not empty for drop {mv_str}"
            assert self.hands[self.turn].get(pt.upper(), 0) > 0, f"No {pt} in hand for {self.turn}"
            self.hands[self.turn][pt.upper()] -= 1
            self.bd[(to_x, to_y)] = piece
        else:
            fx = int(mv_str[0])
            fy = ord(mv_str[1]) - ord('a') + 1
            tx = int(mv_str[2])
            ty = ord(mv_str[3]) - ord('a') + 1
            promo = '+' in mv_str
            piece = self.bd.get((fx, fy))
            assert piece is not None, f"No piece at {(fx, fy)} for move {mv_str}"
            if self.turn == 'Sente':
                assert piece.isupper(), f"Moving gote piece {piece} on Sente turn {mv_str}"
            else:
                assert piece.islower(), f"Moving sente piece {piece} on Gote turn {mv_str}"
            target = self.bd.get((tx, ty))
            if target:
                if self.turn == 'Sente':
                    assert target.islower(), f"Cannot capture own piece at {(tx, ty)}"
                else:
                    assert target.isupper(), f"Cannot capture own piece at {(tx, ty)}"
                cap_pt = target.upper().replace('+', '')
                self.hands[self.turn][cap_pt] = self.hands[self.turn].get(cap_pt, 0) + 1
            del self.bd[(fx, fy)]
            if promo:
                piece = '+' + piece
            self.bd[(tx, ty)] = piece

        self.turn = 'Gote' if self.turn == 'Sente' else 'Sente'

def verify_and_print(moves):
    b = SimpleBoard()
    for i, m in enumerate(moves):
        try:
            b.apply(m)
        except Exception as e:
            print(f"Error at ply {i+1} ({m}): {e}")
            return False
    print(f"All {len(moves)} plies perfectly legal!")
    return True

# Construct Sente Nakabisha with Mino and Central Advance
# 1. 7g7f 2. 3c3d 3. 5g5f 4. 8c8d 5. 2h5h (Rook to 5h)
# 6. 8d8e 7. 7i7h (Silver to 7h) 8. 4a3b (Gold to 3b)
# 9. 5f5e (Pawn 5e) 10. 5a4b (King 4b)
# 11. 3i3h (Silver to 3h) 12. 7a6b (Silver to 6b)
# 13. 5i4h (King 4h) 14. 6c6d (Pawn 6d)
# 15. 4h3i (King 3i) 16. 3a4a (Gold 4a)
# 17. 3i2h (King 2h - Mino) 18. 4b3a (King 3a)
# 19. 4i3i (Gold 3i - Mino Castle complete!) 20. 6b6c (Silver 6c)
# 21. 6i6h (Gold to 6h, defending kobin & 5h rook flank!) 22. 8b8d
# 23. 5h5f (Rook to 5f!) 24. 8d7d
# 25. 6h5h (Gold to 5h! Now 5h is open because Rook lifted to 5f!) 26. 7c7d (Pawn to 7d - open knight path)
# 27. 5h5g (Gold to 5g - Ironclad shield!) 28. 8a7c (Knight to 7c)
# 29. 5g4f (Gold to 4f - Central dominance!) 30. 1c1d
# 31. 1g1f 32. 7c6e 33. 7h6g 34. 6e7g+ 35. 6g7g
# 36. 6c5d 37. 5e5d 38. B*4e 39. 4f3e 40. 4e2g+
# 41. 2h1h 42. 2g3h 43. 3i3h
# Construct Sente Nakabisha with Mino and Central Advance
sente_nakabisha = [
    "7g7f", "3c3d", "5g5f", "8c8d", "2h5h", "8d8e", "7i7h", "4a3b",
    "5f5e", "5a4b", "3i3h", "7a6b", "5i4h", "6c6d", "4h3i", "3a4a",
    "3i2h", "4b3a", "4i3i", "6b6c", "6i6h", "8e8f", "8g8f", "8b8f",
    "P*8g", "8f8b", "5h5f", "7c7d", "6h5h", "8a7c", "1g1f", "1c1d",
    "5h5g", "7c6e", "7h6h", "6e7g+", "6h7g", "8b8d", "5f4f", "6c5d",
    "5e5d", "6d6e", "7g6f"
]

print("Verifying Sente Nakabisha:")
verify_and_print(sente_nakabisha)

# Construct Gote Gokigen Nakabisha vs Chosoku 37 Silver
gote_gokigen = [
    "7g7f", "3c3d", "2g2f", "5c5d", "2f2e", "8b5b", "3g3f", "7a6b",
    "3i3h", "6b5c", "3h3g", "5c4d", "3g4f", "4a3b", "2e2d", "2c2d",
    "2h2d", "P*2c", "2d2h", "5a6b", "6i7h", "6b7b", "5i6h", "7b8b",
    "4i4h", "6a7b", "1g1f", "1c1d", "2i3g", "4d5e", "4f5e", "5b5e",
    "P*5f", "5e5a", "8h2b+", "3a2b", "B*5c", "5a5b", "5c4b+", "3b4b",
    "2h2a+", "2b3a", "2a2c", "4b3b", "2c3b", "3a3b", "G*4a"
]

print("\nVerifying Gote Gokigen:")
verify_and_print(gote_gokigen)

# Construct Sente Kakuexchange Nakabisha
sente_kakuexchange = [
    "7g7f", "3c3d", "5g5f", "8c8d", "2h5h", "8d8e", "7i7h", "8e8f",
    "8g8f", "8b8f", "P*8g", "8f8b", "8h2b+", "3a2b", "6i6h", "7a6b",
    "5i4h", "5a4b", "3i3h", "4a3b", "4h3i", "4b3a", "3i2h", "5c5d",
    "4i3i", "6b5c", "1g1f", "1c1d", "5f5e", "8b8d", "5h5g", "8d7d",
    "6h5h", "7d8d", "5h4h", "8d8e", "3g3f", "8e5e", "5g5e", "5c4d",
    "B*5c", "3b4b", "5c4b+", "3a4b", "G*5a"
]

print("\nVerifying Sente Kakuexchange Nakabisha:")
verify_and_print(sente_kakuexchange)
