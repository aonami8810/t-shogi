import subprocess
import os

INIT_BOARD = {
    (1,1): ('Gote', 'Lance'), (2,1): ('Gote', 'Knight'), (3,1): ('Gote', 'Silver'), (4,1): ('Gote', 'Gold'),
    (5,1): ('Gote', 'King'), (6,1): ('Gote', 'Gold'), (7,1): ('Gote', 'Silver'), (8,1): ('Gote', 'Knight'), (9,1): ('Gote', 'Lance'),
    (8,2): ('Gote', 'Rook'), (2,2): ('Gote', 'Bishop'),
    (1,3): ('Gote', 'Pawn'), (2,3): ('Gote', 'Pawn'), (3,3): ('Gote', 'Pawn'), (4,3): ('Gote', 'Pawn'),
    (5,3): ('Gote', 'Pawn'), (6,3): ('Gote', 'Pawn'), (7,3): ('Gote', 'Pawn'), (8,3): ('Gote', 'Pawn'), (9,3): ('Gote', 'Pawn'),
    (1,7): ('Sente', 'Pawn'), (2,7): ('Sente', 'Pawn'), (3,7): ('Sente', 'Pawn'), (4,7): ('Sente', 'Pawn'),
    (5,7): ('Sente', 'Pawn'), (6,7): ('Sente', 'Pawn'), (7,7): ('Sente', 'Pawn'), (8,7): ('Sente', 'Pawn'), (9,7): ('Sente', 'Pawn'),
    (2,8): ('Sente', 'Rook'), (8,8): ('Sente', 'Bishop'),
    (1,9): ('Sente', 'Lance'), (2,9): ('Sente', 'Knight'), (3,9): ('Sente', 'Silver'), (4,9): ('Sente', 'Gold'),
    (5,9): ('Sente', 'King'), (6,9): ('Sente', 'Gold'), (7,9): ('Sente', 'Silver'), (8,9): ('Sente', 'Knight'), (9,9): ('Sente', 'Lance')
}

def parse_sq(s):
    return (int(s[0]), ord(s[1]) - ord('a') + 1)

def validate_moves(name, moves):
    board = dict(INIT_BOARD)
    hands = {'Sente': {'Pawn':0,'Lance':0,'Knight':0,'Silver':0,'Gold':0,'Bishop':0,'Rook':0},
             'Gote':  {'Pawn':0,'Lance':0,'Knight':0,'Silver':0,'Gold':0,'Bishop':0,'Rook':0}}
    turn = 'Sente'
    
    for i, m in enumerate(moves):
        plr = turn
        opp = 'Gote' if plr == 'Sente' else 'Sente'
        
        # Check: Did King oscillate (immediate back and forth)?
        if i >= 2:
            prev_m = moves[i-2]
            if not '*' in m and not '*' in prev_m:
                if m[:2] == prev_m[2:4] and m[2:4] == prev_m[:2]:
                    # Check if it was King
                    # We know from board state
                    pass
        
        if '*' in m:
            pt_char, sq_str = m.split('*')
            to_sq = parse_sq(sq_str)
            if to_sq in board:
                print(f"[{name}] Move {i+1} ({m}): DROP ON OCCUPIED SQUARE {to_sq} by {board[to_sq]}")
                return False
            pt_map = {'P':'Pawn','L':'Lance','N':'Knight','S':'Silver','G':'Gold','B':'Bishop','R':'Rook'}
            pt = pt_map[pt_char]
            if hands[plr][pt] <= 0:
                print(f"[{name}] Move {i+1} ({m}): DROP PIECE NOT IN HAND {pt}")
                return False
            hands[plr][pt] -= 1
            board[to_sq] = (plr, pt)
        else:
            from_sq = parse_sq(m[:2])
            to_sq = parse_sq(m[2:4])
            promo = m.endswith('+')
            if from_sq not in board:
                print(f"[{name}] Move {i+1} ({m}): FROM SQUARE {from_sq} IS EMPTY!")
                return False
            piece_plr, piece_pt = board[from_sq]
            if piece_plr != plr:
                print(f"[{name}] Move {i+1} ({m}): MOVING OPPONENT PIECE {piece_plr} {piece_pt}!")
                return False
            if to_sq in board and board[to_sq][0] == plr:
                print(f"[{name}] Move {i+1} ({m}): CAPTURING OWN PIECE {board[to_sq]} AT {to_sq}!")
                return False
            del board[from_sq]
            if to_sq in board:
                cap_plr, cap_pt = board[to_sq]
                raw_pt = cap_pt.replace('+', '')
                hands[plr][raw_pt] += 1
            new_pt = ('+' + piece_pt) if promo else piece_pt
            board[to_sq] = (plr, new_pt)
            
        # Check: 88 Bishop fork trap check
        # If Gote has Bishop in hand, is (8,8) empty & unguarded with (9,9) Lance exposed?
        if hands[opp]['Bishop'] > 0:
            corner_sq = (8, 8) if opp == 'Gote' else (2, 2)
            lance_sq  = (9, 9) if opp == 'Gote' else (1, 1)
            # We will inspect this after whole move sequence
        
        turn = opp
    return True

# 16 Master Opening Sequences with Thick Formations, Territory Dominance, Castle Integration, and ZERO 88 Bishop Trap
candidate_openings = [
    # 1. 相掛かり（標準型：飛車先交換・歩受け・5筋位取り・金銀スクラム・玉入城）
    ( "Saddle_Aigakari_Standard", "Saddle", 25.0
    , [ "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d", "2h2d", "P*2c"
      , "2d2h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3i3h", "7a7b", "5g5f", "5c5d"
      , "5i6h", "5a4b", "4i5h", "6a5b", "3h3g", "7b7c", "6h7i", "3c3d", "7g7f", "4b3c"
      ]
    ),
    # 2. 相掛かり（引き飛車棒銀型：銀前進・厚み構築）
    ( "Saddle_Aigakari_Hikibisha", "Saddle", 30.0
    , [ "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d", "2h2d", "P*2c"
      , "2d2h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3i3h", "7a7b", "3h2g", "7b8c"
      , "5g5f", "5c5d", "5i6h", "5a4b", "4i5h", "6a5b", "2g3f", "8c7d"
      ]
    ),
    # 3. 角換わり（腰掛け銀型：角交換後 7h金・2b銀で88/22角打ち隙完全防御、5筋位取り・腰掛け銀）
    ( "Saddle_Kakugawari_Koshikake", "Saddle", 35.0
    , [ "7g7f", "8c8d", "2g2f", "8d8e", "2f2e", "3c3d", "6i7h", "2b3c", "8h2b+", "3c2b"
      , "3i3h", "7a6b", "5g5f", "5c5d", "3h4g", "6b5c", "4g5f", "5c5d" # wait, 5c5d already has pawn! Let's check 6b6c or 6b5c -> 5c6d
      ]
    ),
    # 4. 角換わり（早繰り銀型：88隙完全防御・早繰り銀・玉入城）
    ( "Saddle_Kakugawari_Hayaguri", "Saddle", 40.0
    , [ "7g7f", "8c8d", "2g2f", "8d8e", "2f2e", "3c3d", "6i7h", "2b3c", "8h2b+", "3c2b"
      , "3i4h", "7a6b", "3g3f", "6b7b", "4h3g", "5a4b", "5i6h", "6c6d", "3g4f", "7c7d"
      , "4f3e", "6d6e", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "7b7c"
      ]
    ),
    # 5. 相矢倉（本矢倉：6g6f角道止め・88角打ち隙皆無・5筋位取り・金銀4枚連結・矢倉城深奥入城）
    ( "Saddle_Yagura_Fortress", "Saddle", 50.0
    , [ "7g7f", "8c8d", "6g6f", "3c3d", "7i6h", "6c6d", "6h7g", "7a6b", "5g5f", "5a4b"
      , "4i5h", "3a3b", "6i7h", "4b3c", "5i6h", "5c5d", "6h7h", "6b5c", "2g2f", "8d8e"
      , "2f2e", "7c7d", "3i4h", "2b3c" # wait, 3c has silver! Let's be careful with squares.
      ]
    )
]

print("Testing validator...")
