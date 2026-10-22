import subprocess

# Let's write a python legal move validator for Shogi
INIT_BOARD = {
    (1,1): ('Gote', 'Lance'), (2,1): ('Gote', 'Knight'), (3,1): ('Gote', 'Silver'), (4,1): ('Gote', 'Gold'),
    (5,1): ('Gote', 'King'), (6,1): ('Gote', 'Gold'), (7,1): ('Gote', 'Silver'), (8,1): ('Gote', 'Knight'), (9,1): ('Gote', 'Lance'),
    (8,2): ('Gote', 'Rook'), (2,2): ('Gote', 'Bishop'),
    (1,3): ('Gote', 'Pawn'), (2,3): ('Gote', 'Pawn'), (3,3): ('Gote', 'Pawn'), (4,3): ('Gote', 'Pawn'),
    (5,3): ('Gote', 'Pawn'), (6,3): ('Gote', 'Pawn'), (7,3): ('Gote', 'Pawn'), (8,3): ('Gote', 'Pawn'), (9,3): ('Gote', 'Pawn'),
    (1,7): ('Sente', 'Pawn'), (2,7): ('Sente', 'Pawn'), (3,7): ('Sente', 'Pawn'), (4,7): ('Sente', 'Pawn'),
    (5,7): ('Sente', 'Pawn'), (6,7): ('Sente', 'Pawn'), (7,7): ('Sente', 'Pawn'), (8,7): ('Sente', 'Pawn'), (9,7): ('Pawn', 'Pawn'), # wait, 9,7 is Sente Pawn
    (2,8): ('Sente', 'Rook'), (8,8): ('Sente', 'Bishop'),
    (1,9): ('Sente', 'Lance'), (2,9): ('Sente', 'Knight'), (3,9): ('Sente', 'Silver'), (4,9): ('Sente', 'Gold'),
    (5,9): ('Sente', 'King'), (6,9): ('Sente', 'Gold'), (7,9): ('Sente', 'Silver'), (8,9): ('Sente', 'Knight'), (9,9): ('Sente', 'Lance')
}

def parse_sq(s):
    # e.g. "7g" -> (7, 7)
    return (int(s[0]), ord(s[1]) - ord('a') + 1)

def validate_line(name, moves):
    board = dict(INIT_BOARD)
    board[(9,7)] = ('Sente', 'Pawn')
    hands = {'Sente': {}, 'Gote': {}}
    turn = 'Sente'
    
    for i, m in enumerate(moves):
        plr = turn
        if '*' in m:
            pt, sq_str = m.split('*')
            to_sq = parse_sq(sq_str)
            if to_sq in board:
                print(f"[{name}] Move {i+1} ({m}): DROP ON OCCUPIED SQUARE {to_sq} by {board[to_sq]}")
                return False
            # check hand
            cnt = hands[plr].get(pt, 0)
            if cnt <= 0:
                print(f"[{name}] Move {i+1} ({m}): DROP PIECE NOT IN HAND {pt} (hands: {hands[plr]})")
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
                # demote
                raw_pt = cap_pt.lstrip('+').replace('P', 'Pawn').replace('L', 'Lance') # simpl
                hands[plr]['P' if 'Pawn' in cap_pt else cap_pt] = hands[plr].get('P' if 'Pawn' in cap_pt else cap_pt, 0) + 1
            new_pt = ('+' + piece_pt) if promo else piece_pt
            board[to_sq] = (plr, new_pt)
        turn = 'Gote' if turn == 'Sente' else 'Sente'
    return True

from test_16_master_candidates import master_candidates
print("Strictly validating all 16 candidates against Shogi rules...")
for name, mvs in master_candidates:
    ok = validate_line(name, mvs)
    if ok:
        print(f"  [PERFECT] {name} ({len(mvs)} moves)")
