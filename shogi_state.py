# -*- coding: utf-8 -*-

INIT_BOARD = {
    (1,1): ('Gote', 'L'), (2,1): ('Gote', 'N'), (3,1): ('Gote', 'S'), (4,1): ('Gote', 'G'),
    (5,1): ('Gote', 'K'), (6,1): ('Gote', 'G'), (7,1): ('Gote', 'S'), (8,1): ('Gote', 'N'), (9,1): ('Gote', 'L'),
    (8,2): ('Gote', 'R'), (2,2): ('Gote', 'B'),
    (1,3): ('Gote', 'P'), (2,3): ('Gote', 'P'), (3,3): ('Gote', 'P'), (4,3): ('Gote', 'P'),
    (5,3): ('Gote', 'P'), (6,3): ('Gote', 'P'), (7,3): ('Gote', 'P'), (8,3): ('Gote', 'P'), (9,3): ('Gote', 'P'),
    (1,7): ('Sente', 'P'), (2,7): ('Sente', 'P'), (3,7): ('Sente', 'P'), (4,7): ('Sente', 'P'),
    (5,7): ('Sente', 'P'), (6,7): ('Sente', 'P'), (7,7): ('Sente', 'P'), (8,7): ('Sente', 'P'), (9,7): ('Sente', 'P'),
    (2,8): ('Sente', 'R'), (8,8): ('Sente', 'B'),
    (1,9): ('Sente', 'L'), (2,9): ('Sente', 'N'), (3,9): ('Sente', 'S'), (4,9): ('Sente', 'G'),
    (5,9): ('Sente', 'K'), (6,9): ('Sente', 'G'), (7,9): ('Sente', 'S'), (8,9): ('Sente', 'N'), (9,9): ('Sente', 'L')
}

def parse_sq(s):
    return (int(s[0]), ord(s[1]) - ord('a') + 1)

class BoardState:
    def __init__(self):
        self.board = dict(INIT_BOARD)
        self.hands = {'Sente': {}, 'Gote': {}}
        self.turn = 'Sente'

    def copy(self):
        bs = BoardState()
        bs.board = dict(self.board)
        bs.hands = {'Sente': dict(self.hands['Sente']), 'Gote': dict(self.hands['Gote'])}
        bs.turn = self.turn
        return bs

    def apply_move(self, m):
        plr = self.turn
        opp = 'Gote' if plr == 'Sente' else 'Sente'
        if '*' in m:
            pt, sq_str = m.split('*')
            to_sq = parse_sq(sq_str)
            self.hands[plr][pt] -= 1
            if self.hands[plr][pt] == 0:
                del self.hands[plr][pt]
            self.board[to_sq] = (plr, pt)
        else:
            from_sq = parse_sq(m[:2])
            to_sq = parse_sq(m[2:4])
            promo = m.endswith('+')
            piece_plr, piece_pt = self.board[from_sq]
            del self.board[from_sq]
            if to_sq in self.board:
                _, cap_pt = self.board[to_sq]
                raw_pt = cap_pt.lstrip('+')
                self.hands[plr][raw_pt] = self.hands[plr].get(raw_pt, 0) + 1
            new_pt = ('+' + piece_pt) if promo else piece_pt
            self.board[to_sq] = (plr, new_pt)
        self.turn = opp

    def get_key(self):
        b_items = tuple(sorted(self.board.items()))
        h_sente = tuple(sorted(self.hands['Sente'].items()))
        h_gote = tuple(sorted(self.hands['Gote'].items()))
        return (self.turn, b_items, h_sente, h_gote)

def moves_to_keys(moves):
    bs = BoardState()
    keys = [bs.get_key()]
    for m in moves:
        bs.apply_move(m)
        keys.append(bs.get_key())
    return keys

if __name__ == "__main__":
    # テスト
    test_moves = ["7g7f", "3c3d", "2g2f"]
    keys = moves_to_keys(test_moves)
    print("Keys count:", len(keys))
    print("Test passed!")
