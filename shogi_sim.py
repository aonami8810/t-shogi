# Precise Shogi Simulator & Validator
# Board representation: 9x9 grid, files 1..9, ranks 1..9 (a..i)

class ShogiBoard:
    def __init__(self):
        self.board = {}
        # Gote
        self.board[(1,1)] = ('Gote', 'L')
        self.board[(2,1)] = ('Gote', 'N')
        self.board[(3,1)] = ('Gote', 'S')
        self.board[(4,1)] = ('Gote', 'G')
        self.board[(5,1)] = ('Gote', 'K')
        self.board[(6,1)] = ('Gote', 'G')
        self.board[(7,1)] = ('Gote', 'S')
        self.board[(8,1)] = ('Gote', 'N')
        self.board[(9,1)] = ('Gote', 'L')
        self.board[(8,2)] = ('Gote', 'R')
        self.board[(2,2)] = ('Gote', 'B')
        for f in range(1, 10):
            self.board[(f, 3)] = ('Gote', 'P')
            
        # Sente
        for f in range(1, 10):
            self.board[(f, 7)] = ('Sente', 'P')
        self.board[(2,8)] = ('Sente', 'R')
        self.board[(8,8)] = ('Sente', 'B')
        self.board[(1,9)] = ('Sente', 'L')
        self.board[(2,9)] = ('Sente', 'N')
        self.board[(3,9)] = ('Sente', 'S')
        self.board[(4,9)] = ('Sente', 'G')
        self.board[(5,9)] = ('Sente', 'K')
        self.board[(6,9)] = ('Sente', 'G')
        self.board[(7,9)] = ('Sente', 'S')
        self.board[(8,9)] = ('Sente', 'N')
        self.board[(9,9)] = ('Sente', 'L')
        
        self.hands = {'Sente': {}, 'Gote': {}}
        self.turn = 'Sente'

    def parse_sq(self, s):
        return (int(s[0]), ord(s[1]) - ord('a') + 1)

    def format_sq(self, sq):
        return f"{sq[0]}{chr(ord('a') + sq[1] - 1)}"

    def apply_move(self, m):
        plr = self.turn
        opp = 'Gote' if plr == 'Sente' else 'Sente'
        if '*' in m:
            pt, to_str = m.split('*')
            to_sq = self.parse_sq(to_str)
            if to_sq in self.board:
                raise ValueError(f"Square {to_sq} already occupied by {self.board[to_sq]}")
            if self.hands[plr].get(pt, 0) <= 0:
                raise ValueError(f"Piece {pt} not in hand of {plr}")
            self.hands[plr][pt] -= 1
            self.board[to_sq] = (plr, pt)
        else:
            from_sq = self.parse_sq(m[:2])
            to_sq = self.parse_sq(m[2:4])
            promo = m.endswith('+')
            if from_sq not in self.board:
                raise ValueError(f"Square {from_sq} is empty!")
            p_plr, pt = self.board[from_sq]
            if p_plr != plr:
                raise ValueError(f"Piece at {from_sq} belongs to {p_plr}, not {plr}")
            if to_sq in self.board and self.board[to_sq][0] == plr:
                raise ValueError(f"Cannot capture own piece at {to_sq}")
            
            del self.board[from_sq]
            if to_sq in self.board:
                c_plr, c_pt = self.board[to_sq]
                raw_pt = c_pt.lstrip('+')
                self.hands[plr][raw_pt] = self.hands[plr].get(raw_pt, 0) + 1
            new_pt = ('+' + pt) if promo else pt
            self.board[to_sq] = (plr, new_pt)
        self.turn = opp

    def validate_moves(self, moves):
        for i, m in enumerate(moves):
            try:
                self.apply_move(m)
            except Exception as e:
                return False, i + 1, m, str(e)
        return True, len(moves), None, None

print("Shogi validator initialized.")
