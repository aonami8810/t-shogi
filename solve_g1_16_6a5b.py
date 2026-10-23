# -*- coding: utf-8 -*-
from solve_both_nodes5m import solve_line

if __name__ == "__main__":
    g1_moves = "7g7f 8c8d 2g2f 4a3b 7i7h 3c3d 7h7g 3a4b 2f2e 4b3c 6i7h 5c5d 3i4h 7a6b 3g3f 6a5b".split()
    solve_line(g1_moves, True, "G1_16_6a5b", "g1_ply16_6a5b_nodes5m_win.txt")
