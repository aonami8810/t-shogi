# -*- coding: utf-8 -*-
import ast
from solve_both_nodes5m import solve_line

if __name__ == "__main__":
    with open("g2_ply39_9g9f_nodes5m_win.txt", "r", encoding="utf-8") as f:
        g2_moves = ast.literal_eval(f.read().strip())
    solve_line(g2_moves, False, "G2_39_9g9f_finish", "g2_ply39_9g9f_nodes5m_win.txt")
