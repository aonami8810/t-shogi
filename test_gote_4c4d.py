from test_gote_4a3b import run_match_simulation

start_g2_4c4d = [
    "7g7f", "3c3d", "2g2f", "8c8d", "6i7h", "4c4d"
]
print("=== Testing Gote 6th move 4c4d (Morse = Gote) ===")
moves = run_match_simulation(start_g2_4c4d, gote_byo=2500, sente_byo=500, max_plies=75)
print("Result moves:", repr(moves))
