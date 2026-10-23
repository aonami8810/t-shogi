from test_gote_4a3b import run_match_simulation

# Game 1 branch: 8th ply 5a5b
start_g1_5a5b = [
    "7g7f", "8c8d", "2g2f", "3c3d", "2f2e", "8d8e", "6i7h", "5a5b", "2e2d"
]
print("=== Rolling out Game 1 Branch: 8th ply 5a5b -> 2e2d (Morse = Sente) ===")
moves_g1 = run_match_simulation(start_g1_5a5b, gote_byo=500, sente_byo=2500, max_plies=80)
print("\nGame 1 5a5b Winning Line:")
print(repr(moves_g1))
