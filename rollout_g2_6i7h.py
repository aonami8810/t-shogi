from test_gote_4a3b import run_match_simulation

start_g2_6i7h = [
    "7g7f", "3c3d", "2g2f", "8c8d", "6i7h", "8d8e"
]
print("=== Rolling out Game 2 Branch: 5th ply 6i7h -> 8d8e (Morse = Gote) ===")
moves_g2 = run_match_simulation(start_g2_6i7h, gote_byo=2500, sente_byo=500, max_plies=80)
print("\nGame 2 6i7h Winning Line:")
print(repr(moves_g2))
