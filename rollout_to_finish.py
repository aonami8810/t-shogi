from test_gote_4a3b import run_match_simulation

moves_75 = ['2g2f', '8c8d', '7g7f', '4a3b', '2f2e', '8d8e', '8h7g', '3c3d', '7i6h', '2b7g+', '6h7g', '3a2b', '3i3h', '2b3c', '3g3f', '7c7d', '3h3g', '7a6b', '6i7h', '6b7c', '3g4f', '1c1d', '5i5h', '9c9d', '1g1f', '9d9e', '2h2f', '7c6d', '4i3h', '5a5b', '2i3g', '8a7c', '3f3e', '3d3e', '4f3e', '8b8a', '2e2d', '3c2d', '3e2d', '2c2d', '2f2d', 'P*2c', '2d3d', '2a3c', '7f7e', '6d7e', 'S*2a', '3b4b', 'B*2b', '7c6e', '2b1a+', 'P*3f', '3d3f', 'B*2d', '7g6h', 'S*3e', '3f3e', '2d3e', 'P*3d', 'P*3f', '3d3c+', '3f3g+', '3c4b', '5b6b', '3h3g', 'N*4e', 'G*7c', '6b7c', '1a5e', '7c8c', '5e4e', 'R*3i', 'S*4h', '3i8i+', '7h7i']

print("=== Rolling out to decisive finish (Gote winning line) ===")
final_moves = run_match_simulation(moves_75, gote_byo=2500, sente_byo=500, max_plies=105)
print("Complete Game 2 Moves:")
print(repr(final_moves))
