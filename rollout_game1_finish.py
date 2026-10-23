from test_gote_4a3b import run_match_simulation

moves_80_g1 = ['7g7f', '8c8d', '2g2f', '4a3b', '6i7h', '3c3d', '2f2e', '8d8e', '2e2d', '2c2d', '2h2d', '5a4b', '2d3d', '8e8f', '8g8f', '8b8f', '9g9f', '1c1d', '5i5h', '7a7b', '3d2d', '4b5b', '2d2f', '2b8h+', '7i8h', 'B*4d', 'B*7g', '8f8h+', '7g8h', '4d2f', 'R*2d', 'P*8f', '2d2f', 'S*8g', '8h1a+', '8g7h+', '5h4h', '2a3c', '2f2a+', '7h6i', '3g3f', 'P*2h', '3i2h', 'R*2e', 'L*2f', '2e2a', '1a2a', '3c4e', '4h3h', 'R*7h', '3h2g', '6i5i', '4i3i', '7h5h+', 'R*1b', '5h4g', '2g1h', 'G*4b', '2f2c+', '4g3f', '2c3b', '3a3b', 'B*2d', '6a5a', '2a3b', '4b3b', 'S*2g', '3f3d', 'G*3c', '3d2d', '1b3b+', 'L*4b', '3c4c', '5b6b', '4c4b', '5a4b', '3b4b', 'G*5b', '4b4e', 'P*2f']

print("=== Rolling out Game 1 (Morse = Sente) to decisive mate ===")
final_g1 = run_match_simulation(moves_80_g1, gote_byo=500, sente_byo=2500, max_plies=105)
print("Complete Game 1 Moves:")
print(repr(final_g1))
