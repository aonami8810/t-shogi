from rollout_final_two import simulate_rollout

# Let's test Gote choosing 8d8e at ply 4:
start_kaku = ["2g2f", "8c8d", "7g7f", "8d8e"]
print("=== Testing Gote 8d8e branch (Morse = Gote) ===")
moves, scores = simulate_rollout(start_kaku, favored_player=2, max_plies=60)
print("Finished. Final moves:", moves)
