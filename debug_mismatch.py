# -*- coding: utf-8 -*-
import ast

with open("g1_2i2h_result.txt", "r", encoding="utf-8") as f:
    line = ast.literal_eval(f.read().strip())

actual_moves = "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d P*2d R*2f 3i3h P*2h 7g3c+ 2a3c B*3g 2f2d 3g9a+ 2h2i+ 9a8a 2i2h".split()

pref = ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h']
expanded = pref + line[7:]

print(f"actual_moves len: {len(actual_moves)}")
print(f"expanded len: {len(expanded)}")

for i in range(len(actual_moves)):
    if actual_moves[i] != expanded[i]:
        print(f"Mismatch at index {i} (ply {i+1}): actual='{actual_moves[i]}' vs expanded='{expanded[i]}'")
        break
else:
    print("ALL 36 MOVES MATCH PERFECTLY!")
    print(f"Move 37 should be: '{expanded[36]}'")
