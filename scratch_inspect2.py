# -*- coding: utf-8 -*-
import glob
import re

print("=== Checking goteSuperHighways in ShogiUsiMorse-v4.hs ===")
with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    text = f.read()

gote_matches = re.findall(r'goteSuperHighways_\d+\s*=\s*\[(.*?)\]', text)
for i in range(min(5, len(gote_matches))):
    moves = [x.strip().strip('"').strip("'") for x in gote_matches[i].split(',')]
    print(f"goteSuperHighways_{i} (first 8 moves):", moves[:8])

print("\n=== Searching for python files related to highway / conquest ===")
for p in glob.glob("*.py"):
    if "high" in p or "conquest" in p or "transposition" in p or "verify" in p:
        print(p)

print("\n=== Checking conquest_code_separated.txt ===")
try:
    with open('conquest_code_separated.txt', 'r', encoding='utf-8') as f:
        c = f.read()
    print("conquest_code_separated.txt length:", len(c))
    lines = c.splitlines()
    print("First 10 lines:")
    for l in lines[:10]:
        print(" ", l)
except Exception as e:
    print("Error:", e)

