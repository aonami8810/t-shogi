with open('CompiledMorseBook.hs', 'r', encoding='utf-8', errors='replace') as f:
    text = f.read()

import re
# runtimePaths のエントリを行ごとに抽出
moves = ["2g2f", "8c8d", "7g7f", "3c3d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d"]
target_prefix = '["' + '","'.join(moves) + '"'

matching = []
for line in text.splitlines():
    if target_prefix in line:
        # パス抽出
        start = line.find('["')
        end = line.rfind('"]')
        if start != -1 and end != -1:
            raw_path = line[start+2:end].split('","')
            if len(raw_path) > len(moves):
                matching.append(raw_path[len(moves)])

print(f"Total matching paths for 14th move: {len(matching)}")
from collections import Counter
print("Candidates for 14th move:", Counter(matching))
