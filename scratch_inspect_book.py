import re

with open('CompiledMorseBook.hs', 'r', encoding='cp932', errors='replace') as f:
    text = f.read()

# Look for runtime entries
entries = []
for line in text.splitlines():
    if line.strip().startswith('(') and '",' in line:
        entries.append(line.strip())

print(f"Found {len(entries)} entries like (moves, bestmove)")
# Find entries with long move sequences (> 30 ply)
long_entries = []
for e in text.split('('):
    if '),' in e or ')]' in e:
        # e might contain moves
        moves = re.findall(r'"([0-9a-i+*]{4,5})"', e)
        if len(moves) >= 30:
            long_entries.append(moves)

print(f"Found {len(long_entries)} positions with >= 30 plies!")
for i, moves in enumerate(long_entries[:5]):
    print(f"Sample {i+1} (length {len(moves)}): {' '.join(moves[:5])} ... {' '.join(moves[-5:])}")
