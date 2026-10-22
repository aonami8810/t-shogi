with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

for i, l in enumerate(lines):
    if 'scoredMoves = sortBy' in l:
        lines[i] = l.replace('scoredMoves =', 'let scoredMoves =', 1)
        print(f"Fixed scoredMoves at line {i+1}")
    if 'scored = sortBy' in l:
        lines[i] = l.replace('scored =', 'let scored =', 1)
        print(f"Fixed scored at line {i+1}")

with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)
print("Saved!")
