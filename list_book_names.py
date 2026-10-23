with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()
in_base = False
for l in lines:
    if 'baseBookMoves =' in l:
        in_base = True
    elif in_base and ('\"' in l) and ('Line' in l or 'p' in l):
        parts = l.split('\"')
        if len(parts) >= 2:
            print(parts[1])
    elif in_base and l.strip().startswith(']'):
        break
