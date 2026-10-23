with open('CompiledMorseBook.hs', 'r', encoding='utf-8', errors='replace') as f:
    text = f.read()

target = '["2g2f","8c8d","7g7f","3c3d","2f2e","8d8e","6i7h","8e8f","8g8f","8b8f","2e2d","2c2d","2h2d"'
print('Found prefix?:', target in text)

matches = [line for line in text.splitlines() if '2g2f' in line and '8c8d' in line and '7g7f' in line and '2h2d' in line]
print(f'Total matches: {len(matches)}')
for i, m in enumerate(matches[:5]):
    print(f'Match {i}: {m[:200]}')
