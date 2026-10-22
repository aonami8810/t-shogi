with open('c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    text = f.read()

for i, line in enumerate(text.splitlines()):
    if 'bestmove' in line:
        print(f"{i+1}: {line}")
