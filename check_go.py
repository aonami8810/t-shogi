with open('c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    text = f.read()

idx = text.find('handleCommand _ ("go"')
if idx == -1:
    idx = text.find('("go"')
print(text[idx:idx+1500])
