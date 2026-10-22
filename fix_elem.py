with open('c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    text = f.read()

target = "pt elem [Gold, Silver]"
replacement = "pt `elem` [Gold, Silver]"

assert target in text, "target not found"
text = text.replace(target, replacement)

with open('c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(text)

print("Replacement done via script file!")
