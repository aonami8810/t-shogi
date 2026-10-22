with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    text = f.read()

bad = chr(92) + 'elem' + chr(92)
good = chr(96) + 'elem' + chr(96)

text = text.replace(bad, good)

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(text)

print("Replaced successfully!")
