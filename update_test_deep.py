with open('TestDeepLines.hs', 'r', encoding='utf-8') as f:
    c = f.read()
c = c.replace('"G*2b"', '"G*3b"')
with open('TestDeepLines.hs', 'w', encoding='utf-8') as f:
    f.write(c)
print("Updated G*2b to G*3b")
