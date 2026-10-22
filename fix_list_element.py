with open('ShogiMorseCompiler.hs', 'r', encoding='utf-8') as f:
    c = f.read()

bad = '  morseFlowStep  :: Proxy ply -> Proxy from -> Proxy to -> (Integer, String, String)'
good = '  , "  morseFlowStep  :: Proxy ply -> Proxy from -> Proxy to -> (Integer, String, String)"'

c = c.replace(bad, good)
with open('ShogiMorseCompiler.hs', 'w', encoding='utf-8') as f:
    f.write(c)

print("Fixed list string element!")
