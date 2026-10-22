with open('DebugKakugawari.hs', 'r', encoding='utf-8') as f:
    c = f.read()
c = c.replace('putStrLn $ "Legal moves for Sente next:"', 'putStrLn $ "Hands: " ++ show (hands st)')
with open('DebugKakugawari.hs', 'w', encoding='utf-8') as f:
    f.write(c)
