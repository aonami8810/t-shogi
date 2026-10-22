with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    text = f.read()

target = """                                 let (score, pv) = searchAlphaBeta weights d (-99999.0) 99999.0 state
                                 case pv of"""

replacement = """                                 let (score, pv) = searchAlphaBeta weights d (-99999.0) 99999.0 state
                                 putStrLn $ "info string debug d=" ++ show d ++ " pv_len=" ++ show (length pv) ++ " moves=" ++ unwords (map showMove pv)
                                 hFlush stdout
                                 case pv of"""

if target in text:
    text = text.replace(target, replacement, 1)
    with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
        f.write(text)
    print("Inserted debug print.")
else:
    print("Target not found!")
